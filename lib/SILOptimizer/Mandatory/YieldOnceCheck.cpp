//===------ YieldOnceCheck.cpp - Check usage of yields in accessors  ------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "yield-once-check"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/DiagnosticsSIL.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Stmt.h"
#include "swift/SIL/BasicBlockUtils.h"
#include "swift/SIL/Dominance.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "llvm/ADT/DenseSet.h"
#include "swift/SIL/CFG.h"
#include "llvm/ADT/BreadthFirstIterator.h"

using namespace swift;

namespace {

class YieldOnceCheck : public SILFunctionTransform {

  template <typename... T, typename... U>
  static InFlightDiagnostic diagnose(ASTContext &Context, SourceLoc loc,
                                     Diag<T...> diag, U &&... args) {
    return Context.Diags.diagnose(loc, diag, std::forward<U>(args)...);
  }

  /// Data-flow analysis state that is associated with basic blocks.
  struct BBState {

    /// Indicates whether a basic block is encountered before seeing a yield
    /// (BeforeYield) or after seeing a yield (AfterYield), or in both states
    /// (Conflict). This enum is a semi-lattice where Conflict is the top and
    /// the merge of BeforeYield and AfterYield states is Conflict.
    enum { BeforeYield, AfterYield, Conflict } yieldState = BeforeYield;

  private:
    // The following state is maintained for emitting diagnostics.

    // For AfterYield and Conflict states, this field records the yield
    // instruction that was seen while propagating the state.
    SILInstruction *yieldInst = nullptr;
    SILBasicBlock *yieldingPred = nullptr;
    SILBasicBlock *nonYieldingPred = nullptr;

  public:
    static BBState getAfterYieldState(SILInstruction *yieldI) {
      assert(yieldI);

      BBState afterYield;
      afterYield.yieldState = AfterYield;
      afterYield.yieldInst = yieldI;
      return afterYield;
    }

    static BBState getConflictState(SILInstruction *yieldI) {
      assert(yieldI);

      BBState conflictState;
      conflictState.yieldState = Conflict;
      conflictState.yieldInst = yieldI;

      return conflictState;
    }

    SILInstruction *getYieldInstruction() const {
      assert(yieldState == AfterYield || yieldState == Conflict);
      return yieldInst;
    }

    void setConflictDiagnosticState(SILBasicBlock *yieldPred,
                                    SILBasicBlock *noyieldPred) {
      assert (yieldPred && noyieldPred);
      yieldingPred = yieldPred;
      nonYieldingPred = noyieldPred;
    }

    SILBasicBlock *getYieldingPred() {
      assert(yieldState == Conflict);
      return yieldingPred;
    }

    SILBasicBlock *getNonYieldingPred() {
      assert(yieldState == Conflict);
      return nonYieldingPred;
    }
  };

  /// A structure that records an error found during the analysis along with
  /// some context information that will be used by diagnostics.
  struct YieldError {
    // The kind of error.
    enum Kind { MultipleYield, ReturnBeforeYield, ReturnOnConflict } errorKind;
    // The termination instruction where the error should be reported.
    SILInstruction *termInst;
    // The input state when the error is encountered.
    BBState inState;

  private:
    YieldError(Kind kind, SILInstruction *term, BBState state)
        : errorKind(kind), termInst(term), inState(state) {}

  public:
    static YieldError getMultipleYieldError(YieldInst *yield, BBState state) {
      assert (state.yieldState != BBState::BeforeYield);
      return YieldError(MultipleYield, yield, state);
    }

    static YieldError getReturnBeforeYieldError(ReturnInst *returnI,
                                                BBState state) {
      assert (state.yieldState == BBState::BeforeYield);
      return YieldError(ReturnBeforeYield, returnI, state);
    }

    static YieldError getReturnOnConflict(ReturnInst *returnI, BBState state) {
      assert (state.yieldState == BBState::Conflict);
      return YieldError(ReturnOnConflict, returnI, state);
    }
  };

  /// Transfer function of the data-flow analysis.
  ///
  /// \param bb Basic block that should be processed
  /// \param inState BBState at the start of the basic block
  /// \param error out parameter that will contain information about
  /// an error that is detected.
  /// \return the state at the exit of the basic block if it can be computed
  /// and None otherwise.
  llvm::Optional<BBState>
  transferStateThroughBasicBlock(SILBasicBlock *bb, BBState inState,
                                 llvm::Optional<YieldError> &error) {
    auto *term = bb->getTerminator();

    if (auto *returnInst = dyn_cast<ReturnInst>(term)) {
      if (inState.yieldState == BBState::BeforeYield) {
        error = YieldError::getReturnBeforeYieldError(returnInst, inState);
        return None;
      }

      if (inState.yieldState == BBState::Conflict) {
        error = YieldError::getReturnOnConflict(returnInst, inState);
        return None;
      }
      return inState;
    }

    if (auto *yieldInst = dyn_cast<YieldInst>(term)) {
      if (inState.yieldState != BBState::BeforeYield) {
        error = YieldError::getMultipleYieldError(yieldInst, inState);
        return None;
      }

      // If the current state is BeforeYield and if the basic block ends in a
      // yield the new state is AfterYield.
      return inState.getAfterYieldState(term);
    }

    // We cannot have throws within generalized accessors.
    assert (!isa<ThrowInst>(term));

    return inState;
  }

  /// Merge operation of the data-flow analysis.
  ///
  /// \param mergeBlock the basic block that is reached with two states
  /// \param oldState the previous state at the entry of the basic block
  /// \param newState the current state at the entry of the basic block
  /// \return the new state obtained by merging the oldState with the newState
  BBState merge(SILBasicBlock *mergeBlock, BBState oldState, BBState newState,
                SILBasicBlock *recentPrevBlock,
                llvm::DenseMap<SILBasicBlock *, BBState> &bbToStateMap) {
    if (oldState.yieldState == BBState::Conflict) {
      return oldState;
    }

    if (newState.yieldState == BBState::Conflict) {
      return newState;
    }

    if (oldState.yieldState == newState.yieldState) {
      return oldState;
    }

    // Here, one state is AfterYield and the other one is BeforeYield.
    // Merging them will result in Conflict.
    SILInstruction *yieldInst = (newState.yieldState == BBState::AfterYield)
                                    ? newState.getYieldInstruction()
                                    : oldState.getYieldInstruction();

    auto conflictState = BBState::getConflictState(yieldInst);

    // find a predecessor of 'succBB' that is not 'recentPrevBlock'
    // that is already visted from the bbToStateMap.
    SILBasicBlock *prevSeenBB = nullptr;
    for (auto predBB : mergeBlock->getPredecessorBlocks()) {
      if (predBB != recentPrevBlock && bbToStateMap.count(predBB)) {
        prevSeenBB = predBB;
        break;
      }
    }
    assert (prevSeenBB);

    if (oldState.yieldState == BBState::BeforeYield) {
      conflictState.setConflictDiagnosticState(recentPrevBlock, prevSeenBB);
    } else {
      conflictState.setConflictDiagnosticState(prevSeenBB, recentPrevBlock);
    }
    return conflictState;
  }

  /// Perform a data-flow analysis to check whether there is exactly one
  /// yield before a return in every path in the control-flow graph.
  /// Diagnostics are not reported for nodes unreachable from the entry, and
  /// also for nodes that may not reach the exit of the control-flow graph.
  void diagnoseYieldOnceUsage(SILFunction &fun) {
    llvm::DenseMap<SILBasicBlock *, BBState> bbToStateMap;

    SmallVector<SILBasicBlock *, 16> worklist;

    auto *entryBB = &(*fun.begin());
    bbToStateMap.try_emplace(entryBB);
    worklist.push_back(entryBB);

    // ReturnBeforeYield errors, which denote that no paths yield before
    // returning, are not diagnosed until the analysis completes, in order to
    // distinguish them from ReturnOnConflict errors, which happen when some
    // paths yield and some don't.
    llvm::Optional<YieldError> returnBeforeYieldError = None;

    // The algorithm uses a worklist to propagate the state through basic
    // blocks until a fix point. Since the state lattice has height one, each
    // basic block will be visited at most twice, and at most once if there are
    // no Conflicts (which are errors). The basic blocks are added to the
    // worklist in a breadth-first fashion. The order of visiting basic blocks
    // is not important for correctness, but it could change the errors
    // diagnosed when there are multiple errors. Breadth-first search diagnoses
    // errors along shorter paths.
    while (!worklist.empty()) {
      SILBasicBlock *bb = worklist.pop_back_val();
      BBState state = bbToStateMap[bb];

      llvm::Optional<YieldError> errorResult = None;
      auto resultState = transferStateThroughBasicBlock(bb, state, errorResult);

      if (!resultState.hasValue()) {
        auto error = errorResult.getValue();

        // ReturnBeforeYield errors will not be reported until the analysis
        // completes. So record it and continue.
        if (error.errorKind == YieldError::ReturnBeforeYield) {
          if (!returnBeforeYieldError.hasValue()) {
            returnBeforeYieldError = error;
          }
          continue;
        }

        emitDiagnostics(error, fun);
        return;
      }

      auto nextState = resultState.getValue();

      for (auto &succ : bb->getSuccessors()) {
        SILBasicBlock *succBB = succ.getBB();

        // Optimistically try to set the state of the successor as next state.
        auto insertResult = bbToStateMap.try_emplace(succBB, nextState);

        // If the insertion was successful, it means we are seeing the successor
        // for the first time. Add the successor to the worklist.
        if (insertResult.second) {
          worklist.insert(worklist.begin(), succBB);
          continue;
        }

        // Here, the successor already has a state. Merge the states and
        // propagate it if it is different from the old state.
        const auto &oldState = insertResult.first->second;
        auto mergedState = merge(succBB, oldState, nextState, bb, bbToStateMap);

        if (mergedState.yieldState == oldState.yieldState)
          continue;

        // Even though at this point we know there has to be an error as
        // there is an inconsistency between states coming along two
        // different paths, we cannot stop here as we do not
        // know for sure whether the error will result in multiple yields
        // or a return before a yield.
        insertResult.first->second = mergedState;
        worklist.insert(worklist.begin(), succBB);
      }
    }

    if (returnBeforeYieldError.hasValue()) {
      emitDiagnostics(returnBeforeYieldError.getValue(), fun);
    }
  }

  void emitDiagnostics(YieldError &error, SILFunction &fun) {
    ASTContext &astCtx = fun.getModule().getASTContext();

    switch (error.errorKind) {
    case YieldError::ReturnBeforeYield: {
      diagnose(astCtx, error.termInst->getLoc().getSourceLoc(),
               diag::return_before_yield);
        return;
    }
    case YieldError::MultipleYield: {
      diagnose(astCtx, error.termInst->getLoc().getSourceLoc(),
               diag::multiple_yields);

      // Add a note that points to the previous yield.
      diagnose(astCtx,
               error.inState.getYieldInstruction()->getLoc().getSourceLoc(),
               diag::previous_yield);
      return;
    }
    case YieldError::ReturnOnConflict: {
      // Emit an error on the return statement.
      diagnose(astCtx, error.termInst->getLoc().getSourceLoc(),
               diag::possible_return_before_yield);

      // Find the first point where yieldPred and noYieldPred meet when
      // traversing the CFG in the reverse order. This is the branch that
      // results in this conflict. Then, analyze the branch and produce
      // diagnostics.
      auto &errorState = error.inState;

//      llvm::errs() << "Yielding bb: bb" << errorState.getYieldingPred()->getDebugID()  << "\n";
//      llvm::errs() << "Non-yielding bb: bb" << errorState.getNonYieldingPred()->getDebugID()  << "\n";

      // Find all transitive predecessors of 'yieldPred' Basic block.
      SmallPtrSet<SILBasicBlock *, 8> predcessorsOfYieldPred;
      auto yieldPredRange =
        llvm::breadth_first<llvm::Inverse<SILBasicBlock *>>(
                                                  errorState.getYieldingPred());
      for (auto *predBB : yieldPredRange) {
        predcessorsOfYieldPred.insert(predBB);
      }

      // Find the first predecessor of 'noYieldPred' basic block that is also
      // a predecessor of 'yieldPred'.
      SILBasicBlock *branchBB = nullptr;
      SmallPtrSet<SILBasicBlock *, 8> predcessorsOfNoYieldPred;

      auto noyieldPredRange =
        llvm::breadth_first<llvm::Inverse<SILBasicBlock *>>(
                                              errorState.getNonYieldingPred());
      for (auto *pred : noyieldPredRange) {
        if (predcessorsOfYieldPred.count(pred)) {
          branchBB = pred;
          break;
        }
        predcessorsOfNoYieldPred.insert(pred);
      }
      assert(branchBB);
      // Extract the branching instruction from the branchBB and the target
      // that does not yield.
      auto *branchInst = branchBB->getTerminator();

      // Find the target of  branchInst that doesn't yield.
      SILBasicBlock *noYieldTarget = nullptr;
      for (auto *succ : branchBB->getSuccessorBlocks()) {
        if (predcessorsOfNoYieldPred.count(succ)) {
          noYieldTarget = succ;
          break;
        }
      }
      assert (noYieldTarget);

//      llvm::errs() << "Branch bb: bb" << branchBB->getDebugID()  << "\n";
//      llvm::errs() << "noYieldTarget bb: bb" << noYieldTarget->getDebugID()  << "\n";

      // 'branchInst' cannot be an unconditionally branching instruction or
      // an exit instruction or a yield instruction.
      assert(!isa<BranchInst>(branchInst) && !isa<ReturnInst>(branchInst) &&
             !isa<YieldInst>(branchInst) && !isa<ThrowInst>(branchInst));

      // Handle each of the branching case and report diagnostics.
      if (auto *condbr = dyn_cast<CondBranchInst>(branchInst)) {
        auto diag = (condbr->getTrueBB() == noYieldTarget)
            ? diag::true_branch_doesnt_yield : diag::false_branch_doesnt_yield;
        diagnose(astCtx, condbr->getLoc().getSourceLoc(), diag);
        return;
      }

      // FIXME: Handle optionals specially as they can be switched in 'if's a
      // 'guard's through let patterns.
      if (auto *switchEnum = dyn_cast<SwitchEnumInst>(branchInst)) {
        if (noYieldTarget->begin() != noYieldTarget->end()) {
          diagnose(astCtx, noYieldTarget->begin()->getLoc().getSourceLoc(),
                   diag::switch_case_doesnt_yield);
          return;
        }
        diagnose(astCtx, switchEnum->getLoc().getSourceLoc(),
                 diag::some_case_doesnt_yield);
        return;
      }

      if (auto *tryApply = dyn_cast<TryApplyInst>(branchInst)) {
        auto diag = (tryApply->getErrorBB() == noYieldTarget)
            ? diag::error_branch_doesnt_yield
            : diag::try_fallthrough_doesnt_yield;
        diagnose(astCtx, tryApply->getLoc().getSourceLoc(), diag);
        return;
      }

      // The fall back is to just point to one yield instruction found.
      auto *yieldInst = error.inState.getYieldInstruction();
      diagnose(astCtx, yieldInst->getLoc().getSourceLoc(), diag::one_yield);
    }
    }
  }

  /// The entry point to the transformation.
  void run() override {
    auto *fun = getFunction();

    if (fun->getLoweredFunctionType()->getCoroutineKind() !=
        SILCoroutineKind::YieldOnce)
      return;

    diagnoseYieldOnceUsage(*fun);
  }
};

} // end anonymous namespace

SILTransform *swift::createYieldOnceCheck() {
  return new YieldOnceCheck();
}
