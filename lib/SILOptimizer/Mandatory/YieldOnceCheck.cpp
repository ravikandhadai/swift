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
#include "swift/AST/DiagnosticsSIL.h"
#include "swift/SIL/BasicBlockUtils.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "llvm/ADT/DenseSet.h"
#include "swift/AST/Stmt.h"

using namespace swift;

namespace {

class YieldOnceCheck : public SILFunctionTransform {

  template <typename... T, typename... U>
  static InFlightDiagnostic diagnose(ASTContext &Context, SourceLoc loc,
                                     Diag<T...> diag, U &&... args) {
    return Context.Diags.diagnose(loc, diag, std::forward<U>(args)...);
  }

  /// A state that is associated with basic blocks to indicate whether a basic
  /// block is encountered before seeing a yield (BeforeYield) or after
  /// seeing a yield (AfterYield), or in both state, which is denoted by
  /// 'Conflict'. This is a semi-lattice that is used by the dataflow analysis
  /// that checks for yields. Merging BeforeYield and AfterYield states
  /// results in Conflict.
  enum YieldState { BeforeYield, AfterYield, Conflict };

  struct BBState {
    const YieldState yieldState;

  private:
    // The following are auxiliary information that is tracked for specific
    // states primarily for emitting diagnostics. They are set once when the
    // state is created and never changed later.

    // For AfterYield and Conflict states, this field tracks the yield
    // instruction that was seen while propagating the state.
    SILInstruction *yieldInst = nullptr;

    // For Conflict state, this field tracks the merge point where the conflict
    // is detected. Note that a conflict can happen if, at a point where two
    // branches meet, one branch yields while the other doesn't.
    SILInstruction *conflictInst = nullptr;

    // For Conflict State, this field tracks the basic block that reaches
    // the merge point without a yield. No that the other basic block can be
    // obtained from the `yieldInst` field.
    SILBasicBlock *noYieldBB = nullptr;

  public:

    BBState(YieldState state): yieldState(state) { }

    /// Creates an AfterYield state with the provided auxiliary information.
    static BBState getAfterYieldState(SILInstruction *yieldInstParam) {
      assert(yieldInstParam != nullptr);

      BBState afterYield(YieldState::AfterYield);
      afterYield.yieldInst = yieldInstParam;
      return afterYield;
    }

    static BBState getConflictState(SILInstruction *yieldInstParam,
                                    SILInstruction *conflictInstParam,
                                    SILBasicBlock *noYieldBBParam) {
      //assert(yieldState != Conflict);
      assert(yieldInstParam != nullptr);
      assert(conflictInstParam != nullptr);
      assert(noYieldBBParam != nullptr);

      BBState conflictState(YieldState::Conflict);
      conflictState.yieldInst = yieldInstParam;
      conflictState.conflictStmt = conflictInstParam;
      conflictState.noYieldBB = noYieldBBParam;

      return conflictState;
    }

    SILInstruction *getYieldInstruction() const {
      assert(yieldState == AfterYield || yieldState == Conflict);
      return yieldInst;
    }

    SILInstruction *getConflictInst() const {
      assert(yieldState == Conflict);
      return conflictInst;
    }

    SILBasicBlock *getNoYieldBasicBlock() const {
      assert(yieldState == Conflict);
      return noYieldBB;
    }
  };

  /// Given a basic block and an input state, compute the state after the
  /// basic block. If the next state cannot be computed as an error has been
  /// detected, generates a closure that can emit diagnostics.
  llvm::Optional<BBState> propagate(BBState inState, SILBasicBlock *bb) {
    auto *term = bb->getTerminator();

    if (isa<ReturnInst>(term)) {
      // If the input state is not AfterYield it implies that there is a path to
      // return before seeing a yield. This is an error.
      if (inState.yieldState != YieldState::AfterYield) {
        return None;
      }
      return inState; // Output state is same as input state otherwise.
    }

    if (isa<YieldInst>(term)) {
      // If the input state is not before yield, it implies that there are
      // multiple yield along this path, which is an error.
      if (inState.yieldState != YieldState::BeforeYield) {
        return None;
      }
      // If the current state is BeforeYield and if the basic block has a yield
      // the new state is AfterYield.
      return BBState::getAfterYieldState(term);
    }
    // In all other cases, the outout state is same as input state.
    return inState;
  }

  /// Merge two states and return the merged state. This performs the dataflow
  /// merge.
  llvm::Optional<BBState> merge(BBState oldState, BBState newState,
                                SILBasicBlock *currentBlock,
                                SILBasicBlock *mergePoint) {
    if (oldState.yieldState == newState.yieldState)
      return oldState;

    if (oldState.yieldState == Conflict) {
      // The oldState is already top of the lattice (which is an error state).
      return oldState;
    }

    // If the newState is the top of the lattice, return the new state.
    if (newState.yieldState == Conflict) {
      return newState;
    }

    // Here, one state is AfterYield and the other one is BeforeYield.
    // Therefore, create a new conflict state with the necessary auxiliary
    // information.

    // Find the sibling of the currentBlock.
    //SILBasicBlock *otherBB =

    SILInstruction *yieldInst;
    SILBasicBlock *noYieldBB;

    if (newState.yieldState == YieldState::AfterYield) {
      yieldInst = newState.getYieldInstruction();
      noYieldBB = nullptr; // make this not yield bb
    } else {
      yieldInst = oldState.getYieldInstruction();
      noYieldBB = nullptr; // make this no yield bb
    }

    return BBState::getConflictState(yieldInst, *(succBB->begin()), noYieldBB);
  }

  /// Return true if the given 'stmt' is a control-flow construct of the source
  /// language.
  //  bool isControlFlowStmt(Stmt *stmt) {
  //    switch (stmt->getKind()) {
  //      case StmtKind::If:
  //      case StmtKind::While:
  //      case StmtKind::ForEach:
  //      case StmtKind::RepeatWhile:
  //      case StmtKind::Switch:
  //      case StmtKind::Guard:
  //        return true;
  //      default:
  //        return false;
  //    }
  //  }

  /// Checks whether there is exactly one yield before a return in every path
  /// in the control-flow graph.
  /// Diagnostics are not reported for nodes unreachable from the entry, and
  /// also for nodes that may not reach the exit of the control-flow graph.
  void diagnoseYieldOnceUsage(SILFunction &fun) {
    // Do a traversal of the basic blocks starting from the entry node
    // in a breadth-first fashion. The traversal order is not important for
    // correctness, but it could change the errors diagnosed when there are
    // multiple errors. Breadth-first search could diagnose errors along
    // shorter paths.
    llvm::DenseMap<SILBasicBlock *, BBState> visitedBBs;
    SmallVector<SILBasicBlock *, 16> worklist;

    auto *entryBB = &(*fun.begin());
    visitedBBs.try_emplace(entryBB);
    worklist.push_back(entryBB);

    // Track the return instruction of the function, if it is reached
    // before a yield instruction. This is needed for emitting diagnostics
    // when there is no yield instruction along any path reaching a
    // return instruction.
    llvm::Optional<SILInstruction *> returnInst = None;
    ASTContext &astCtx = fun.getModule().getASTContext();

    while (!worklist.empty()) {
      SILBasicBlock *bb = worklist.pop_back_val();
      BBState state = visitedBBs[bb];

      auto nextStateOpt = propagate(state, bb);

      if (!nextStateOpt.hasValue()) {
        // We found an error and therefore return after presenting diagnostics.
      // Handle diagnostics
      // If the current state is conflict it implies that there is a path to
      // return before seeing a yield. Diagnose this error.
      // TODO: should we defer this and prioritize other errors.
//      if (state.yieldState == YieldState::Conflict) {
//        // diagnoseConflicts here.
//        diagnose(astCtx, term->getLoc().getSourceLoc(),
//                 diag::possible_return_before_yield);
//      }
//
//      // If the state is BeforeYield, it is an error. But, defer emitting
//      // diagnostics until we see a Conflict state or have visited all nodes
//      // in order to gather more information.
//      if (state.yieldState != YieldState::AfterYield) {
//        returnInst = term;
//      }
//      if (isa<YieldInst>(term)) {
//        // If the input state is not before yield, it implies that there are
//        // multiple yield along this path, which is an error.
//        if (inState.yieldState != YieldState::BeforeYield) {
//          diagnose(astCtx, term->getLoc().getSourceLoc(),
//                   diag::multiple_yields);
//          // Add a note that points to the previous yield.
//          diagnose(astCtx, state.getYieldInstruction()->getLoc().getSourceLoc(),
//                   diag::previous_yield);
//          return;
//        }
//      }
        return;
      }

      auto nextState = nextStateOpt.getValue();

      for (auto &succ : bb->getSuccessors()) {
        SILBasicBlock *succBB = succ.getBB();
        // Optimistically try to set the state of the successor as next state.
        auto insertResult = visitedBBs.try_emplace(succBB, nextState);

        // If the insertion was successful, it means we are seeing the successor
        // for the first time. Add the successor to the worklist.
        if (insertResult.second) {
          worklist.insert(worklist.begin(), succBB);
          continue;
        }

        // Here, the successor already has a state. Therefore, we have
        // to merge them and if we see new state after merging, we have to
        // propagate the merged state.
        const auto &succState = insertResult.first->second;
        auto mergedState = merge(succState, nextState, succBB);

        if (mergedState == succState)
          continue;

        insertResult.first->second = mergedState;
        worklist.insert(worklist.begin(), succBB);
          // Even though at this point we know there has to be an error as
          // there is an inconsistency between states coming along two
          // different paths, we cannot stop here as we do not
          // know for sure whether the error will result in multiple yields
          // or a return before a yield.
      }
    }

    if (returnInst.hasValue()) {
      // Here, we haven't seen a yield along any path that leads to this return.
      // Otherwise, the analysis must have propagated a conflict state to this
      // return instruction, which must have been diagnosed earlier.
      diagnose(astCtx, returnInst.getValue()->getLoc().getSourceLoc(),
               diag::return_before_yield);
      return;
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
