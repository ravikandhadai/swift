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

  /// A state that is associated with basic blocks to record whether a basic
  /// block appears before a yield or after a yield, or could be in either
  /// state, which is denoted by 'Conflict'.
  enum YieldState { BeforeYield, AfterYield, Conflict };

  struct BBState {
    YieldState yieldState = BeforeYield;
    // Track the latest branch statement seen during the propagation of the
    // state. A branch AST node is a control-flow construct such as if-else
    // statement that results in branches at SIL. This information is used to
    // produce better diagnostics.
    Stmt *branchStmt = nullptr;

  private:
    // For AfterYield and Conflict states, track the yield instruction.
    SILInstruction *yieldInst = nullptr;

    // For Conflict state, track the branch statement that results in a
    // conflict, if any. Note that a conflict can happen if one branch yields
    // while the other doesn't. Since branches at SIL level does not have a
    // one-to-one correspondence with the control-flow language constructs
    // (e.g. branches could be introduced in SIL to merge multiple return
    // paths), this information is not guaranteed to be always available when
    // there is a conflict. Therefore, as a fallback we also record the
    // SIL location of the where the conflict happened.
    Stmt *conflictStmt = nullptr;
    llvm::Optional<SILLocation> conflictLoc = None;

  public:
    void updateToAfterYield(SILInstruction *yieldInstParam) {
      assert(yieldState == BeforeYield);
      assert(yieldInstParam != nullptr);

      yieldState = AfterYield;
      yieldInst = yieldInstParam;
    }

    void updateToConflict(SILInstruction *yieldInstParam,
                          SILLocation loc,
                          Stmt *conflictStmtParam) {
      assert(yieldState != Conflict);
      assert(yieldInstParam != nullptr);

      yieldState = Conflict;
      yieldInst = yieldInstParam;
      conflictLoc = loc;
      conflictStmt = conflictStmtParam;
    }

    SILInstruction *getYieldInstruction() const {
      assert(yieldState == AfterYield || yieldState == Conflict);

      return yieldInst;
    }

    Stmt *getConflictStmt() const {
      assert(yieldState == Conflict);

      return conflictStmt;
    }

    SILLocation getConflictLoc() const {
      assert(yieldState == Conflict);

      return conflictLoc.getValue();
    }
  };

  /// Return true if the given 'stmt' is a control-flow construct of the source
  /// language.
  bool isControlFlowStmt(Stmt *stmt) {
    switch (stmt->getKind()) {
      case StmtKind::If:
      case StmtKind::While:
      case StmtKind::ForEach:
      case StmtKind::RepeatWhile:
      case StmtKind::Switch:
      case StmtKind::Guard:
        return true;
      default:
        return false;
    }
  }

  /// This function computes the state after a terminal SIL instruction 'term',
  /// given the current state 'state'. This is like a single instruction
  /// transfer function.
  BBState propagate(BBState state, SILInstruction *term) {
    auto newState = state;
    // If we follow a 'yield', set the yield state to AfterYield.
    if (isa<YieldInst>(term)) {
      newState.updateToAfterYield(term);
    }

    // If the source statement of term is a control-flow statement,
    // set the control-flow info of the new state.
    auto *branchAST = term->getLoc().getAsASTNode<Stmt>();
    if (branchAST && isControlFlowStmt(branchAST)) {
      llvm::errs() << "Found the control-flow statment of the branch inst: " << *term << "\n";
      branchAST->dump();
      llvm::errs() << "\n";
      newState.branchStmt = branchAST;
    }
  }

  /// Merge two states and return the merged state if it is different from
  /// the old state. This implements the join operation of the lattice.
  llvm::Optional<BBState> merge(BBState oldState, BBState newState,
                                SILLocation mergeLoc) {
    if (oldState.yieldState == Conflict) {
      // The oldState is already top of the lattice (which is an error state).
      // So the merged state is same as the old state.
      return None;
    }

    // If the old state is not a conflict but the new state is,
    // merged state should be the new state.
    if (newState.yieldState == Conflict) {
      return newState;
    }

    if (newState.yieldState == oldState.yieldState)
      return None;

    // Here, the new and old state are not equal and therefore we have
    // widen them to top i.e., conflict, and set the auxiliary
    // info needed for the conflict state.
    // (a) Compute the yield instruction.
    auto *yieldInst = (newState.yieldState == YieldState::AfterYield)
                        ? newState.getYieldInstruction()
                        : oldState.getYieldInstruction();

    // (b) Compute the control-flow instruction that results in the
    // conflict, if possible.
    Stmt *conflictStmt = oldState.branchStmt;
    if (conflictStmt) {
      if (newState.branchStmt) {
        // TODO: Choose the branch statement that subsumes the other.
        llvm::errs() << "Found branch statements along both branches!!\n";
        assert(false);
      }
    }

    if (!conflictStmt) {
      conflictStmt = newState.branchStmt;
    }

    BBState mergedState;
    mergedState.updateToConflict(yieldInst, mergeLoc, conflictStmt);
    return mergedState;
  }

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

    // Track that last return instruction seen by the analysis, if any, before
    // encountering a yield.
    // This is needed for emitting diagnostics when there is no yield
    // instruction along any path reaching a return instruction.
    llvm::Optional<SILInstruction *> returnInst = None;
    ASTContext &astCtx = fun.getModule().getASTContext();

    while (!worklist.empty()) {
      SILBasicBlock *bb = worklist.pop_back_val();
      BBState state = visitedBBs[bb];

      auto *term = bb->getTerminator();
      if (isa<ReturnInst>(term)) {
        // A conflict state implies that there is a path to return before
        // seeing a yield.
        if (state.yieldState == YieldState::Conflict) {
          diagnose(astCtx, term->getLoc().getSourceLoc(),
                   diag::possible_return_before_yield);

          // If a branch statement that results in a conflict is available
          // present that information. Otherwise, emit a default message.
          auto *conflictStmt = state.getConflictStmt();
          if (!conflictStmt) {
            llvm::errs() << "Didn't find any conflict inst. \n";
            diagnose(astCtx, state.getConflictLoc().getSourceLoc(),
                     diag::conflicting_join);
            return;
          }

          switch (conflictStmt->getKind()) {
            case StmtKind::If:
              llvm::errs() << "Found If construct. \n";
              diagnose(astCtx, state.getConflictLoc().getSourceLoc(),
                       diag::if_conflict);
              return;

            case StmtKind::While:
            case StmtKind::ForEach:
            case StmtKind::RepeatWhile:
              llvm::errs() << "Found while construct. \n";
              // Note that in all these cases the only possibility is that
              // the body has a yield but the fall through case does not.
              // Otherwise, the error will be detected by other cases.
              diagnose(astCtx, state.getConflictLoc().getSourceLoc(),
                       diag::loop_conflict);
              return;
            case StmtKind::Brace:
              llvm::errs() << "Found brace statement \n";
            default:
              diagnose(astCtx, state.getConflictLoc().getSourceLoc(),
                       diag::conflicting_join);
              return;
          }
        }
        // If the state is BeforeYield, it is an error. But, defer emitting
        // diagnostics until we see a Conflict state or have visited all nodes
        // in order to gather more information.
        if (state.yieldState != YieldState::AfterYield) {
          returnInst = term;
        }
        continue;
      }

      // Check whether there are multiple yields.
      if (isa<YieldInst>(term)) {
        if (state.yieldState != YieldState::BeforeYield) {
          diagnose(astCtx, term->getLoc().getSourceLoc(),
                   diag::multiple_yields);
          // Add a note that points to the previous yield.
          diagnose(astCtx, state.getYieldInstruction()->getLoc().getSourceLoc(),
                   diag::previous_yield);
          return;
        }
      }

      for (auto &succ : term->getSuccessors()) {
        SILBasicBlock *succBB = succ.getBB();

        auto nextState = propagate(state, term);
        // Optimistically try to set the state of the successor as next state.
        auto insertResult = visitedBBs.try_emplace(succBB, nextState);

        // If the insertion was successful, it means we are seeing the successor
        // for the first time. Propogate the state and add the successor to the
        // worklist.
        if (insertResult.second) {
          worklist.insert(worklist.begin(), succBB);
          continue;
        }

        // Here, the successor already has a state. Therefore, we have
        // to merge them and, if necessary, have to propagate the merged state.
        const auto &succState = insertResult.first->second;
        auto newStateOpt = merge(succState, nextState,
                                (*(succBB->begin())).getLoc());

        if (!newStateOpt.hasValue())
          continue;

        insertResult.first->second = newStateOpt.getValue();
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
