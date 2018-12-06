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

  private:
    // For AfterYield and Conflict states, track the yield instruction.
    llvm::Optional<SILInstruction *> yieldInst = None;
    // For Conflict state, track the first instruction that is found to be in
    // conflict state. This should be the first instruction of a join node in
    // the CFG.
    llvm::Optional<SILInstruction *> conflictInst = None;

  public:
    void updateToAfterYield(SILInstruction *yldInst) {
      assert(yieldState == BeforeYield);
      assert(yldInst != nullptr);

      yieldState = AfterYield;
      yieldInst = yldInst;
    }

    void updateToConflict(SILInstruction *yldInst, SILInstruction *confInst) {
      assert(yieldState != Conflict);
      assert(yldInst != nullptr && confInst != nullptr);

      yieldState = Conflict;
      yieldInst = yldInst;
      conflictInst = confInst;
    }

    SILInstruction *getYieldInstruction() const {
      assert(yieldState == AfterYield || yieldState == Conflict);

      return yieldInst.getValue();
    }

    SILInstruction *getConflictInstruction() const {
      assert(yieldState == Conflict);

      return conflictInst.getValue();
    }
  };

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
          // Add a note to the instruction where the conflict first appeared.
          diagnose(astCtx,
                   state.getConflictInstruction()->getLoc().getSourceLoc(),
                   diag::conflicting_join);
          return;
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

        // Optimistically try to set our current state as the state
        // of the successor.
        auto insertResult = visitedBBs.try_emplace(succBB, state);

        // If the insertion was successful, it means we are seeing the successor
        // for the first time. Propogate the state and add the successor to the
        // worklist.
        if (insertResult.second) {
          worklist.insert(worklist.begin(), succBB);

          // When the successor follows a 'yield', update the successor state.
          if (isa<YieldInst>(term)) {
            insertResult.first->second.updateToAfterYield(term);
          }
          continue;
        }

        const auto &succState = insertResult.first->second;
        if (succState.yieldState == Conflict) {
          // The successor is already in the error state, so we need
          // not propagate anything. Diagnostics will be reported, where
          // appropriate, when the successor is removed from the worklist.
          continue;
        }

        // If the successor state is not a conflict but the current state is,
        // propagate the conflict down to the successor.
        // (This is needed to identify the conflicting yields and
        // present good diagnostics.)
        if (state.yieldState == Conflict) {
          worklist.insert(worklist.begin(), succBB);
          insertResult.first->second = state;
          continue;
        }

        // Here, neither 'state' nor 'succState' is equal to 'Conflict'.

        if (state.yieldState != succState.yieldState) {
          // We have found that the successor can appear before and
          // also after a yield. Therefore, set the state as a conflict and
          // propagate it to its successors to emit diagnostics.
          // (Note that the successor must be a join node in the control graph
          // for this scenario to happen.)
          auto *yieldInst = (state.yieldState == YieldState::AfterYield)
                                ? state.getYieldInstruction()
                                : succState.getYieldInstruction();
          insertResult.first->second.updateToConflict(yieldInst,
                                                      &*(succBB->begin()));
          worklist.insert(worklist.begin(), succBB);

          continue;
          // Even though at this point we know there has to be an error as
          // there is an inconsistent state, we cannot stop here as we do not
          // know for sure whether the error will result in multiple yields
          // or a return before a yield.
        }
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
