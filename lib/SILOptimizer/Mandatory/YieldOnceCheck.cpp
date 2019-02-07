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
#include "swift/AST/ASTWalker.h"
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
  /// seeing a yield (AfterYield), or in both states, which is denoted by
  /// Conflict. This enum is a semi-lattice where Conflict is the top and
  /// the merge of BeforeYield and AfterYield states results in Conflict.
  enum YieldState { BeforeYield, AfterYield, Conflict };

  // TODO: try to make this const and use move instead of copy
  struct BBState {
    YieldState yieldState = BeforeYield;

  private:
    // For AfterYield and Conflict states, this field tracks the yield
    // instruction that was seen while propagating the state.
    SILInstruction *yieldInst = nullptr;

    // For Conflict state, this field tracks the merge point where the conflict
    // is detected. Note that a conflict can happen if, at a block where two
    // branches meet, one branch yields and the other doesn't.
    SILBasicBlock *conflictBB = nullptr;

    BBState(YieldState state): yieldState(state) { }

  public:

    BBState() { }

    /// Creates an AfterYield state with the provided auxiliary information.
    static BBState getAfterYieldState(SILInstruction *yieldInstParam) {
      assert(yieldInstParam != nullptr);

      BBState afterYield(YieldState::AfterYield);
      afterYield.yieldInst = yieldInstParam;
      return afterYield;
    }

    static BBState getConflictState(SILInstruction *yieldInstParam,
                                    SILBasicBlock *conflictBBParam) {
      assert(yieldInstParam != nullptr);
      assert(conflictBBParam != nullptr);

      BBState conflictState(YieldState::Conflict);
      conflictState.yieldInst = yieldInstParam;
      conflictState.conflictBB = conflictBBParam;
      //conflictState.noYieldBB = noYieldBBParam;

      return conflictState;
    }

    SILInstruction *getYieldInstruction() const {
      assert(yieldState == AfterYield || yieldState == Conflict);
      return yieldInst;
    }

    SILBasicBlock *getConflictBB() const {
      assert(yieldState == Conflict);
      return conflictBB;
    }
  };

  /// A structure that captures enough information about an error so that
  /// diagnositcs can be emitted for it later. This stores the kind of error,
  /// the terminal instruction where the error should be diagnosed and
  /// the input BB state at the point where the error should be diagnosed.
  struct DiagnosticsInfo {
    enum ErrorKind {
      MultipleYield,
      ReturnBeforeYield,
      ReturnOnConflict
    } errKind;
    BBState inState;
    SILInstruction * termInst;

    DiagnosticsInfo(BBState stateParam, ErrorKind kindParam,
                    SILInstruction *termInstParam) :
        errKind(kindParam), inState(stateParam), termInst(termInstParam) {
      assert(termInstParam != nullptr);

      // The following are some sanity checks that ensure that the parameters
      // do represent error scenarios.
      assert(kindParam != MultipleYield ||
             (isa<YieldInst>(termInstParam) &&
              stateParam.yieldState != BeforeYield));

      assert(kindParam != ReturnBeforeYield ||
             (isa<ReturnInst>(termInst) && inState.yieldState == BeforeYield));

      assert(kindParam != ReturnOnConflict ||
             (isa<ReturnInst>(termInstParam) &&
              stateParam.yieldState == Conflict));
    }
  };

  /// Given a basic block and an input state, compute the state after the
  /// basic block. If the next state cannot be computed, return None and
  /// set the diagnostics information. This implements the single-step transfer
  /// function of the dataflow analysis.
  llvm::Optional<BBState> propagate(BBState inState, SILBasicBlock *bb,
                                    llvm::Optional<DiagnosticsInfo> &diagInfo) {
    auto *term = bb->getTerminator();

    if (isa<ReturnInst>(term)) {
      // If the input state is not AfterYield it implies that there is a path
      // to the return instruction before a yield. This is an error.
      if (inState.yieldState == YieldState::BeforeYield) {
        diagInfo = DiagnosticsInfo(inState, DiagnosticsInfo::ReturnBeforeYield,
                                   term);
        return None;
      }

      if (inState.yieldState == YieldState::Conflict) {
        diagInfo = DiagnosticsInfo(inState, DiagnosticsInfo::ReturnOnConflict,
                                   term);
        return None;
      }
      return inState;
    }

    if (isa<YieldInst>(term)) {
      // If the input state is not BeforeYield, it implies that there are
      // multiple yields along this path, which is an error.
      if (inState.yieldState != YieldState::BeforeYield) {
        diagInfo = DiagnosticsInfo(inState, DiagnosticsInfo::MultipleYield,
                                   term);
        return None;
      }
      // If the current state is BeforeYield and if the basic block ends in a
      // yield the new state is AfterYield.
      return BBState::getAfterYieldState(term);
    }
    return inState;
  }

  /// Merge oldState with newState. This performs the dataflow merge operation.
  /// This may have to create a new Conflict state, in which case it uses
  /// the mergeBlock which is the merge point where conflict is detected.
  BBState merge(BBState oldState, BBState newState, SILBasicBlock *mergeBlock) {
    if (oldState.yieldState == newState.yieldState)
      return oldState;

    // If the newState or oldState is already top of the lattice, return it.
    if (oldState.yieldState == Conflict) {
      return oldState;
    }

    if (newState.yieldState == Conflict) {
      return newState;
    }

    // Here, one state is AfterYield and the other one is BeforeYield.
    // Therefore, create a new conflict state with the necessary auxiliary
    // information.
    SILInstruction *yieldInst = (newState.yieldState == YieldState::AfterYield)
                                  ? newState.getYieldInstruction()
                                  : oldState.getYieldInstruction();

    return BBState::getConflictState(yieldInst, mergeBlock);
  }

  /// Checks whether there is exactly one yield before a return in every path
  /// in the control-flow graph.
  /// Diagnostics are not reported for nodes unreachable from the entry, and
  /// also for nodes that may not reach the exit of the control-flow graph.
  void diagnoseYieldOnceUsage(SILFunction &fun) {
    llvm::DenseMap<SILBasicBlock *, BBState> bbStateMap;
    SmallVector<SILBasicBlock *, 16> worklist;

    auto *entryBB = &(*fun.begin());
    bbStateMap.try_emplace(entryBB);
    worklist.push_back(entryBB);

    // Track return-before-yield error.
    // They happen only when there is no yield in along any path in the
    // function. This information can only be obtained with certainty once
    // the analysis completes. Therefore, this error is diagnosed only after
    // the analysis completes. There are other errors which could be emitted
    // on the fly as they are discovered.
    llvm::Optional<DiagnosticsInfo> returnBeforeYieldError = None;

    // The algorithm uses a worklist to propagate the state through basic
    // blocks until a fix point. Since the state lattice has height one, each
    // basic block will be visited at most twice, and at most once if there are
    // no Conflicts (which are errors). The basic blocks are added to the
    // worklist  in a breadth-first fashion. The order of visiting basic blocks
    // is not important for correctness, but it could change the errors
    // diagnosed when there are multiple errors.
    // Breadth-first search diagnoses errors along shorter paths.
    while (!worklist.empty()) {
      SILBasicBlock *bb = worklist.pop_back_val();
      BBState state = bbStateMap[bb];

      llvm::Optional<DiagnosticsInfo> diagInfoOpt = None;
      auto nextStateOpt = propagate(state, bb, diagInfoOpt);

      // Diagnose errors if any.
      if (!nextStateOpt.hasValue()) {
        auto diagInfo = diagInfoOpt.getValue();

        // Return-before-yield errors will not be reported until the analysis
        // completes. So record it and continue.
        if (diagInfo.errKind == DiagnosticsInfo::ReturnBeforeYield) {
          if (!returnBeforeYieldError.hasValue()) {
            returnBeforeYieldError = diagInfo;
          }
          continue;
        }

        emitDiagnostics(diagInfo, bbStateMap, fun);
        return;
      }

      auto nextState = nextStateOpt.getValue();

      for (auto &succ : bb->getSuccessors()) {
        SILBasicBlock *succBB = succ.getBB();
        // Optimistically try to set the state of the successor as next state.
        auto insertResult = bbStateMap.try_emplace(succBB, nextState);

        // If the insertion was successful, it means we are seeing the successor
        // for the first time. Add the successor to the worklist.
        if (insertResult.second) {
          worklist.insert(worklist.begin(), succBB);
          continue;
        }

        // Here, the successor already has a state. Therefore, we have
        // to merge the states. if we see a different state after merging,
        // we have to propagate the merged state.
        const auto &succState = insertResult.first->second;
        auto mergedState = merge(succState, nextState, succBB);

        if (mergedState.yieldState == succState.yieldState)
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

    if (returnBeforeYieldError.hasValue()) {
      emitDiagnostics(returnBeforeYieldError.getValue(), bbStateMap, fun);
    }
  }

  void emitDiagnostics(DiagnosticsInfo &diagInfo,
                       llvm::DenseMap<SILBasicBlock *, BBState> &bbStateMap,
                       SILFunction &fun) {
    ASTContext &astCtx = fun.getModule().getASTContext();

    switch (diagInfo.errKind) {
    case DiagnosticsInfo::ReturnBeforeYield: {
      diagnose(astCtx, diagInfo.termInst->getLoc().getSourceLoc(),
               diag::return_before_yield);
      return;
    }
    case DiagnosticsInfo::MultipleYield: {
      diagnose(astCtx, diagInfo.termInst->getLoc().getSourceLoc(),
               diag::multiple_yields);
      // Add a note that points to the previous yield.
      diagnose(astCtx,
               diagInfo.inState.getYieldInstruction()->getLoc().getSourceLoc(),
               diag::previous_yield);
      return;
    }
    case DiagnosticsInfo::ReturnOnConflict: {
      // Here we try to figure out the source-level branch construct
      // (such as if-then-else) that is responsible for this Conflict state
      // and explain the error, by analyzing the AST, if it is available.

      auto *conflictBB = diagInfo.inState.getConflictBB();
      auto *yieldInst = diagInfo.inState.getYieldInstruction();

      // Emit an error on the return statement and add a note pointing to one
      // yield found by the analysis.
      diagnose(astCtx, diagInfo.termInst->getLoc().getSourceLoc(),
               diag::possible_return_before_yield);
      diagnose(astCtx, yieldInst->getLoc().getSourceLoc(), diag::one_yield);

      auto fallBackDiagnostics = [&] {
        // If there is no better option, point to the conflicting source loc.
        diagnose(astCtx, conflictBB->begin()->getLoc().getSourceLoc(),
                 diag::conflicting_merge);
      };

      // Find the conditional branch whose targets meet at conflict BB

      // Using the AST information, try point to the conditional statement
      // that resulted in the conflict.
      auto *yieldStmt = yieldInst->getLoc().getAsASTNode<Stmt>();
      Decl *funDecl = fun.getLocation().getAsASTNode<Decl>();
      if (!yieldStmt || !funDecl) {
        fallBackDiagnostics();
        return;
      }

      Stmt *condStmt = getContainingCondStmt(funDecl, yieldStmt);
      if (!condStmt) {
        fallBackDiagnostics();
        return;
      }

      switch (condStmt->getKind()) {
      case StmtKind::If: {
        emitIfStmtDiagnosticNote(dyn_cast<IfStmt>(condStmt), yieldStmt, astCtx);
        return;
      }
      case StmtKind::Switch:
      case StmtKind::Guard:
        fallBackDiagnostics();
        return;
      // Handle try-catch
      default:
        llvm_unreachable("Unsupported conditional statement.");
    }
    }
  }
  }

  void emitIfStmtDiagnosticNote(IfStmt *ifstmt, Stmt *yieldStmt,
                                 ASTContext &astCtx) {
    // Does the yield appear in the then branch?
    auto thenHasYield = containsStmt(ifstmt->getThenStmt(), yieldStmt);

    // If-else statements?
    if (ifstmt->getElseStmt()) {
      auto diag = thenHasYield
      ? diag::no_yield_in_else : diag::no_yield_in_then;
      diagnose(astCtx, ifstmt->getIfLoc(), diag);
      diagnose(astCtx, yieldStmt->getStartLoc(), diag::one_yield);
      return;
    }

    // If without else.
    auto diag = thenHasYield
    ? diag::no_yield_in_fallthrough : diag::no_yield_in_then;
    diagnose(astCtx, ifstmt->getIfLoc(), diag);
  }

  /// Return true if the given 'stmt' is a conditional statement of the source
  /// language.
  static bool isCondStmt(Stmt *stmt) {
    switch (stmt->getKind()) {
    case StmtKind::If:
    case StmtKind::Switch:
    case StmtKind::Guard:
      return true;
    default:
      return false;
    }
  }

  /// Check if 'keyStmt' is contained in the AST rooted at 'rootStmt'
  bool containsStmt(Stmt *rootStmt, Stmt *keyStmt) {
    class SearchTraversal : public ASTWalker {
      const Stmt *searchKey;
      bool result = false;

    public:
      SearchTraversal(Stmt *key): searchKey(key) { }

      std::pair<bool, Stmt *> walkToStmtPre(Stmt *s) override {
        if (s == searchKey) {
          result = true;
          return { false, s};
        }
        return { true, s };
      }

      bool walkToDeclPre(Decl *d) override { return !result; }

      std::pair<bool, Expr *> walkToExprPre(Expr *e) override {
        return { !result, e };
      }

      bool getResult() const { return result; }
    };

    SearchTraversal traversal(keyStmt);
    rootStmt->walk(traversal);
    return traversal.getResult();
  }

  /// Obtain the closest conditional statement that contains the given
  /// stmt. Return nullptr if there is no such conditional statement.
  Stmt *getContainingCondStmt(Decl *funDecl, Stmt *stmt) {
    class ClosestCondStmtTraversal : public ASTWalker {
      const Stmt *searchKey;
      // A stack of open conditional statements.
      llvm::SmallVector<Stmt *, 4> condStmts;
      Stmt *result = nullptr;

    public:
      ClosestCondStmtTraversal(Stmt *key): searchKey(key) { }

      std::pair<bool, Stmt *> walkToStmtPre(Stmt *s) override {
        if (s == searchKey) {
          if (!condStmts.empty()) {
            result = condStmts.back();
          }
          return { false, s};
        }

        if (isCondStmt(s)) {
          condStmts.push_back(s);
        }
        return { true, s };
      }

      Stmt * walkToStmtPost(Stmt *s) override {
        if (isCondStmt(s) && !condStmts.empty() && s == condStmts.back()) {
          condStmts.pop_back();
        }
        return s;
      }

      bool walkToDeclPre(Decl *d) override { return !result; }

      std::pair<bool, Expr *> walkToExprPre(Expr *e) override {
        return { !result, e };
      }

      Stmt *getResult() const { return result; }
    };

    ClosestCondStmtTraversal traversal(stmt);
    funDecl->walk(traversal);
    return traversal.getResult();
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
