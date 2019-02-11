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
#include "swift/SIL/Dominance.h"
#include "swift/AST/ASTWalker.h"
#include "swift/SIL/BasicBlockUtils.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "llvm/ADT/DenseSet.h"
#include "swift/AST/Stmt.h"
#include "swift/AST/Expr.h"

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
  
  typedef llvm::DenseMap<SILInstruction *, std::shared_ptr<SmallPtrSetImpl<SILBasicBlock *>>> BranchInfo;

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
    
    // These states are stored for the conflict case
    SILInstruction *conflictBranch = nullptr;
    SILBasicBlock *noYieldTarget = nullptr;
    
    // A mapping from SIL branch instruction to the set of targets seen.
    // If the value equals the total number of targets of the branch,
    // the key can be removed as all targets have been seen.
    // This is map is populated only during non-conflict states. When
    // a conflict is seen the map is frozen and the conflicting branch and
    // the conflicting targets are recorded.
    BranchInfo seenBranches;

    //BBState(YieldState state): yieldState(state) { }

  public:

    BBState() { }

    /// Creates an AfterYield state with the provided auxiliary information.
    BBState getAfterYieldState(SILInstruction *yieldInstParam) {
      assert(yieldInstParam != nullptr);

      BBState afterYield = *this; // this will copy all diagnostic info.
      afterYield.yieldState = YieldState::AfterYield;
      afterYield.yieldInst = yieldInstParam;
      return afterYield;
    }

    static BBState getConflictState(SILInstruction *yieldInstParam,
                             SILBasicBlock *conflictBBParam,
                             BBState oldState, BBState newState) {
      assert(yieldInstParam != nullptr);
      assert(conflictBBParam != nullptr);

      BBState conflictState; // This will copy diagnostics info.
      conflictState.yieldState = YieldState::Conflict;
      conflictState.yieldInst = yieldInstParam;
      conflictState.conflictBB = conflictBBParam;
      
      // Populate the data needed for diagnostics.
      conflictState.populateConflictData(oldState, newState);

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
    
    BBState withCondBranch(CondBranchInst *condbr, SILBasicBlock *destBB) {
      assert (condbr != nullptr);
      assert (yieldState != Conflict);
      
      if (seenBranches.count(condbr)) {
        // We are seeing the branch again through a loop. So ignore this.
        return *this;
      }
      
      llvm::errs() << "Adding conditional branch: " << *condbr << "\n";
      BBState outState = *this;
      
      auto *targetSet = new SmallPtrSet<SILBasicBlock *, 4>();
      targetSet->insert(destBB);
      
      outState.seenBranches.try_emplace(condbr, targetSet);
      return outState;
    }
    
    void unionSeenBranches(BranchInfo &from) {
      for (auto &kv: from) {
        auto key = kv.first;
        const auto value = kv.second;
        
        if (!seenBranches.count(key)) {
          auto *targetSet = new SmallPtrSet<SILBasicBlock *, 4>();
          seenBranches.try_emplace(key, targetSet);
        }
        
        auto existingTargetSet = seenBranches.lookup(key);
        existingTargetSet->insert(value->begin(), value->end());
      }
    }
    
    // This function is optional, we need not remove completed branches.
    void removeCompletedBranches() {
      SmallPtrSet<SILInstruction *, 4> compKeys;
      for (auto &kv: seenBranches) {
        const auto key = kv.first;
        const auto value = kv.second;

        if (value->size() == 2) {
          // We are only handling conditional branches as of now.
          compKeys.insert(key);
        }
      }
      
      for (auto k : compKeys) {
        seenBranches.erase(k);
      }
    }
    
    void populateConflictData(BBState oldState, BBState newState) {
      assert (oldState.yieldState != newState.yieldState);
      assert (oldState.yieldState != Conflict &&
              newState.yieldState != Conflict);
      
      auto oldSeenBranches = oldState.seenBranches;
      auto newSeenBranches = newState.seenBranches;
      
      for (auto &kv: oldSeenBranches) {
        auto key = kv.first;
        const auto value = kv.second;
        
        if (!newSeenBranches.count(key)) {
          // This cannot be the branch that is merged here, as it strictly
          // belongs to one of the paths but not the other.
          continue;
        }
        
        const auto newValue = newSeenBranches.lookup(key);
        for (auto elem : *value) {
          if (!newValue->count(elem)) {
            // We have seen a different target of this branch along the
            // conflict and no conflict path.
            // Therefore, this branch has been merged here and the targets
            // have conflicting yield states.
            conflictBranch = key;
            noYieldTarget = (oldState.yieldState == BeforeYield)
              ? (*value->begin()) : elem;
            return;
          }
        }
      }
    }
    
    BBState mergeWith(BBState newState) {
      assert (yieldState == newState.yieldState);
      
      // For each branch in the new and old states, union the targets.
      // If at some point, all targets are seen, remove the target.
      BBState mergedState;
      mergedState.yieldState = yieldState;
      
      mergedState.unionSeenBranches(seenBranches);
      mergedState.unionSeenBranches(newState.seenBranches);
      //mergedState.removeCompletedBranches();
      return mergedState;
    }
    
    SILInstruction *getConflictBranch() const {
      return conflictBranch;
    }
    
    SILBasicBlock *getNoYieldTarget() const {
      return noYieldTarget;
    }
    
    void dump() {
      llvm::errs() << "Yield state: " << yieldState << "\n";
      llvm::errs() << "Conditional branches: \n";
      for (auto &kv : seenBranches) {
        llvm::errs() << " key: " << *kv.first << " value: [";
        for (auto elem : *kv.second) {
          llvm::errs() << "bb" << elem->getDebugID() << ";";
        }
        llvm::errs() << "]\n";
      }
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
  /// TODO: handle throws.
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
      return inState.getAfterYieldState(term);
    }
    return inState;
  }
  
  /// This will only change the diagnostics state.
  BBState propagateAlongCFGEdge(BBState srcState, SILBasicBlock *srcBB,
                                SILBasicBlock *destBB) {
    if (srcState.yieldState == Conflict) // Conflicts remain the same.
      return srcState;
    
    auto *term = srcBB->getTerminator();
    if (auto condbr = dyn_cast<CondBranchInst>(term)) {
      return srcState.withCondBranch(condbr, destBB);
    }
    return srcState;
  }

  /// Merge oldState with newState. This performs the dataflow merge operation.
  /// This may have to create a new Conflict state, in which case it uses
  /// the mergeBlock which is the merge point where conflict is detected.
  BBState merge(BBState oldState, BBState newState, SILBasicBlock *mergeBlock) {
    // If the newState or oldState is already top of the lattice, return it.
    if (oldState.yieldState == Conflict) {
      return oldState;
    }

    if (newState.yieldState == Conflict) {
      return newState;
    }
    
    if (oldState.yieldState == newState.yieldState) {
      // Here we have identical states. Merge the diagnostics state here,
      // which will remove completed branches.
      return oldState.mergeWith(newState);
    }

    // Here, one state is AfterYield and the other one is BeforeYield.
    // Therefore, create a new conflict state with the necessary auxiliary
    // information.
    SILInstruction *yieldInst = (newState.yieldState == YieldState::AfterYield)
                                  ? newState.getYieldInstruction()
                                  : oldState.getYieldInstruction();

    return BBState::getConflictState(yieldInst, mergeBlock,
                                     oldState, newState);
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
      auto afterStateOpt = propagate(state, bb, diagInfoOpt);

      // Diagnose errors if any.
      if (!afterStateOpt.hasValue()) {
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

      auto afterState = afterStateOpt.getValue();

      for (auto &succ : bb->getSuccessors()) {
        SILBasicBlock *succBB = succ.getBB();
        
        auto nextState = propagateAlongCFGEdge(afterState, bb, succBB);
        llvm::errs() << "State for bb" << succBB->getDebugID() << "\n";
        nextState.dump();
        
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
      // Here we try to figure out the branch construct that is responsible
      // for this Conflict state and explain the error.
      // If the source information is available extract the source-level
      // branch statement (such as if-then-else) from the AST and present it
      // in diagnostics, otherwise present a deafult and less precise
      // diagnostics.

      // Emit an error on the return statement.
      diagnose(astCtx, diagInfo.termInst->getLoc().getSourceLoc(),
               diag::possible_return_before_yield);

      auto *yieldInst = diagInfo.inState.getYieldInstruction();
      auto *conflictBranch = diagInfo.inState.getConflictBranch();
      auto *noYieldTarget = diagInfo.inState.getNoYieldTarget();
      
      assert (yieldInst);
      
      if (!conflictBranch || !noYieldTarget) {
        if (!conflictBranch)
          llvm::errs() << "Conflict branch is null\n ";
        
        if (!noYieldTarget)
          llvm::errs() << "no yield target is null\n ";
        diagnose(astCtx, yieldInst->getLoc().getSourceLoc(), diag::one_yield);
        return;
      }

      // If there is no better option, point to the conflicting branch's
      // source location, and one yield that is found.
      auto fallBackDiagnostics = [&] {
        // If there is no better option, point to the conflicting branch's
        // source location.
        diagnose(astCtx, conflictBranch->getLoc().getSourceLoc(),
                 diag::conflicting_branch);
        // Include noYieldTarget here instead of the yield instruction.
        llvm::errs() << "No Yield target: BB" << noYieldTarget->getDebugID() << "\n";
        diagnose(astCtx, yieldInst->getLoc().getSourceLoc(), diag::one_yield);
      };
      
      fallBackDiagnostics();
      return;

      // Using the AST information, try to find the conditional statement
      // that corresponds to the branchingInst.
      auto branchLoc = conflictBranch->getLoc();
      Decl *funDecl = fun.getLocation().getAsASTNode<Decl>();
      auto *yieldStmt = yieldInst->getLoc().getAsASTNode<Stmt>();
      if (!yieldStmt || !funDecl || !branchLoc.isASTNode()) {
        // We do not have sufficient AST information here.
        fallBackDiagnostics();
        return;
      }

      Stmt *branchingStmt = getBranchingSourceStmt(funDecl, conflictBranch);
      if (!branchingStmt) {
        // Cannot extract a source-level branching statement here.
        fallBackDiagnostics();
        return;
      }

      switch (branchingStmt->getKind()) {
      case StmtKind::If: {
        emitIfStmtDiagnosticNote(dyn_cast<IfStmt>(branchingStmt), yieldStmt,
                                 astCtx);
        return;
      }
      case StmtKind::Guard: {
        emitGuardStmtDiagnosticNote(dyn_cast<GuardStmt>(branchingStmt),
                                    yieldStmt, astCtx);
        return;
      }
      case StmtKind::Switch: {
        emitSwitchStmtDiagnosticNote(dyn_cast<SwitchStmt>(branchingStmt),
                                     astCtx);
        return;
      }
      default:
        // Unsupported  branching statement.
        fallBackDiagnostics();
        return;
    }
    }
  }
  }

  /// Try to obtain the branching statement within a function declaration
  /// that contains the given ASTNode. Return nullptr if there is no such
  /// branching statement.
  Stmt *getBranchingSourceStmt(Decl *funDecl, SILInstruction *branchingInst) {
    auto branchLoc = branchingInst->getLoc();
    auto branchingASTNode = branchLoc.isASTNode<Expr>()
                              ? ASTNode(branchLoc.getAsASTNode<Expr>())
                              : ASTNode(branchLoc.getAsASTNode<Stmt>());

    // Check if the branchingASTNode corresponds to a branching statement.
    if (branchingASTNode.is<Stmt *>()) {
      auto *stmt = branchingASTNode.get<Stmt *>();
      if (isBranchingStmt(stmt)) {
        return stmt;
      }
    }

    // Otherwise, the branchingASTNode could be the condition of a conditional
    // statement like if-else or guard.
    if (branchingASTNode.is<Expr *>()) {
      auto *expr = branchingASTNode.get<Expr *>();
      return findConditionalStmtWithCondition(funDecl, expr);
    }
    return nullptr;
  }

  /// Return true if the given 'stmt' may branch to multiple targets but
  /// is not a loop. This includes, if, guard, switch, try-catch statements.
  static bool isBranchingStmt(Stmt *stmt) {
    switch (stmt->getKind()) {
    case StmtKind::If:
    case StmtKind::Switch:
    case StmtKind::Guard:
      return true;
    default:
      return false;
    }
  }

  /// Return true if the given 'expr' is the branch condition of a conditional
  /// statement such as if-else or guard.
  Stmt *findConditionalStmtWithCondition(Decl *funDecl, Expr *cond) {
    Stmt *result = nullptr;
    hasASTNodeSatisfyingPredicate(funDecl,
            [&](ASTNode node){
              // If node is a conditional statement, check if its branch
              // condition matched 'cond'.
              if (!node.is<Stmt *>())
                return false;

              auto *labeledCondStmt =
                    dyn_cast<LabeledConditionalStmt>(node.get<Stmt *>());
              if (!labeledCondStmt)
                return false;

              for (auto condElem : labeledCondStmt->getCond()) {
                if (hasExpr(&condElem, cond)) {
                  result = labeledCondStmt;
                  return true;
                }
              }
              return false;
            });
    return result;
  }

  void emitIfStmtDiagnosticNote(IfStmt *ifstmt, Stmt *yieldStmt,
                                ASTContext &astCtx) {
    // Does the yield appear in the then branch?
    auto thenHasYield = hasStmt(ifstmt->getThenStmt(), yieldStmt);

    // If-else statements?
    if (ifstmt->getElseStmt()) {
      auto diag = thenHasYield
                    ? diag::no_yield_in_else : diag::no_yield_in_then;
      auto srcLoc = thenHasYield ? ifstmt->getElseLoc() : ifstmt->getIfLoc();
      diagnose(astCtx, srcLoc, diag);
      return;
    }

    // If without else.
    auto diag = thenHasYield
                    ? diag::no_yield_in_fallthrough : diag::no_yield_in_then;
    auto srcLoc = thenHasYield ? ifstmt->getEndLoc() : ifstmt->getIfLoc();
    diagnose(astCtx, srcLoc, diag);
  }

  void emitGuardStmtDiagnosticNote(GuardStmt *guardStmt, Stmt *yieldStmt,
                                   ASTContext &astCtx) {
    // Does the yield appear in the else branch?
    auto elseHasYield = hasStmt(guardStmt->getBody(), yieldStmt);

    auto diag = elseHasYield ? diag::no_yield_in_guard_fallthrough
                             : diag::no_yield_in_guard_else;
    auto srcLoc =
        elseHasYield ? guardStmt->getEndLoc() : guardStmt->getGuardLoc();
    diagnose(astCtx, srcLoc, diag);
  }

  void emitSwitchStmtDiagnosticNote(SwitchStmt *switchStmt,
                                    ASTContext &astCtx) {
    // Find a case without any yield statements.
    CaseStmt *noYieldCase = nullptr;
    for (auto switchCase : switchStmt->getCases()) {
      if (noYieldCase)
        break;
      bool hasYield =
          hasASTNodeSatisfyingPredicate(switchCase,
                                    [&] (ASTNode node) {
                                      return (node.is<Stmt *>() &&
                                         isa<YieldStmt>(node.get<Stmt *>()));
                                    });
      if (!hasYield) {
        noYieldCase = switchCase;
      }
    }
    if (!noYieldCase) {
      // Here, the control-flow is possibly obscure due to throws, jumps etc.
      diagnose(astCtx, switchStmt->getLoc(), diag::conflicting_switch);
      return;
    }
    diagnose(astCtx, noYieldCase->getLoc(), diag::switch_case_without_yield);
  }

  // The following are some utility functions for traversing ASTs.

  /// A utility function that returns true iff the AST rooted at 'root'
  /// has the expression as a descendant
  template <typename T>
  static bool hasExpr(T *root, Expr *key) {
    return hasASTNodeSatisfyingPredicate(root,
                                     [&] (ASTNode node) {
                                        return (node.is<Expr *>() &&
                                                node.get<Expr *>() == key);
                                     });
  }

  template <typename T>
  static bool hasStmt(T *root, Stmt *key) {
    return hasASTNodeSatisfyingPredicate(root,
                                         [&] (ASTNode node) {
                                           return (node.is<Stmt *>() &&
                                                   node.get<Stmt *>() == key);
                                         });
  }

  /// A utility function that returns true iff the AST rooted at 'root'
  /// has a descendant node satisfying the given predicate. 'root' should have
  /// a walk method that accepts an AST walker.
  template <typename T>
  static bool hasASTNodeSatisfyingPredicate(T *root,
                                            std::function<bool(ASTNode)> pred) {
    class SearchTraversal : public ASTWalker {
      std::function<bool(ASTNode)> pred;
      bool result = false;

    public:
      SearchTraversal(std::function<bool(ASTNode)> p): pred(p) { }

      std::pair<bool, Stmt *> walkToStmtPre(Stmt *s) override {
        if (pred(s)) {
          result = true;
          return { false, s};
        }
        return { true, s };
      }

      bool walkToDeclPre(Decl *d) override {
        if (pred(d)) {
          result = true;
          return false;
        }
        return !result;
      }

      std::pair<bool, Expr *> walkToExprPre(Expr *e) override {
        if (pred(e)) {
          result = true;
          return { false, e};
        }
        return { !result, e };
      }

      bool getResult() const { return result; }
    };

    SearchTraversal traversal(pred);
    root->walk(traversal);
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

//      // Find the conditional branch whose targets meet at conflict BB.
//      // The conditional branch will be an immediate dominator of the
//      // conflict BB.
//      auto dominanceInfo = DominanceInfo(&fun);
//      auto *immediateDominator = dominanceInfo.getNode(conflictBB)->getIDom();
//
//      assert(immediateDominator);
//      assert(immediateDominator->getBlock());
//
//      auto *branchingInst = immediateDominator->getBlock()->getTerminator();
//      assert(branchingInst);
