//===--- LocalizationKeyExtraction.cpp - Dump localization keys to metadata ===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ASTContext.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/DiagnosticsSIL.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Module.h"
#include "swift/AST/SubstitutionMap.h"
#include "swift/Basic/OptimizationMode.h"
#include "swift/AST/SemanticAttrs.h"
#include "swift/Demangling/Demangle.h"
#include "swift/Demangling/Demangler.h"
#include "swift/SIL/BasicBlockUtils.h"
#include "swift/SIL/CFG.h"
#include "swift/SIL/InstructionUtils.h"
#include "swift/SIL/OwnershipUtils.h"
#include "swift/SIL/SILBasicBlock.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILConstants.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILLocation.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/TypeLowering.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/CFGOptUtils.h"
#include "swift/SILOptimizer/Utils/ConstExpr.h"
#include "swift/SILOptimizer/Utils/InstOptUtils.h"
#include "swift/SILOptimizer/Utils/SILInliner.h"
#include "swift/SILOptimizer/Utils/SILOptFunctionBuilder.h"
#include "swift/SILOptimizer/Utils/ValueLifetime.h"
#include "llvm/ADT/BreadthFirstIterator.h"
#include "llvm/ADT/MapVector.h"

using namespace swift;
using namespace Lowering;

template <typename... T, typename... U>
static void diagnose(ASTContext &Context, SourceLoc loc, Diag<T...> diag,
                     U &&... args) {
  // The lifetime of StringRef arguments will be extended as necessary by this
  // utility. The copy happens in onTentativeDiagnosticFlush at the bottom of
  // DiagnosticEngine.cpp, which is called when the destructor of the
  // InFlightDiagnostic returned by diagnose runs.
  Context.Diags.diagnose(loc, diag, std::forward<U>(args)...);
}

namespace {

/// If the given instruction is a call to the compiler-intrinsic initializer
/// of String that accepts string literals, return the called function.
/// Otherwise, return nullptr.
//static SILFunction *getStringMakeUTF8Init(SILInstruction *inst) {
//  auto *apply = dyn_cast<ApplyInst>(inst);
//  if (!apply)
//    return nullptr;
//
//  SILFunction *callee = apply->getCalleeFunction();
//  if (!callee || !callee->hasSemanticsAttr(semantics::STRING_MAKE_UTF8))
//    return nullptr;
//  return callee;
//}
//
///// State needed for constant folding.
struct EvaluationState {
  /// Storage for symbolic values constructed during interpretation.
  SymbolicValueBumpAllocator allocator;

  /// Evaluator for evaluating instructions one by one.
  ConstExprStepEvaluator constantEvaluator;

  /// Instruction from where folding must begin.
  SILInstruction *beginInstruction;

  /// Instructions that mark the end points of constant evaluation.
  SmallSetVector<SILInstruction *, 2> endInstructions;

  EvaluationState(SILFunction *fun, unsigned assertConfig, SILInstruction *beginInst,
            ArrayRef<SILInstruction *> endInsts)
      : constantEvaluator(allocator, fun, assertConfig),
        beginInstruction(beginInst),
        endInstructions(endInsts.begin(), endInsts.end()) {}
};

///// Decide if the given instruction (which could possibly be a call) should
///// be constant evaluated.
/////
///// \returns true iff the given instruction is not a call or if it is, it calls
///// a known constant-evaluable function such as string append etc., or calls
///// a function annotate as "constant_evaluable".
static bool shouldAttemptEvaluation(SILInstruction *inst) {
  auto *apply = dyn_cast<ApplyInst>(inst);
  if (!apply)
    return true;
  SILFunction *calleeFun = apply->getCalleeFunction();
  if (!calleeFun)
    return false;
  return isConstantEvaluable(calleeFun);
}

///// Skip or evaluate the given instruction based on the evaluation policy and
///// handle errors. The policy is to evaluate all non-apply instructions as well
///// as apply instructions that are marked as "constant_evaluable".
static std::pair<Optional<SILBasicBlock::iterator>, Optional<SymbolicValue>>
evaluateOrSkip(ConstExprStepEvaluator &stepEval,
               SILBasicBlock::iterator instI) {
  SILInstruction *inst = &(*instI);

  // Note that skipping a call conservatively approximates its effects on the
  // interpreter state.
  if (shouldAttemptEvaluation(inst)) {
    return stepEval.tryEvaluateOrElseMakeEffectsNonConstant(instI);
  }
  return stepEval.skipByMakingEffectsNonConstant(instI);
}

///// Diagnose traps and instruction-limit exceeded errors. These have customized
///// error messages. \returns true if the given error is diagnosed. Otherwise,
///// returns false.
static bool diagnoseSpecialErrors(SILInstruction *unevaluableInst,
                                  SymbolicValue errorInfo) {
  SourceLoc sourceLoc = unevaluableInst->getLoc().getSourceLoc();
  ASTContext &ctx = unevaluableInst->getFunction()->getASTContext();
  UnknownReason unknownReason = errorInfo.getUnknownReason();

  if (unknownReason.getKind() == UnknownReason::Trap) {
    // We have an assertion failure or fatal error.
    diagnose(ctx, sourceLoc, diag::oslog_constant_eval_trap,
             unknownReason.getTrapMessage());
    return true;
  }
  if (unknownReason.getKind() == UnknownReason::TooManyInstructions) {
    // This should not normally happen. But could be because of extensions
    // defined by users, or very rarely due to unknown bugs in the os_log API
    // implementation. These errors may get hidden during testing as it is input
    // specific.
    diagnose(ctx, sourceLoc, diag::oslog_too_many_instructions);
    return true;
  }
  return false;
}

///// Diagnose failure during evaluation of a call to a constant-evaluable
///// function that is not a specially-handled error. These are errors that
///// happen within  'appendInterpolation' calls, which must be constant
///// evaluable by the definition of APIs.
static void diagnoseErrorInConstantEvaluableFunction(ApplyInst *call,
                                                     SymbolicValue errorInfo) {
  SILFunction *callee = call->getCalleeFunction();
  assert(callee);
  SILLocation loc = call->getLoc();
  SourceLoc sourceLoc = loc.getSourceLoc();
  ASTContext &astContext = callee->getASTContext();

  // Here, we know very little about what actually went wrong. It could be due
  // to bugs in the library implementation or in extensions created by users.
  // Emit a general message here and some diagnostic notes.
  std::string demangledCalleeName = Demangle::demangleSymbolAsString(
      callee->getName(),
      Demangle::DemangleOptions::SimplifiedUIDemangleOptions());
  diagnose(astContext, sourceLoc, diag::oslog_invalid_log_message);
  diagnose(astContext, sourceLoc, diag::oslog_const_evaluable_fun_error,
           demangledCalleeName);
  errorInfo.emitUnknownDiagnosticNotes(loc);
}

///// Detect and emit diagnostics for errors found during evaluation. Errors
///// can happen due to bugs in the implementation of the os log API, or
///// due to incorrect use of the os log API.
static bool detectAndDiagnoseErrors(SymbolicValue errorInfo,
                                    SILInstruction *unevaluableInst) {
  // TODO: fix the globalStrinTableBuiltin error after emitting diagnostics.
  SILFunction *parentFun = unevaluableInst->getFunction();
  ASTContext &astContext = parentFun->getASTContext();

  if (diagnoseSpecialErrors(unevaluableInst, errorInfo))
    return true;
  // If evaluation of any constant_evaluable function call fails, point
  // to that failed function along with a reason.
  ApplyInst *call = dyn_cast<ApplyInst>(unevaluableInst);
  if (call) {
    SILFunction *callee = call->getCalleeFunction();
    if (callee && isConstantEvaluable(callee)) {
      diagnoseErrorInConstantEvaluableFunction(call, errorInfo);
      return true; // abort evaluation.
    }
  }
  // Every other error must happen in the top-level code containing the string
  // interpolation construction and body of the log methods. If we have a
  // fail-stop error, point to the error and abort evaluation. Otherwise, just
  // ignore the error and continue evaluation as this error might not affect the
  // constant value of the OSLogMessage instance.
  if (isFailStopError(errorInfo)) {
    SILLocation loc = unevaluableInst->getLoc();
    diagnose(astContext, loc.getSourceLoc(), diag::oslog_invalid_log_message);
    errorInfo.emitUnknownDiagnosticNotes(loc);
    return true;
  }
  return false;
}

///// Given a 'foldState', constant evaluate instructions from
///// 'foldState.beginInstruction' until an instruction in
///// 'foldState.endInstructions' is seen. Add foldable, constant-valued
///// instructions discovered during the evaluation to
///// 'foldState.constantSILValues'.
static void
evaluateLocalizationInfoInitCode(EvaluationState &evalState) {
  ConstExprStepEvaluator &constantEvaluator = evalState.constantEvaluator;
  SILBasicBlock::iterator currI = evalState.beginInstruction->getIterator();
  auto &endInstructions = evalState.endInstructions;

  // The loop will break when it sees a return instruction or an instruction in
  // endInstructions or when the next instruction to evaluate cannot be
  // determined (which may happend due to non-constant branches).
  while (true) {
    SILInstruction *currInst = &(*currI);
    if (endInstructions.count(currInst))
      break;

    Optional<SymbolicValue> errorInfo = None;
    Optional<SILBasicBlock::iterator> nextI = None;

    std::tie(nextI, errorInfo) = evaluateOrSkip(constantEvaluator, currI);

    // If the evaluation of this instruction failed, check whether it should be
    // diagnosed and reported. If so, abort evaluation. Otherwise, continue
    // evaluation if possible as this error could be due to an instruction that
    // doesn't affect the OSLogMessage value.
    if (errorInfo && detectAndDiagnoseErrors(errorInfo.getValue(), currInst)) {
      return;
    }

    if (!nextI) {
      // We cannnot find the next instruction to continue evaluation, and we
      // haven't seen any reportable errors during evaluation. Therefore,
      // consider this the end point of evaluation.
      return;
    }

    // Set the next instruction to continue evaluation from.
    currI = nextI.getValue();

//    // If the instruction results are foldable and if we found a constant value
//    // for the results, record it.
//    for (SILValue instructionResult : currInst->getResults()) {
//      if (!isSILValueFoldable(instructionResult))
//        continue;
//
//      Optional<SymbolicValue> constantVal =
//          constantEvaluator.lookupConstValue(instructionResult);
//      if (constantVal.hasValue()) {
//        foldState.addConstantSILValue(instructionResult);
//      }
//    }
  }
}

///// Given a SILValue \p value, return the instruction immediately following the
///// definition of the value. That is, if the value is defined by an
///// instruction, return the instruction following the definition. Otherwise, if
///// the value is a basic block parameter, return the first instruction of the
///// basic block.
//SILInstruction *getInstructionFollowingValueDefinition(SILValue value) {
//  SILInstruction *definingInst = value->getDefiningInstruction();
//  if (definingInst) {
//    return &*std::next(definingInst->getIterator());
//  }
//  // Here value must be a basic block argument.
//  SILBasicBlock *bb = value->getParentBlock();
//  return &*bb->begin();
//}


/// Given a SILValue \p value, compute the set of transitive users of the value
/// (excluding value itself) by following the use-def chain starting at value.
/// Note that this function does not follow use-def chains though branches.
static void getTransitiveUsers(SILValue value,
                               SmallVectorImpl<SILInstruction *> &users) {
  // Collect the instructions that are data dependent on the value using a
  // fix point iteration.
  SmallPtrSet<SILInstruction *, 16> visitedUsers;
  SmallVector<SILValue, 16> worklist;
  worklist.push_back(value);

  while (!worklist.empty()) {
    SILValue currVal = worklist.pop_back_val();
    for (Operand *use : currVal->getUses()) {
      SILInstruction *user = use->getUser();
      if (visitedUsers.count(user))
        continue;
      visitedUsers.insert(user);
      llvm::copy(user->getResults(), std::back_inserter(worklist));
    }
  }
  // At this point, visitedUsers have all the transitive, data-dependent uses.
  users.append(visitedUsers.begin(), visitedUsers.end());
}

///// Collect the end points of the instructions that are data dependent on \c
///// value. A instruction is data dependent on \c value if its result may
///// transitively depends on \c value. Note that data dependencies through
///// addresses are not tracked by this function.
/////
///// \param value SILValue that is not an address.
///// \param fun SILFunction that defines \c value.
///// \param endUsers buffer for storing the found end points of the data
///// dependence chain.
static void
getEndPointsOfDataDependentChain(SILValue value, SILFunction *fun,
                                 SmallVectorImpl<SILInstruction *> &endUsers) {
  assert(!value->getType().isAddress());

  SmallVector<SILInstruction *, 16> transitiveUsers;
  // Get transitive users of value, ignoring use-def chain going through
  // branches. These transitive users define the end points of the constant
  // evaluation. Igoring use-def chains through branches causes constant
  // evaluation to miss some constant folding opportunities. This can be
  // relaxed in the future, if necessary.
  getTransitiveUsers(value, transitiveUsers);

  // Compute the lifetime frontier of all the transitive uses which are the
  // instructions following the last uses. Every exit from the last uses will
  // have a lifetime frontier.
  SILInstruction *valueDefinition = value->getDefiningInstruction();
  SILInstruction *def =
      valueDefinition ? valueDefinition : &(value->getParentBlock()->front());
  ValueLifetimeAnalysis lifetimeAnalysis =
      ValueLifetimeAnalysis(def, transitiveUsers);
  ValueLifetimeAnalysis::Frontier frontier;
  bool hasCriticlEdges = lifetimeAnalysis.computeFrontier(
      frontier, ValueLifetimeAnalysis::DontModifyCFG);
  endUsers.append(frontier.begin(), frontier.end());
  if (!hasCriticlEdges)
    return;
  // If there are some lifetime frontiers on the critical edges, take the
  // first instruction of the target of the critical edge as the frontier. This
  // will suffice as every exit from the visitedUsers must go through one of
  // them.
  for (auto edgeIndexPair : lifetimeAnalysis.getCriticalEdges()) {
    SILBasicBlock *targetBB =
        edgeIndexPair.first->getSuccessors()[edgeIndexPair.second];
    endUsers.push_back(&targetBB->front());
  }
}

///// Check whether OSLogMessage and OSLogInterpolation instances and all their
///// stored properties are constants. If not, it indicates errors that are due to
///// incorrect implementation of OSLogMessage either in the os module or in the
///// extensions created by users. Detect and emit diagnostics for such errors.
///// The diagnostics here are for os log library authors.
//static bool checkOSLogMessageIsConstant(SingleValueInstruction *osLogMessage,
//                                        FoldState &foldState) {
//  ConstExprStepEvaluator &constantEvaluator = foldState.constantEvaluator;
//  SILLocation loc = osLogMessage->getLoc();
//  SourceLoc sourceLoc = loc.getSourceLoc();
//  SILFunction *fn = osLogMessage->getFunction();
//  SILModule &module = fn->getModule();
//  ASTContext &astContext = fn->getASTContext();
//
//  Optional<SymbolicValue> osLogMessageValueOpt =
//      constantEvaluator.lookupConstValue(osLogMessage);
//  if (!osLogMessageValueOpt ||
//      osLogMessageValueOpt->getKind() != SymbolicValue::Aggregate) {
//    diagnose(astContext, sourceLoc, diag::oslog_non_constant_message);
//    return true;
//  }
//
//  // The first (and only) property of OSLogMessage is the OSLogInterpolation
//  // instance.
//  SymbolicValue osLogInterpolationValue =
//      osLogMessageValueOpt->getAggregateMembers()[0];
//  if (!osLogInterpolationValue.isConstant()) {
//    diagnose(astContext, sourceLoc, diag::oslog_non_constant_interpolation);
//    return true;
//  }
//
//  // Check if every proprety of the OSLogInterpolation instance has a constant
//  // value.
//  SILType osLogMessageType = osLogMessage->getType();
//  StructDecl *structDecl = osLogMessageType.getStructOrBoundGenericStruct();
//  assert(structDecl);
//
//  auto typeExpansionContext =
//      TypeExpansionContext(*osLogMessage->getFunction());
//  VarDecl *interpolationPropDecl = structDecl->getStoredProperties().front();
//  SILType osLogInterpolationType = osLogMessageType.getFieldType(
//      interpolationPropDecl, module, typeExpansionContext);
//  StructDecl *interpolationStruct =
//      osLogInterpolationType.getStructOrBoundGenericStruct();
//  assert(interpolationStruct);
//
//  auto propertyDecls = interpolationStruct->getStoredProperties();
//  ArrayRef<SymbolicValue> propertyValues =
//      osLogInterpolationValue.getAggregateMembers();
//  auto propValueI = propertyValues.begin();
//  bool errorDetected = false;
//  // Also, track if there is a string-valued property.
//  bool hasStringValuedProperty = false;
//
//  for (auto *propDecl : propertyDecls) {
//    SymbolicValue propertyValue = *(propValueI++);
//    if (!propertyValue.isConstant()) {
//      diagnose(astContext, sourceLoc, diag::oslog_property_not_constant,
//               propDecl->getNameStr());
//      errorDetected = true;
//      break;
//    }
//    hasStringValuedProperty = propertyValue.getKind() == SymbolicValue::String;
//  }
//
//  // If we have a string-valued property but don't have the stringInfo
//  // initialized here, it means the initializer OSLogInterpolation is explicitly
//  // called, which should be diagnosed.
//  if (hasStringValuedProperty && !foldState.stringInfo.isInitialized()) {
//    diagnose(astContext, sourceLoc, diag::oslog_message_explicitly_created);
//    errorDetected = true;
//  }
//  return errorDetected;
//}


///// Constant evaluate instructions starting from \p start and fold the uses
///// of the SIL value \p oslogMessage.
static void extractLocalizationKey(SILInstruction *start,
                         SingleValueInstruction *localizedStringKey,
                         unsigned assertConfig) {
  SILFunction *fun = start->getFunction();
  assert(fun->hasOwnership() && "function not in ownership SIL");

  // Initialize fold state.
  SmallVector<SILInstruction *, 2> endUsersOfOSLogMessage;
  getEndPointsOfDataDependentChain(localizedStringKey, fun, endUsersOfOSLogMessage);
  assert(!endUsersOfOSLogMessage.empty());
  
  EvaluationState state(fun, assertConfig, start, endUsersOfOSLogMessage);
  evaluateLocalizationInfoInitCode(state);
}

static bool isConstantEvaluableCall(ApplyInst *apply) {
  SILFunction *callee = apply->getCalleeFunction();
  return callee && isConstantEvaluable(callee);
}

/// Given a call to the initializer of OSLogMessage, which conforms to
/// 'ExpressibleByStringInterpolation', find the first instruction, if any, that
/// marks the begining of the string interpolation that is used to create an
/// OSLogMessage instance. This function traverses the backward data-dependence
/// chain of the given OSLogMessage initializer: \p oslogInit. As a special case
/// it avoids chasing the data-dependencies from the captured values of
/// partial-apply instructions, as a partial apply instruction is considered as
/// a constant regardless of the constantness of its captures.
static SILInstruction *beginOfInterpolation(ApplyInst *localizedKeyInit) {
  auto localizedInitCallSite = FullApplySite(localizedKeyInit);
  SILFunction *callee = localizedInitCallSite.getCalleeFunction();

  assert (callee->hasSemanticsAttrThatStartsWith("swiftui.localized_string_key.init"));
  // The initializer must return the OSLogMessage instance directly.
  assert(localizedInitCallSite.getNumArguments() >= 1 &&
         localizedInitCallSite.getNumIndirectSILResults() == 0);

  // List of backward dependencies that needs to be analyzed.
  SmallVector<SILInstruction *, 4> worklist = { localizedKeyInit };
  SmallPtrSet<SILInstruction *, 4> seenInstructions = { localizedKeyInit };
  // List of instructions that could potentially mark the beginning of the
  // interpolation.
  SmallPtrSet<SILInstruction *, 4> candidateStartInstructions;

  unsigned i = 0;
  while (i < worklist.size()) {
    SILInstruction *inst = worklist[i++];
    // Set when the defining instruction of an operand of inst is added to the
    // worklist. This indicates that we found an instruction that precedes
    // inst in the control-flow order.
    bool operandAdded = false;
    
    // Only consider arguments of fully-applied constant_evaluable functions.
    // Note that non-constant evaluable functions would anyway be skipped during
    // the evaluation and therefore it is not necessary to capture their
    // dependencies.
    if ((isa<ApplyInst>(inst) && !isConstantEvaluableCall(cast<ApplyInst>(inst)))
        ||  isa<PartialApplyInst>(inst)) {
      SILInstruction *definingInstruction =
          inst->getOperand(0)->getDefiningInstruction();
      assert(definingInstruction && "no function-ref operand in apply");
      if (seenInstructions.insert(definingInstruction).second) {
        worklist.push_back(definingInstruction);
        candidateStartInstructions.insert(definingInstruction);
      }
      operandAdded = true;
    }

    for (Operand &operand : inst->getAllOperands()) {
      if (SILInstruction *definingInstruction =
            operand.get()->getDefiningInstruction()) {
        if (seenInstructions.count(definingInstruction))
          continue;
        worklist.push_back(definingInstruction);
        seenInstructions.insert(definingInstruction);
        candidateStartInstructions.insert(definingInstruction);
        operandAdded = true;
      }
      // If there is no definining instruction for this operand, it could be a
      // basic block or function parameter. Such operands are not considered
      // in the backward slice. Dependencies through them are safe to ignore
      // in this context.
    }

    // If the instruction: `inst` has an operand, its definition should precede
    // `inst` in the control-flow order. Therefore, remove `inst` from the
    // candidate start instructions.
    if (operandAdded) {
      candidateStartInstructions.erase(inst);
    }

    if (!isa<AllocStackInst>(inst)) {
      continue;
    }

    // If we have an alloc_stack instruction, include stores into it into the
    // backward dependency list. However, whether alloc_stack precedes the
    // definitions of values stored into the location in the control-flow order
    // can only be determined by traversing the instrutions in the control-flow
    // order.
    AllocStackInst *allocStackInst = cast<AllocStackInst>(inst);
    for (StoreInst *storeInst : allocStackInst->getUsersOfType<StoreInst>()) {
      worklist.push_back(storeInst);
      candidateStartInstructions.insert(storeInst);
    }
    // Skip other uses of alloc_stack including function calls on the
    // alloc_stack and data dependenceis through them. This is done because
    // the constant evaluable parts of all functions using the alloc_stack are
    // expected to constants or auto closures that are constructed immediately
    // before the call and would only appear in the SIL after the alloc_stack
    // instruction.
  }

  // Find the first basic block in the control-flow order.
  SmallPtrSet<SILBasicBlock *, 4> candidateBBs;
  for (auto *candidate: candidateStartInstructions) {
    SILBasicBlock *candidateBB = candidate->getParent();
    candidateBBs.insert(candidateBB);
  }

  SILBasicBlock *firstBB = nullptr;
  if (candidateBBs.size() == 1) {
    firstBB = *candidateBBs.begin();
  } else {
    SILBasicBlock *entryBB = localizedKeyInit->getFunction()->getEntryBlock();
    for (SILBasicBlock *bb : llvm::breadth_first<SILBasicBlock *>(entryBB)) {
      if (candidateBBs.count(bb)) {
        firstBB = bb;
        break;
      }
    }
    if (!firstBB) {
      // This case will be reached only if the call appears in unreachable
      // code and, for some reason, its data depedencies extend beyond a basic
      // block. This case should generally not happen. Skip this case.
      // Unreachable code warning will appear here.
      return nullptr;
    }
  }

  // Iterate over the instructions in the firstBB and find the instruction that
  // starts the interpolation.
  SILInstruction *startInst = nullptr;
  for (SILInstruction &inst : *firstBB) {
    if (candidateStartInstructions.count(&inst)) {
      startInst = &inst;
      break;
    }
  }
  assert(startInst && "could not find beginning of interpolation");
  return startInst;
}

/// If the SILInstruction is an initialization of OSLogMessage, return the
/// initialization call as an ApplyInst. Otherwise, return nullptr.
static ApplyInst *getAsLocalizedStringKeyInit(SILInstruction *inst) {
  auto *applyInst = dyn_cast<ApplyInst>(inst);
  if (!applyInst) {
    return nullptr;
  }

  SILFunction *callee = applyInst->getCalleeFunction();
  if (!callee ||
      !callee->hasSemanticsAttrThatStartsWith("swiftui.localized_string_key.init")) {
    return nullptr;
  }

  // Default argument generators created for a function also inherit
  // the semantics attribute of the function. Therefore, check that there are
  // at least two operands for this apply instruction.
  if (applyInst->getNumOperands() > 1) {
    return applyInst;
  }
  return nullptr;
}

class LocalizationKeyExtraction : public SILFunctionTransform {

  ~LocalizationKeyExtraction() override {}

  /// The entry point to the transformation.
  void run() override {
    auto &fun = *getFunction();
    unsigned assertConfig = getOptions().AssertConfig;

    // Don't rerun optimization on deserialized functions or stdlib functions.
    if (fun.wasDeserializedCanonical()) {
      return;
    }

    // Collect all 'OSLogMessage.init' in the function. 'OSLogMessage' is a
    // custom string interpolation type used by the new OS log APIs.
    SmallVector<ApplyInst *, 4> localizedKeyInits;
    for (auto &bb : fun) {
      for (auto &inst : bb) {
        auto init = getAsLocalizedStringKeyInit(&inst);
        if (!init)
          continue;
        localizedKeyInits.push_back(init);
      }
    }

    // Constant fold the uses of properties of OSLogMessage instance. Note that
    // the function body will change due to constant folding, after each
    // iteration.
    for (auto *localizedKeyInit : localizedKeyInits) {
      SILInstruction *interpolationStart = beginOfInterpolation(localizedKeyInit);
      if (!interpolationStart) {
        // The log call is in unreachable code here.
        continue;
      }
      llvm::errs() << "Begining of interpolation " << *interpolationStart << "\n";
      extractLocalizationKey(interpolationStart, localizedKeyInit, assertConfig);
    }
  }
};

} // end anonymous namespace

SILTransform *swift::createLocalizationKeyExtraction() {
  return new LocalizationKeyExtraction();
}
