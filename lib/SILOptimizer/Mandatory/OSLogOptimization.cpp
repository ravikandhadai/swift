//===--- OSLogOptimizer.cpp - Optimizes calls to OS Log ===//
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

#include "swift/AST/ASTContext.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/DiagnosticsSIL.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Module.h"
#include "swift/AST/SubstitutionMap.h"
#include "swift/SIL/InstructionUtils.h"
#include "swift/SIL/SILBasicBlock.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILConstants.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILLocation.h"
#include "swift/SIL/SILModule.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/ConstExpr.h"
#include "swift/SILOptimizer/Utils/Local.h"
#include "swift/SILOptimizer/Utils/SILInliner.h"
#include "swift/SILOptimizer/Utils/SILOptFunctionBuilder.h"
#include "llvm/ADT/MapVector.h"

using namespace swift;

template <typename... T, typename... U>
static void diagnose(ASTContext &Context, SourceLoc loc, Diag<T...> diag,
                     U &&... args) {
  Context.Diags.diagnose(loc, diag, std::forward<U>(args)...);
}

namespace {

class OSLogOptimization : public SILFunctionTransform {

  ~OSLogOptimization() override {}

  /// Given an os log call, constant evaluate and fold values that should be
  /// compile-time constants. Specifically, constant evaluate relevant
  /// instructions and extract the format string as a compile-time constant.
  void optimizeOSLogCall(ApplyInst *oslogCall, SILFunction *caller);

  /// Given a range of instructions from `first` to `last`, fold the constants
  /// that could be discovered be constant evaluating the instructions.
  void constantFold(SILBasicBlock::iterator first,
                    SILBasicBlock::iterator last);

  /// Constant evaluate the instructions in the range 'first' to 'last' and
  /// collect the constant values found during evaluation in the provided
  /// `constantMap`. Only constant values that are of string or integer types
  /// are collected.
  /// \returns error information for emitting diagnostics if the evaluation
  /// failed.
  Optional<SymbolicValue> collectConstants(
      SILBasicBlock::iterator first, SILBasicBlock::iterator last,
      llvm::SmallMapVector<SingleValueInstruction *, SymbolicValue, 4>
          &constantMap,
      ConstExprStepEvaluator &constantEvaluator);

  /// Given a `constantMap` that maps instructions to their constant values,
  /// substitute the instructions with the constant values. The constant values
  /// could be strings or Stdlib integer-struct values.
  void substituteConstants(llvm::SmallMapVector<SingleValueInstruction *,
                                                SymbolicValue, 4> &constantMap);

  /// Generate SIL code for constructing the constant given symbolic value
  /// `symVal`. Note strings and struct-typed constant values will require
  /// mutliple instructions to be emitted.
  /// \param symVal symbolic value for which SIL code needs to be emitted.
  /// \param expectedType the expected type of the instruction that would be
  /// constructing the symbolic value `symVal`. The type is accepted as a
  /// parameter as some symbolic values like integer constants can inhabit more
  /// than one type.
  /// \param builder SILBuilder that provides the context for emitting the code
  /// for the symbolic value
  /// \param loc SILLocation to use in the emitted instructions.
  SILValue emitCodeForSymbolicValue(SymbolicValue symVal, SILType &expectedType,
                                    SILBuilder &builder, SILLocation &loc);

  /// Detect and emit diagnostics for error founds during evaluation. Errors
  /// can happen due to incorrect implementation of the os log API in overlays
  /// or due to incorrect use of the os log API.
  /// TODO: some of the checks here would be made redundant by a dedicated
  /// diagnostics check that will happen before the optimization starts.
  bool detectAndDiagnoseErrors(Optional<SymbolicValue> errorInfo,
                               SingleValueInstruction *osLogMessageAddr,
                               ConstExprStepEvaluator &constantEvaluator);

  /// The entry point to the transformation.
  void run() override;

  // Cache a couple of SIL-level, string-related information while traversing
  // the function body. This information is needed for constant folding string
  // literals and recording it will avoid reconstructing it.
  SILFunction *stringInitIntrinsic = nullptr;
  Optional<SILType> stringMetatype = None;

  /// Extract and cache the required string-related information from the
  /// given instruction, if possible.
  void extractStringInfoFromInstruction(SILInstruction *inst);
};

/// Find the SILBasicBlock::iterator for a given instruction
static SILBasicBlock::iterator
findIteratorForInstruction(SILInstruction *keyInst) {
  auto *bb = keyInst->getParent();
  for (auto currI = bb->begin(), endI = bb->end(); currI != endI; currI++) {
    if (keyInst == &(*currI)) {
      return currI;
    }
  }
  llvm_unreachable("Parent block doesn't contain instruction.");
}

/// Find an argument of type OSLogMessage from the given os log call.
static SILValue findOSLogMessageArgument(ApplyInst *oslogCall) {
  ASTContext &astContext = oslogCall->getFunction()->getASTContext();

  for (auto argument : oslogCall->getArguments()) {
    SILType argumentType = argument->getType();
    auto *structDecl = argumentType.getStructOrBoundGenericStruct();
    if (!structDecl) {
      continue;
    }

    if (structDecl->getName() == astContext.Id_OSLogMessage) {
      return argument;
    }
  }
  llvm_unreachable("No argument of type OSLogMessage in os log call");
}

/// A utility function for inlining the apply instruction into the caller.
/// TODO: should this function be moved to the SILInliner utility?
std::pair<SILBasicBlock::iterator, SILBasicBlock *> static inlineFunction(
    ApplyInst *applyInst, SILFunction *caller,
    SILFunctionTransform &transform) {
  FullApplySite applySite = FullApplySite(applyInst);

  SILOpenedArchetypesTracker OpenedArchetypesTracker(caller);
  caller->getModule().registerDeleteNotificationHandler(
      &OpenedArchetypesTracker);
  // The callee only needs to know about opened archetypes used in
  // the substitution list.
  OpenedArchetypesTracker.registerUsedOpenedArchetypes(applyInst);
  SILOptFunctionBuilder funcBuilder(transform);
  SILInliner inliner(funcBuilder, SILInliner::InlineKind::PerformanceInline,
                     applySite.getSubstitutionMap(), OpenedArchetypesTracker);
  SmallVector<SILValue, 8> args;
  for (const auto &arg : applySite.getArguments())
    args.push_back(arg);
  return inliner.inlineFunction(applySite.getReferencedFunction(), applySite,
                                args);
}

void OSLogOptimization::optimizeOSLogCall(ApplyInst *oslogCall,
                                          SILFunction *caller) {
  // Find the range of instructions that have to be analyzed in order to
  // optimize the given os log call. The relevant instructions begin with the
  // `allocStack` of the string interpolation type (namely, OSLogMessage)
  // passed to the log call and end at the last instruction of the body of the
  // called os log function (which will be inlined).
  auto *stringInterpolAllocInst =
      findOSLogMessageArgument(oslogCall)->getDefiningInstruction();
  if (!stringInterpolAllocInst ||
      !isa<AllocStackInst>(stringInterpolAllocInst)) {
    diagnose(caller->getASTContext(), oslogCall->getLoc().getSourceLoc(),
             diag::oslog_dynamic_message);
    return;
  }

  auto firstI = findIteratorForInstruction(stringInterpolAllocInst);

  // Load and link the called os log function before inlining. This is needed
  // to link shared functions that are used in the callee body.
  auto *callee = oslogCall->getReferencedFunction();
  assert(callee);

  if (callee->isExternalDeclaration()) {
    callee->getModule().loadFunction(callee);
    assert(!callee->isExternalDeclaration());
    caller->getModule().linkFunction(callee,
                                     SILModule::LinkingMode::LinkNormal);
  }

  // Inline the log call into the caller and find the last instruction of the
  // log call.
  auto lastBB = inlineFunction(oslogCall, caller, *this).second;
  auto lastI = lastBB->end();

  // Interpret the instructions from first to the last and fold all
  // compile-time constants found during interpretation.
  constantFold(firstI, lastI);
}

void OSLogOptimization::constantFold(SILBasicBlock::iterator first,
                                     SILBasicBlock::iterator last) {

  SILInstruction *firstInst = &(*first);

  // A mapping from SILValues to their constant values.
  llvm::SmallMapVector<SingleValueInstruction *, SymbolicValue, 4> constantMap;

  // An allocator to store symbolic values constructed during interpretation.
  SymbolicValueBumpAllocator allocator;
  ConstExprStepEvaluator constantEvaluator(allocator, firstInst->getFunction());

  auto errorInfo =
      collectConstants(first, last, constantMap, constantEvaluator);

  // At this point, the `OSLogMessage` instance should be mapped to a symbolic
  // value in the interpreter state. Furthermore, its format string and
  // interger-valued fields (other than `OSLogArguments`) must be constants.
  // If this is not the case, it means the formatting options or privacy
  // qualifiers provided by the user were not inferred as compile-time
  // constants. Detect and diagnose this scenario.
  assert(isa<SingleValueInstruction>(firstInst));
  bool errorDetected = detectAndDiagnoseErrors(
      errorInfo, dyn_cast<SingleValueInstruction>(firstInst),
      constantEvaluator);
  if (errorDetected) {
    return;
  }
  // The necessary string information must have been initialized here.
  assert(stringInitIntrinsic && stringMetatype);

  substituteConstants(constantMap);
}

/// Return true if and only if the given nominal type declaration is that of
/// a stdlib Int or stdlib Bool.
static bool isStdlibIntegerOrBoolDecl(NominalTypeDecl *numberDecl,
                                      ASTContext &astCtx) {
  return (numberDecl == astCtx.getIntDecl() ||
          numberDecl == astCtx.getInt8Decl() ||
          numberDecl == astCtx.getInt16Decl() ||
          numberDecl == astCtx.getInt32Decl() ||
          numberDecl == astCtx.getInt64Decl() ||
          numberDecl == astCtx.getUIntDecl() ||
          numberDecl == astCtx.getUInt8Decl() ||
          numberDecl == astCtx.getUInt16Decl() ||
          numberDecl == astCtx.getUInt32Decl() ||
          numberDecl == astCtx.getUInt64Decl() ||
          numberDecl == astCtx.getBoolDecl());
}

/// Return true if and only if the given SIL type represents a String or
/// a Stdlib or builtin integer type.
static bool isIntegerOrStringType(SILType silType, ASTContext &astContext,
                                  bool excludeSILAddresses = false) {
  if (excludeSILAddresses && silType.isAddress()) {
    return false;
  }

  if (silType.is<BuiltinIntegerType>()) {
    return true;
  }

  auto *nominalDecl = silType.getNominalOrBoundGenericNominal();
  if (!nominalDecl) {
    return false;
  }

  return (nominalDecl == astContext.getStringDecl()) ||
         isStdlibIntegerOrBoolDecl(nominalDecl, astContext);
}

/// If the given instruction is a call to the compiler-intrinsic initializer
/// of String that accepts string literals, return the called function.
/// Otherwise, return nullptr.
static SILFunction *getStringMakeUTF8Init(SILInstruction *inst) {
  auto *apply = dyn_cast<ApplyInst>(inst);
  if (!apply)
    return nullptr;

  auto *callee = apply->getCalleeFunction();
  if (!callee || !callee->hasSemanticsAttr("string.makeUTF8"))
    return nullptr;
  return callee;
}

/// Decide if the given instruction (which could possibly be a call) should
/// be constant evaluated.
///
/// \returns true iff the given instruction is not a call or if it is, it calls
/// a known stdlib function that the interpreter supports or calls a os_log
/// overlay function annotated with a semantics attribute.
static bool shouldAttemptEvaluation(SILInstruction *inst,
                                    ConstExprStepEvaluator &stepEval) {
  auto *apply = dyn_cast<ApplyInst>(inst);
  if (!apply)
    return true;

  auto calleeFun = apply->getCalleeFunction();
  if (!calleeFun)
    return false;

  return stepEval.isKnownFunction(calleeFun) ||
         calleeFun->hasSemanticsAttrThatStartsWith("oslog");
}

/// Skip or evaluate the given instruction based on the evaluation policy and
/// handle errors. The policy is to evaluate all non-apply instructions as well
/// as apply instructions that either invoke a known stdlib function or an os
/// log specific function that constructs compile-time constants
/// (like format string). Every other function call is skipped.
/// This includes calls that manipulate runtime values such as the arguments
/// (i.e, interpolated expressions) or the raw byte buffer.
static std::pair<Optional<SILBasicBlock::iterator>, Optional<SymbolicValue>>
evaluateOrSkip(ConstExprStepEvaluator &stepEval,
               SILBasicBlock::iterator instI) {
  auto *inst = &(*instI);

  // Note that skipping a call conservatively approximates its effects on the
  // interpreter state.
  if (shouldAttemptEvaluation(inst, stepEval)) {
    return stepEval.tryEvaluateOrElseSkip(instI);
  }
  return stepEval.skip(instI);
}

Optional<SymbolicValue> OSLogOptimization::collectConstants(
    SILBasicBlock::iterator first, SILBasicBlock::iterator last,
    llvm::SmallMapVector<SingleValueInstruction *, SymbolicValue, 4>
        &constantMap,
    ConstExprStepEvaluator &constantEvaluator) {

  SILFunction *fn = (*first).getFunction();
  ASTContext &astContext = fn->getASTContext();

  for (auto currI = first; currI != last;) {
    auto *currInst = &(*currI);

    // Initialize cache from this instruction if possible.
    extractStringInfoFromInstruction(currInst);

    Optional<SymbolicValue> errorInfo = None;
    Optional<SILBasicBlock::iterator> nextI = None;

    std::tie(nextI, errorInfo) = evaluateOrSkip(constantEvaluator, currI);
    if (!nextI) {
      // If evaluation cannot be continued, return the error.
      return errorInfo;
    }
    // Set the next instruction to continue evaluation from.
    currI = nextI.getValue();

    // If we found a constant value for the result of the instruction,
    // record it.
    auto *singleValInst = dyn_cast<SingleValueInstruction>(currInst);
    if (!singleValInst) {
      continue;
    }

    auto constantVal = constantEvaluator.lookupConstValue(singleValInst);
    if (!constantVal.hasValue()) {
      continue;
    }

    // Track constant strings and integer values only. Do not include
    // SIL address types as it suffices to fold the values written into the
    // addresses.
    if (!isIntegerOrStringType(singleValInst->getType(), astContext,
                               /*excludeSILAddresses*/ true)) {
      continue;
    }

    // Do not track literal instructions as they are already folded. Also do
    // not track 'StructInst' as we can only fold their arguments and not the
    // instruction itself. Do not track string makeUTF8 initializers also.
    if (isa<LiteralInst>(singleValInst) || isa<StructInst>(singleValInst) ||
        getStringMakeUTF8Init(singleValInst)) {
      continue;
    }

    // Here we have either a String decl or a Stdlib integer decl or a
    // builtin integer type.
    constantMap.insert({singleValInst, constantVal.getValue()});
  }
  return None; // No error.
}

void OSLogOptimization::extractStringInfoFromInstruction(SILInstruction *inst) {
  // If the cache is already initialized do nothing.
  if (stringInitIntrinsic)
    return;

  auto *callee = getStringMakeUTF8Init(inst);
  if (!callee)
    return;

  this->stringInitIntrinsic = callee;

  auto *stringMetatypeInst =
      dyn_cast<MetatypeInst>(inst->getOperand(4)->getDefiningInstruction());
  this->stringMetatype = stringMetatypeInst->getType();
}

void OSLogOptimization::substituteConstants(
    llvm::SmallMapVector<SingleValueInstruction *, SymbolicValue, 4>
        &constantMap) {
  SmallVector<SILInstruction *, 4> deletedInsts;
  for (auto &kv : constantMap) {
    auto *inst = kv.first;
    auto constantVal = kv.second;

    SILBuilderWithScope builder(inst);
    auto loc = inst->getLoc();
    auto instType = inst->getType();
    auto newSILVal =
        emitCodeForSymbolicValue(constantVal, instType, builder, loc);

    inst->replaceAllUsesWith(newSILVal);
    deletedInsts.push_back(inst);
  }

  recursivelyDeleteTriviallyDeadInstructions(deletedInsts, true,
                                             [&](SILInstruction *DeadI) {});
}

SILValue OSLogOptimization::emitCodeForSymbolicValue(SymbolicValue symVal,
                                                     SILType &expectedType,
                                                     SILBuilder &builder,
                                                     SILLocation &loc) {
  auto &astContext = expectedType.getASTContext();

  switch (symVal.getKind()) {
  case SymbolicValue::String: {
    assert(astContext.getStringDecl() ==
           expectedType.getNominalOrBoundGenericNominal());

    auto stringVal = symVal.getStringValue();
    auto *stringLitInst = builder.createStringLiteral(
        loc, stringVal, StringLiteralInst::Encoding::UTF8);

    // Create a builtin word for the size of the string
    auto *sizeInst = builder.createIntegerLiteral(
        loc, SILType::getBuiltinWordType(astContext), stringVal.size());
    // Set isAscii to false.
    auto *isAscii = builder.createIntegerLiteral(
        loc, SILType::getBuiltinIntegerType(1, astContext), 0);
    // Create a metatype inst.
    auto *metatypeInst = builder.createMetatype(loc, stringMetatype.getValue());

    auto args = SmallVector<SILValue, 4>();
    args.push_back(stringLitInst);
    args.push_back(sizeInst);
    args.push_back(isAscii);
    args.push_back(metatypeInst);

    auto *stringInitRef = builder.createFunctionRef(loc, stringInitIntrinsic);
    auto *applyInst = builder.createApply(loc, stringInitRef, SubstitutionMap(),
                                          ArrayRef<SILValue>(args), false);
    return applyInst;
  }
  case SymbolicValue::Integer: { // Builtin integer types.
    APInt resInt = symVal.getIntegerValue();
    assert(expectedType.is<BuiltinIntegerType>());

    auto *intLiteralInst =
        builder.createIntegerLiteral(loc, expectedType, resInt);
    return intLiteralInst;
  }
  case SymbolicValue::Aggregate: {
    // Support only stdlib integer or bool structs.
    auto *structDecl = expectedType.getStructOrBoundGenericStruct();
    assert(structDecl);
    assert(isStdlibIntegerOrBoolDecl(structDecl, astContext));

    VarDecl *propertyDecl = structDecl->getStoredProperties().front();
    auto propertyType =
        expectedType.getFieldType(propertyDecl, builder.getModule());
    auto propertyVal = symVal.lookThroughSingleElementAggregates();
    auto newPropertySIL =
        emitCodeForSymbolicValue(propertyVal, propertyType, builder, loc);
    auto *newStructInst = builder.createStruct(
        loc, expectedType, ArrayRef<SILValue>(newPropertySIL));
    return newStructInst;
  }
  default: {
    assert(false && "Symbolic value kind is not supported");
  }
  }
}

bool OSLogOptimization::detectAndDiagnoseErrors(
    Optional<SymbolicValue> errorInfo, SingleValueInstruction *osLogMessageAddr,
    ConstExprStepEvaluator &constantEvaluator) {

  auto loc = osLogMessageAddr->getLoc();
  auto sourceLoc = loc.getSourceLoc();
  auto *fn = osLogMessageAddr->getFunction();
  auto &module = fn->getModule();
  auto &astContext = fn->getASTContext();
  bool errorDetected = false;

  // If we have errorInfo that indicates a fail-stop error, diagnose it.
  if (errorInfo && constantEvaluator.isFailStopError(*errorInfo)) {
    assert(errorInfo->getKind() == SymbolicValue::Unknown);
    diagnose(astContext, sourceLoc, diag::oslog_const_evaluation_error);
    errorInfo->emitUnknownDiagnosticNotes(loc);
    errorDetected = true;
  }

  // Check if the OSLogMessage and OSLogInterpolation instances are correctly
  // inferred as a constants. If not, it implies incorrect implementation
  // of the os log API in the overlay. Diagnostics here are for os log
  // library authors.
  auto osLogMessageAddrValueOpt =
      constantEvaluator.lookupConstValue(osLogMessageAddr);
  assert(osLogMessageAddrValueOpt.hasValue() &&
         osLogMessageAddrValueOpt->getKind() == SymbolicValue::Address);

  SmallVector<unsigned, 2> accessPath;
  SymbolicValue osLogMessageValue =
      osLogMessageAddrValueOpt->getAddressValue(accessPath)->getValue();
  if (!osLogMessageValue.isConstant()) {
    diagnose(astContext, sourceLoc, diag::oslog_non_constant_message);
    return true;
  }

  auto osLogInterpolationValue =
      osLogMessageValue.lookThroughSingleElementAggregates();
  if (!osLogInterpolationValue.isConstant()) {
    diagnose(astContext, sourceLoc, diag::oslog_non_constant_interpolation);
    return true;
  }

  // Check if every proprety of the OSLogInterpolation instance that is a
  // string or integer has a constant value. If this is violated this could
  // be an indication of an error in the usage of the API. Diagnostics emitted
  // here are for the users of the os log APIs.
  SILType osLogMessageType = osLogMessageAddr->getType();
  auto *structDecl = osLogMessageType.getStructOrBoundGenericStruct();
  assert(structDecl);

  VarDecl *interpolationPropDecl = structDecl->getStoredProperties().front();
  SILType osLogInterpolationType =
      osLogMessageType.getFieldType(interpolationPropDecl, module);
  auto *interpolationStruct =
      osLogInterpolationType.getStructOrBoundGenericStruct();
  assert(interpolationStruct);

  auto propertyDecls = interpolationStruct->getStoredProperties();
  auto propertyValues = osLogInterpolationValue.getAggregateValue();
  auto propValueI = propertyValues.begin();

  for (auto *propDecl : propertyDecls) {
    SymbolicValue propertyValue = *(propValueI++);
    if (propertyValue.isConstant()) {
      continue;
    }

    if (!isIntegerOrStringType(
            osLogInterpolationType.getFieldType(propDecl, module),
            astContext)) {
      continue;
    }

    diagnose(astContext, sourceLoc, diag::oslog_property_not_constant,
             propDecl->getNameStr());
    errorDetected = true;
    break;
  }
  return errorDetected;
}

/// If the SILInstruction is a call to an os log function, return the call
/// as an in ApplyInst. Otherwise, return nullptr.
static ApplyInst *getAsOSLogCall(SILInstruction *inst) {
  auto *applyInst = dyn_cast<ApplyInst>(inst);
  if (!applyInst) {
    return nullptr;
  }

  SILFunction *callee = applyInst->getCalleeFunction();
  if (!callee || !callee->hasSemanticsAttrThatStartsWith("oslog.log")) {
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

void OSLogOptimization::run() {
  // Don't rerun optimization on deserialized functions or stdlib function.
  if (getFunction()->wasDeserializedCanonical()) {
    return;
  }

  auto &fun = *getFunction();

  // Collect all os log calls in the function.
  SmallVector<ApplyInst *, 4> oslogCalls;
  for (auto &bb : fun) {
    for (auto &inst : bb) {
      auto oslogCall = getAsOSLogCall(&inst);
      if (!oslogCall)
        continue;
      oslogCalls.push_back(oslogCall);
    }
  }

  // Optimize each os log call found. Optimizing a call will change the
  // function body by inlining functions and folding constants.
  for (auto *oslogCall : oslogCalls) {
    optimizeOSLogCall(oslogCall, &fun);
  }
}

} // end anonymous namespace

SILTransform *swift::createOSLogOptimization() {
  return new OSLogOptimization();
}
