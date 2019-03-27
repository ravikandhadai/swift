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

#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/DiagnosticsSema.h"
#include "swift/AST/DiagnosticsSIL.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Stmt.h"
#include "swift/AST/Type.h"
#include "swift/AST/SubstitutionMap.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILLocation.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/SILVisitor.h"
#include "swift/SIL/SILConstants.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILBasicBlock.h"
#include "swift/SILOptimizer/Utils/Local.h"
#include "swift/SILOptimizer/Utils/SILOptFunctionBuilder.h"
#include "swift/SILOptimizer/Utils/SILInliner.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/ConstExpr.h"
#include "swift/Demangling/Demangler.h"
#include "swift/AST/Module.h"
#include "swift/SIL/InstructionUtils.h"
#include <queue>

using namespace swift;

template<typename...T, typename...U>
static void diagnose(ASTContext &Context, SourceLoc loc, Diag<T...> diag,
              U &&...args) {
  Context.Diags.diagnose(loc, diag, std::forward<U>(args)...);
}

namespace {

SILBasicBlock::iterator findIteratorForInstruction(SILInstruction *keyInst) {
  auto *bb = keyInst->getParent();
  for (auto currI = bb->begin(), endI = bb->end(); currI != endI; currI++) {
    if (keyInst == &(*currI)) {
      return currI;
    }
  }
  llvm_unreachable("Parent block doesn't contain instruction.");
}

/// A utility function for inlining the apply instruction into the caller.
/// TODO: move some of the boiler plate code here could be moved into the
/// SILInliner utility.
std::pair<SILBasicBlock::iterator, SILBasicBlock *>
inlineFunction(ApplyInst *applyInst, SILFunction &caller,
               SILFunctionTransform &transform) {
  FullApplySite applySite = FullApplySite(applyInst);

  SILOpenedArchetypesTracker OpenedArchetypesTracker(&caller);
  caller.getModule().registerDeleteNotificationHandler(
                                            &OpenedArchetypesTracker);
  // The callee only needs to know about opened archetypes used in
  // the substitution list.
  OpenedArchetypesTracker.registerUsedOpenedArchetypes(applyInst);
  SILOptFunctionBuilder funcBuilder(transform);
  SILInliner inliner(funcBuilder,
                     SILInliner::InlineKind::PerformanceInline,
                     applySite.getSubstitutionMap(),
                     OpenedArchetypesTracker);
  SmallVector<SILValue, 8> args;
  for (const auto &arg : applySite.getArguments())
    args.push_back(arg);
  return inliner.inlineFunction(applySite.getReferencedFunction(),
                                applySite, args);
}

/// Skips or evaluates the instruction based on the policy and handles errors.
Optional<SILBasicBlock::iterator>
evaluateOrSkip(ConstExprStepEvaluator &stepEval,
               SILBasicBlock::iterator instI) {
  auto inst = &(*instI);

  // TODO: ensure that we invalidate any mutable state whenever we skip
  // an instruction or if the instruction cannot be evaluated.

  // If this is a call to apply, interpret this only if this is a
  // compiler-evaluable function or a well-known function (which is like
  // a primitve operation). For every non-apply instruction, if we can't
  // evaluate this flow-sensitively, ignore this and keep going.
  Optional<SymbolicValue> errorVal = None;
  Optional<SILBasicBlock::iterator> nextI = None;

  if (auto *apply = dyn_cast<ApplyInst>(inst)) {
    auto calleeFun = apply->getCalleeFunction();
    if (!calleeFun || (!stepEval.isKnownPrimitive(calleeFun) &&
        !calleeFun->hasSemanticsAttrThatStartsWith("oslog"))) {
      if (isa<TermInst>(inst)) {
        // We do not know the target branch if we skipped this call which is
        // a terminal instruction.
        return None;
      }
      llvm::errs() << "Skipping call: " << *inst << "\n";
      return ++instI;
    }
    if (calleeFun->hasSemanticsAttrThatStartsWith("oslog")) {
      llvm::errs() << "Evaluating oslog call: " << *inst << "\n";
    }
  }

  // Evaluation of this function bound not to fail.
  std::tie(nextI, errorVal) = stepEval.stepOver(instI);
  if (errorVal.hasValue()){
    llvm::errs() << "Interpretation of " << *instI << "failed: " << errorVal.getValue() << "\n";
  }

  if (!nextI.hasValue()) {
    if (isa<TermInst>(inst)) {
      return None;
    }
    return ++instI;
  }
  return nextI;
}

/// This function generates SIL for converting a symbolic value into an
/// instance of the provided SILType. The code only handles those conversions
/// that are needed by the OSLogOptimization pass. Other symbolic value kinds
/// shall be added incrementally as and when they are needed.
SILValue generateCode(SymbolicValue symVal,
                      SILType &expectedType,
                      SILBuilder &builder,
                      SILLocation &loc,
                      llvm::Optional<ApplyInst *> stringUTF8InitCall) {
  auto &astContext = expectedType.getASTContext();

  switch (symVal.getKind()) {
  case SymbolicValue::String: {
    assert(astContext.getStringDecl() ==
           expectedType.getASTType()->getAnyNominal());
    assert(stringUTF8InitCall.hasValue());

    auto stringVal = symVal.getStringValue(); // Note that this is a utf8 string.
    auto *stringLitInst =
      builder.createStringLiteral(loc, stringVal,
                                  StringLiteralInst::Encoding::UTF8);

    // Create a builtin word for the size of the string
    auto *sizeInst =
      builder.createIntegerLiteral(loc, SILType::getBuiltinWordType(astContext),
                                   stringVal.size());
    // Set isAscii to false.
    auto *isAscii =
      builder.createIntegerLiteral(loc,
                                   SILType::getBuiltinIntegerType(1, astContext),
                                   0);
    auto *initCall = stringUTF8InitCall.getValue();

    auto args  = SmallVector<SILValue, 4>();
    args.push_back(stringLitInst);
    args.push_back(sizeInst);
    args.push_back(isAscii);
    args.push_back(initCall->getOperand(4));

    auto *applyInst = builder.createApply(loc, initCall->getOperand(0),
                                    SubstitutionMap(),
                                    ArrayRef<SILValue>(args), false);
    return applyInst;
  }
  default: {
    assert(false &&
           "Symbolic value other than strings are not supported");
  }
  }
}

  
class OSLogOptimization : public SILFunctionTransform {
  
  // Tracks a stringUTF8Call instruction. This is a very hacky way
  // of getting a reference to String.makeUTF8 initializer.
  // TODO: fix this.
  ApplyInst *stringUTF8InitCall = nullptr;
  
  ~OSLogOptimization() override {}

  /// Search for loads of packedMsgArg fields that are constants and
  /// replace them with their values.
  bool fold(ConstExprStepEvaluator &stepEval, SILBasicBlock::iterator startInst,
            SILBasicBlock *lastBB) {
    auto &astCtx = getFunction()->getASTContext();
    
    // A mapping from SILValue to its constant value, if any.
    llvm::SmallMapVector<SingleValueInstruction *, SymbolicValue, 4> constantMap;

    for(auto currI = startInst, endI = lastBB->end(); currI != endI; ) {
      auto *currInst = &(*currI);

      // TODO: make sure we invalidate non-constant state during interpretation.

      auto nextI = evaluateOrSkip(stepEval, currI);
      if (!nextI) {
        break;
      }
      currI = nextI.getValue();

      // If we found a constant value for the result of the instruction,
      // track it, if it could be folded.
      auto *singleValInst = dyn_cast<SingleValueInstruction>(currInst);
      if (!singleValInst)
        continue;

      auto symVal = stepEval.lookupConstValue(singleValInst);
      if (!symVal.hasValue())
        continue;

      auto silType = singleValInst->getType();
      // Track constant string values.
      if (!silType.isAddress() &&
          astCtx.getStringDecl() == silType.getASTType()->getAnyNominal()) {
        constantMap.insert({ singleValInst, symVal.getValue() });
      }
    }

    // Replace the instructions we have found and collect folded instructions
    // for deletion.
    SmallVector<SILInstruction *, 4> deletedInsts;
    for (auto &kv : constantMap) {
      auto *inst = kv.first;
      auto symVal = kv.second;

      // Generate code for the string literal.
      SILBuilderWithScope builder(inst);
      auto loc = inst->getLoc();
      auto instType = inst->getType();
      auto newSILVal = generateCode(symVal, instType, builder, loc,
                                    this->stringUTF8InitCall);

      // for debugging
      llvm::errs() << "Folding Inst: "<< *inst << " with value: ";
      symVal.print(llvm::errs());
      llvm::errs() << "\n";
      llvm::errs() << "New SILValue: " << newSILVal << "\n";

      // fold using the new value and record deleted instructions
      inst->replaceAllUsesWith(newSILVal);
      deletedInsts.push_back(inst);
      // TODO: remove calls to getIntegerFormatSpecifier.
    }
    
    recursivelyDeleteTriviallyDeadInstructions(deletedInsts, true,
                                          [&](SILInstruction *DeadI) {
                                          });
    return !deletedInsts.empty();
  }

  llvm::Optional<SymbolicValue>
  partiallyEvaluate(ConstExprStepEvaluator &stepEval,
                    SILBasicBlock::iterator startIter, SILInstruction *endInst,
                    SILValue keyOperand) {
    auto currI = startIter;
    while(&(*currI) != endInst) {
      auto inst = &(*currI);

      // If we see a control-flow instruction return an error
      if (isa<ReturnInst>(inst)) {
        llvm::errs() << " Unexpected instruction found: " << inst << "\n";
        return None;
      }

      // TODO: is this needed?
      if (!stringUTF8InitCall) {
        if (auto *apply = dyn_cast<ApplyInst>(inst)) {
          if (auto calleeFun = apply->getCalleeFunction()) {
            if (calleeFun->hasSemanticsAttr("string.makeUTF8")) {
              stringUTF8InitCall = apply;
            }
          }
        }
      }

      // Evaluation of this function bound to not fail.
      auto nextI = evaluateOrSkip(stepEval, currI);
      if (!nextI.hasValue()) {
        // We don't have a next instruction and we haven't seen the end
        // instruction.
        return None;
      }
      currI = nextI.getValue();
    }
    auto symValOpt = stepEval.lookupConstValue(keyOperand);
    return symValOpt;
  }

  void optimizeOSLogCall(ConstExprStepEvaluator &stepEval,
                         ApplyInst *applyInst) {
    // Load and link the callee before inlining. This is needed to link
    // (shared) functions that are used in the callee body.
    auto &caller = *getFunction();
    auto *callee = applyInst->getReferencedFunction();
    assert(callee);

    if (callee->isExternalDeclaration()) {
      callee->getModule().loadFunction(callee);
      assert(!callee->isExternalDeclaration());
      caller.getModule().linkFunction(callee,
                                      SILModule::LinkingMode::LinkNormal);
    }

    // Inline the log call into the caller
    SILBasicBlock::iterator firstInlinedInst;
    SILBasicBlock *lastBB;
    std::tie(firstInlinedInst, lastBB) =
      inlineFunction(applyInst, caller, *this);

    // Constant-fold simple constants in the body of the log method.
    fold(stepEval, firstInlinedInst, lastBB);
  }

  /// The entry point to the transformation.
  void run() override {
    // Don't rerun diagnostics on deserialized functions or stdlib function.
    if (getFunction()->wasDeserializedCanonical() ||
        !getFunction()->hasSemanticsAttr("run.oslog.optimization"))
      return;

    auto fname = demangleSymbolAsString(getFunction()->getName(),
                    Demangle::DemangleOptions::SimplifiedUIDemangleOptions());
    llvm::errs() << "Running the OSLogOptimzation phase on " << fname << "\n";

    auto &fun = *getFunction();
    auto deletedInsts = SmallVector<SILInstruction *, 10>();

    for (auto &bb : fun) {
      for (auto &inst : bb) {
        // Check if this is a call to "oslog".
        auto *applyInst = dyn_cast<ApplyInst>(&inst);
        if (!applyInst || !applyInst->getCalleeFunction()
            || !applyInst->getCalleeFunction()
                  ->hasSemanticsAttrThatStartsWith("oslog.log"))
          continue;
        llvm::errs() << "Found call to oslog " << inst << "\n";

        // Extract the second operand to the call. This is the value that
        // needs to be statically computed.
        SILValue oslogMessageArg = applyInst->getArgument(1);
        llvm::errs() << "Res operand: " << oslogMessageArg << "\n";

        auto allocStackIter =
          findIteratorForInstruction(oslogMessageArg->getDefiningInstruction());
        llvm::errs() << "Starting interpretation from: " << *allocStackIter << "\n";

        // Create a step evaluator.
        llvm::BumpPtrAllocator bumpAllocator;
        ConstExprEvaluator evaluator(
                   [&](unsigned long bytes, unsigned alignment) {
                     return bumpAllocator.Allocate(bytes, alignment);
                   });
        ConstExprStepEvaluator stepEval(evaluator, &fun);

        // Partially evaluate and compute the format string using the step
        // evaluator.
        auto resValue = partiallyEvaluate(stepEval, allocStackIter, applyInst,
                                          oslogMessageArg);
        if (!resValue.hasValue()) {
          llvm::errs() << "Interpretation failed!! \n";
        }

        // for debugging
        auto oslogMessageValue = resValue.getValue();
        SymbolicValue printableValue = oslogMessageValue;
        if (oslogMessageValue.getKind() == SymbolicValue::Address) {
          SmallVector<unsigned, 2> accessPath;
          printableValue = oslogMessageValue.getAddressValue(accessPath)->getValue();
        }
        llvm::errs() << "Result value for " <<  oslogMessageArg  <<  ": \n";
        printableValue.dump();
        llvm::errs() << "\n";

        // Optimize the body of log function using the step evaluator.
        optimizeOSLogCall(stepEval, applyInst);
        break;
      }
    }
    llvm::errs() << "New function body: \n";
    fun.dump();
    llvm::errs() << "\n";
  }
};
} // end anonymous namespace

SILTransform *swift::createOSLogOptimization() {
  return new OSLogOptimization();
}
