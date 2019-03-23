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

// TODO: make sure that we do not over-slice esp when we have autoclosures.
// The definitions of the captured arguments need not be pulled in.
// For now this is not used!
Optional<std::pair<SILBasicBlock::iterator, SILBasicBlock::iterator>>
  getStringInterpolationInstrs(SILInstruction *first, SILInstruction *last,
                               SILFunction &fun) {
  assert(first != last);
//  // Try to get the start of the interpolation, which is an AllocStack
//  // of PackedOSLogMessage by going backward.
//  SILInstruction *startInstr = nullptr;
//
//  auto queue = std::queue<SILInstruction*>();
//  queue.push(seed);
//
//  while (!queue.empty()) {
//    auto currInst = queue.front();
//    queue.pop();
//
//    // If we have an alloc-stack instruction, check if it is the start of the
//    // interpolation.
//    if (auto *allocInst = dyn_cast<AllocStackInst>(currInst)) {
//      auto varDecl = allocInst->getDecl();
//      llvm::errs() << "Declname: " << varDecl->getNameStr() << "\n";
//      // This is an string interpolation construction.
//      if (varDecl && varDecl->getNameStr().equals("$interpolation")) {
//        startInstr = allocInst;
//        break;
//      }
//    }
//
//    for (auto &operand : currInst->getAllOperands()) {
//      if (auto *valueI = operand.get()->getDefiningInstruction()) {
//        queue.push(valueI);
//      }
//    }
//  }
//
//  if (!startInstr) {
//    llvm::errs() << "Couldn't find start instruction of osLog \n";
//    return None;
//  }

  // Collect all instructions from the start instruction down to the seed inst.
  SILBasicBlock::iterator startI;
  SILBasicBlock::iterator endI;

  llvm::errs() << "Start inst" << *first  << "\n";
  llvm::errs() << "End inst" << *last  << "\n";

  for (auto &bb : fun) {
    for (auto instI = bb.begin(); instI != bb.end(); instI++) {
      auto *Iptr = &(*instI);

      if (Iptr == first) {
        startI = instI;
      }
      if (Iptr == last) {
        endI = instI;
        break;
      }
    }
  }
  std::pair<SILBasicBlock::iterator, SILBasicBlock::iterator> res =
    { startI, endI };
  return res;
}

void dumpInstructions(SILBasicBlock::iterator start,
                      SILBasicBlock::iterator end) {
//  llvm::errs() << "Size: " << std::distance(end, start) << "\n";
  for (auto currInst = start; currInst != end; currInst++) {
    llvm::errs() << *currInst << "\n";
  }
}

class SymbolicValueCodeGenerator {
  SILBuilder &B;
  SILLocation &Loc;
  ASTContext &astCtx;
  SILModule &M;
  llvm::Optional<ApplyInst *> stringUTF8InitCall;

public:
  SymbolicValueCodeGenerator(SILBuilder &builder, ASTContext &astctx,
                             SILLocation &loc, SILModule &mod,
                             llvm::Optional<ApplyInst *> stringInit = None)
  : B(builder), Loc(loc), astCtx(astctx), M(mod),
    stringUTF8InitCall(stringInit) {
  }

  SILValue generateCode(SymbolicValue symVal, SILType &expectedType);
};

/// This function generates SIL for converting a symbolic value into an instance
/// of the provided SILType. The code only handles those conversions that are
/// needed for os_log implementation.
/// Note that the symbolic value do not store a type.
/// Therefore, we need to pass in the expectedType of the result to
/// construct appropriate code.
SILValue SymbolicValueCodeGenerator::generateCode(SymbolicValue symVal,
                                    SILType &expectedType) {
  switch (symVal.getKind()) {
  case SymbolicValue::String: {
    assert(astCtx.getStringDecl() == expectedType.getASTType()->getAnyNominal());
    assert(stringUTF8InitCall.hasValue());
    
    auto stringVal = symVal.getStringValue(); // Note: this is a utf8 string.
    auto *stringLitInst = B.createStringLiteral(Loc, stringVal,
                                            StringLiteralInst::Encoding::UTF8);
    
    // create a builtin word for the size of the string
    auto *sizeInst = B.createIntegerLiteral(Loc,
                                            SILType::getBuiltinWordType(astCtx),
                                            stringVal.size());
    auto *isAscii = B.createIntegerLiteral(Loc,
                                SILType::getBuiltinIntegerType(1, astCtx),
                                          0); // Set to false.
    
    auto *initCall = stringUTF8InitCall.getValue();
    
    auto args  = SmallVector<SILValue, 2>();
    args.push_back(stringLitInst);
    args.push_back(sizeInst);
    args.push_back(isAscii);
    args.push_back(initCall->getOperand(4));
    
    auto *applyInst = B.createApply(Loc, initCall->getOperand(0),
                                    SubstitutionMap(),
                                    ArrayRef<SILValue>(args), false);
    return applyInst;
  }
  case SymbolicValue::Integer: {
    // create a swift Integer struct of appropriate bit-width
    APInt resInt = symVal.getIntegerValue();
    assert(expectedType.getASTType()->is<BuiltinIntegerType>());

    auto *intLitInst = B.createIntegerLiteral(Loc, expectedType, resInt);
    return intLitInst;
  }
  case SymbolicValue::EnumWithPayload: {
    auto *enumDecl = expectedType.getEnumOrBoundGenericEnum();
    assert(enumDecl);
    
    // generate code for the payload
    auto enumPayload = symVal.getEnumPayloadValue();
    auto *enumElemDecl = symVal.getEnumValue();
    auto elemType = expectedType.getEnumElementType(enumElemDecl, M);
    auto payloadSILVal = generateCode(enumPayload, elemType);
   
    
    auto *enumInst = B.createEnum(Loc, payloadSILVal, enumElemDecl,
                                  expectedType);
    return enumInst;
  }
  case SymbolicValue::Aggregate: {
    // Recursively get the types of the fields.
    auto *structDecl = expectedType.getStructOrBoundGenericStruct();
    assert(structDecl);

    auto aggVal = symVal.getAggregateValue();
    auto aggValIter = aggVal.begin();
    SmallVector<SILValue, 3> newopds;

    for (auto *fieldDecl: structDecl->getStoredProperties()) {
      if (auto *field = dyn_cast<VarDecl>(fieldDecl)) {
        auto fieldType = expectedType.getFieldType(fieldDecl, M);
        newopds.push_back(generateCode(*aggValIter, fieldType));
        ++aggValIter;
      }
    }
    auto *structInst = B.createStruct(Loc, expectedType, newopds);
    return structInst;
  }
  default: {
    llvm::errs() << "Unsupported symbolic value kind: \n";
    symVal.print(llvm::errs());
    llvm::errs() << "\n";
    assert(false &&
           "Given symbolic value kind not yet supported\n");
  }
  }
}

static bool isConcreteNumberDecl(NominalTypeDecl * numberDecl,
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
          numberDecl == astCtx.getDoubleDecl() ||
          numberDecl == astCtx.getFloatDecl() ||
          numberDecl == astCtx.getFloat80Decl() ||
          numberDecl == astCtx.getBoolDecl());
}
  
class OSLogOptimization : public SILFunctionTransform {
  
  // Tracks a stringUTF8Call instruction. This is a very hacky way
  // of getting a reference to String.makeUTF8 initializer.
  // TODO: fix this.
  ApplyInst *stringUTF8InitCall = nullptr;
  
  ~OSLogOptimization() override {}

  /// A utility function for inlining the apply instruction into the caller.
  std::pair<SILBasicBlock::iterator, SILBasicBlock *>
  inlineFunction(ApplyInst *applyInst, SILFunction &caller) {
    // Inline the instructions
    FullApplySite applySite = FullApplySite(applyInst);
    
    SILOpenedArchetypesTracker OpenedArchetypesTracker(&caller);
    caller.getModule().registerDeleteNotificationHandler(
                                                    &OpenedArchetypesTracker);
    // The callee only needs to know about opened archetypes used in
    // the substitution list.
    OpenedArchetypesTracker.registerUsedOpenedArchetypes(applyInst);
    SILOptFunctionBuilder funcBuilder(*this);
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
  
  void getAllTransitiveUses(SingleValueInstruction *inst,
                            SmallVectorImpl<SILInstruction *> &result) {
    auto queue = std::queue<SingleValueInstruction *>();
    queue.push(inst);
    result.push_back(inst);
    
    while (!queue.empty()) {
      auto currInst = queue.front();
      queue.pop();
      
      for (auto use : currInst->getUses()) {
        auto *user = use->getUser();
        result.push_back(user);
        
        if (auto *singValUser = dyn_cast<SingleValueInstruction>(user)) {
          queue.push(singValUser);
        }
      }
    }
  }
    
  void collectStructFieldUses(SILValue structVar, unsigned fieldNum,
                              SILFunction &fun,
                              SmallVectorImpl<SILInstruction *> &result) {
    for (auto bbI = fun.begin(); bbI != fun.end(); bbI++) {
      auto *bb = &(*bbI);
      for (auto instI = bb->begin(); instI != bb->end(); instI++) {
        auto *inst = &(*instI);
        auto *seInst = dyn_cast<StructExtractInst>(inst);
        if (!seInst || seInst->getOperand() != structVar)
          continue;
        getAllTransitiveUses(seInst, result);
      }
    }
  }
  
  /// Search for loads of packedMsgArg fields that are constants and
  /// replace them with their values.
  bool fold(SILValue packedMsgArg, SymbolicValue packedMsgValue,
            SILBasicBlock::iterator firstInlinedInst,
            SILBasicBlock *lastBB, SILFunction &fun, SILModule &module) {
    auto &astCtx = packedMsgArg->getType().getASTContext();
    
    auto packedMsgAggregate = packedMsgValue.getAggregateValue();
    
    // A mapping from SILValue to its constant value, if any.
    llvm::SmallMapVector<StructExtractInst *, SymbolicValue, 4> constantMap;
    // We can try removing all constant-valued users. Most of them will be
    // dead if they are just producing intermediate results of a constant
    // computation.
    llvm::SetVector<SILInstruction *> possiblyDeadUsers;

    // Record seen instructions to avoid going around loops.
    llvm::SetVector<SILInstruction *> seenUsers;
    seenUsers.insert(packedMsgArg->getDefiningInstruction());
    
    auto *firstInlinedBB = firstInlinedInst->getParent();
    for (auto bbI = firstInlinedBB->getIterator(); bbI != fun.end(); bbI++) {
      auto *bb = &(*bbI);
      auto instI = (bb == firstInlinedBB) ? firstInlinedInst : bb->begin();
      
      for (; instI != bb->end(); instI++) {
        auto *user = &(*instI);
        if (seenUsers.count(user))
          continue;
        seenUsers.insert(user); // Record that we have seen this user.

        auto *seInst = dyn_cast<StructExtractInst>(user);
        if (!seInst || seInst->getOperand() != packedMsgArg)
          continue;

        auto instVal = packedMsgAggregate[seInst->getFieldNo()];
        auto silType = seInst->getType();
        auto astType = silType.getASTType();
        
        // Track builtin and stdlib integer values.
        if (astType->is<BuiltinIntegerType>() ||
            isConcreteNumberDecl(astType->getAnyNominal(), astCtx)) {
          constantMap.insert({ seInst, instVal });
        }
        
        // Track constant string values.
        if (astCtx.getStringDecl() == astType->getAnyNominal()) {
          constantMap.insert({ seInst, instVal });
        }
      }
    }

    // Replace the instructions we have found and collect folded instructions
    // for deletion.
    SmallVector<SILInstruction *, 4> deletedInsts;
    for (auto &kv : constantMap) {
      auto *inst = kv.first;
      auto symVal = kv.second;

      llvm::errs() << "Folding Inst: "<< *inst << " with value: ";
      symVal.print(llvm::errs());
      llvm::errs() << "\n";

      // Build data structures needed for code generation
      SILBuilderWithScope B(inst);
      auto loc = inst->getLoc();
      SymbolicValueCodeGenerator codeGen(B, astCtx, loc, module,
                                         this->stringUTF8InitCall);

      // generate code
      auto instType = inst->getType();
      auto newSILVal = codeGen.generateCode(symVal, instType);
      
      llvm::errs() << "New SILValue: " << newSILVal << "\n";

      // fold using the new value and record deleted instructions
      inst->replaceAllUsesWith(newSILVal);
      deletedInsts.push_back(inst);
      
      // Collect all instructions of fun that references the folded fields of
      // PackedOSLogMessage and recursively delete all their uses as well.
//      collectStructFieldUses(packedMsgArg, inst->getFieldNo(), fun,
//                            deletedInsts);
    }
    
    recursivelyDeleteTriviallyDeadInstructions(deletedInsts, true,
                                          [&](SILInstruction *DeadI) {
                                          });
    return !deletedInsts.empty();
  }

  void optimizeOSLogFun(ApplyInst *applyInst, SILValue packedMsgArg,
                        SymbolicValue packedMsgValue,
                        SILFunction &caller, SILModule &M) {
    
    // (a) Inline osLog call into the caller
    SILBasicBlock::iterator firstInlinedInst;
    SILBasicBlock *lastBB;
    std::tie(firstInlinedInst, lastBB) = inlineFunction(applyInst, caller);
    
    // (b) fold simple constants in the body of oslog
    fold(packedMsgArg, packedMsgValue, firstInlinedInst, lastBB, caller, M);
  }

  llvm::Optional<SymbolicValue>
  partiallyEvaluate(ConstExprEvaluator &constEval, SILFunction *fun,
                    SILBasicBlock::iterator startInst,
                    SILBasicBlock::iterator endInst,
                    SILValue keyOperand) {
    // Initialize the interpreter state
    ConstExprStepEvaluator stepEval(constEval, fun);

    for (auto currInst = startInst; ; currInst++) {
      if (currInst == endInst) { // || currInst == fun->g) {
        break;
      }
      auto inst = &(*currInst);
      //llvm::errs() << "Partially evaluating: " << *inst << "\n";

      // If we see a control-flow instruction return an error
      if (isa<ReturnInst>(inst)) {
        llvm::errs() << " Unexpected instruction found: " << inst << "\n";
        return None;
      }

      // TODO: ensure that we invalidate any mutable state whenever we give
      // the interpretation.

      // If this is a call to apply, interpret this only if this is a
      // compiler-evaluable function or a well-known function (which is like
      // a primitve operation).
      if (auto *apply = dyn_cast<ApplyInst>(inst)) {
        auto calleeFun = apply->getCalleeFunction();
        if (!calleeFun ||
            (!stepEval.isKnownPrimitive(calleeFun) &&
             !calleeFun->hasSemanticsAttrThatStartsWith("oslog"))) {
          continue;
        }
        
        if (!stringUTF8InitCall &&
            calleeFun->hasSemanticsAttr("string.makeUTF8")) {
          stringUTF8InitCall = apply;
        }

        // Evaluation of this function bound to not fail.
        auto res = stepEval.stepOver(currInst);
        auto errorVal = res.second;
        if (errorVal.hasValue() &&
            calleeFun->hasSemanticsAttr("compiler.evaluable")) {
          llvm::errs() << "Interpretation of compiler evaluable function failed with this error: " << errorVal.getValue() << "\n";
          return None;
        }
        continue;
      }

      // For every non-apply instruction, if we can't evaluate this
      // flow-sensitively, ignore this and keep going.
      auto res = stepEval.stepOver(currInst);
      auto errorVal = res.second;
      if (errorVal.hasValue()) { // If it has a value then it signals an error.
        continue;
      }
    }
    auto symValOpt = stepEval.lookupConstValue(keyOperand);
    return symValOpt;
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

    SILModule &M = getFunction()->getModule();
    auto &fun = *getFunction();
    auto deletedInsts = SmallVector<SILInstruction *, 10>();
    for (auto &BB : fun) {
      for (auto &I : BB) {
        // (a) check if this is a call to "oslog"
        auto *applyInst = dyn_cast<ApplyInst>(&I);
        if (!applyInst || !applyInst->getCalleeFunction()
            || !applyInst->getCalleeFunction()->hasSemanticsAttrThatStartsWith(
                                                          "oslog.log"))
          continue;
        llvm::errs() << "Found call to oslog " << I << "\n";

        // (b) extract the second operand to the call.
        // This is the value that needs to be statically computed.
        SILValue packedMsgArg = applyInst->getArgument(1);
        // Ensure that this
        llvm::errs() << "Res operand: " << packedMsgArg << "\n";

        // (c) Walk backwards along the use-def chain and identify the
        // the sequence of instructions that needs to be interpreted
        // to statically determine the value of `packedMsgArg`
        SILBasicBlock::iterator startInst;
        SILBasicBlock::iterator endInst;
        std::tie(startInst, endInst) =
            getStringInterpolationInstrs(
                          packedMsgArg->getDefiningInstruction(),
                          applyInst->getOperand(0)->getDefiningInstruction(),
                          fun)
                          .getValue();

        llvm::errs() << "Instructs to interpret: \n";
        dumpInstructions(startInst, endInst);
        return;

        // (d) Interpret the code
        llvm::BumpPtrAllocator bumpAllocator;
        ConstExprEvaluator evaluator(
                   [&](unsigned long bytes, unsigned alignment) {
                     return bumpAllocator.Allocate(bytes, alignment);
                   });
        auto resValue = partiallyEvaluate(evaluator, &fun, startInst, endInst,
                                          packedMsgArg);
        if (!resValue.hasValue()) {
          llvm::errs() << "Interpretation failed!! \n";
        }
        auto packedMsgValue = resValue.getValue();

        // for debugging
        llvm::errs() << "Result value for " <<  packedMsgArg <<  ": \n";
        packedMsgValue.dump();
        llvm::errs() << "\n";

        // (e) Optimize the body of oslog using the constant value for the PackedOSLogMessage.
        optimizeOSLogFun(applyInst, packedMsgArg, packedMsgValue, fun, M);
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
