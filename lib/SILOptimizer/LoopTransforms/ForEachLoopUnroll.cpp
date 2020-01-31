//===--- ForEachLoopUnrolling.cpp - Unroll loops over array literals ----- ===//
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

// Algorithm Overview:
//
//
//  1. Use array semantic call functions and @_semantics attribute on forEach
//     loops to determine whether the array is a array literal and is only
//     read. If so, collect all forEach uses. Note that we can ignore
//     reference counting uses including fixLifetime use (which accepts an
//     an indirect SIL parameter).
//
//  2. If this is a read-only array literal. Use a threshold to determine
//      which loops to unroll. When there is only one loop to unroll, it
//      is almost always beneficial to unroll the loop as the stores into the
//     array will be dead.
//
//  3. Unroll the forEach loops this would involve first taking the first SIL
//     argument of the forEach call, which is a closure that throws and also
//     accepts the argument indirectly. The closure should then be applied
//     to the array elements (which may have to stored into a stack, before
//     passing in). Also, closure has to be try-applied where the normal
//     case should go to the next closure application and the error case goes
//     to the error case of the forEach. Alternatively, we can look through the
//     reabstraction thunks to recover the non-throwing closure. But the former
//     is preferred.
//
//  4. Try and delete the array literal initialization if there are no uses
//      of it other than in reference counting uses. Here, we need to assert
//      that the array is destroyed eventually. Otherwise, it must have had an
//      escaping use. (What about indirect uses? they only dealloc_stack and
//      never  destroy_addr). Since the array is destroyed, we need not destroy
//      the stored uses. If there are other uses of the array literal, bail out
//      and they will be handled by the ArrayElementPropagation passes.

#include "swift/AST/ASTContext.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Module.h"
#include "swift/AST/SubstitutionMap.h"
#include "swift/Basic/OptimizationMode.h"
#include "swift/AST/SemanticAttrs.h"
#include "swift/SIL/BasicBlockUtils.h"
#include "swift/SIL/CFG.h"
#include "swift/SIL/InstructionUtils.h"
#include "swift/SIL/OwnershipUtils.h"
#include "swift/SIL/SILBasicBlock.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILConstants.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/TypeLowering.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/CFGOptUtils.h"
#include "swift/SILOptimizer/Utils/InstOptUtils.h"
#include "swift/SILOptimizer/Analysis/ArraySemantic.h"
#include "swift/SIL/DebugUtils.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/DenseMap.h"


using namespace swift;

namespace {

// TODO: move this to a common place. This is copied from array element
// propagation.
/// Map the indices of array element initialization stores to their values.
static bool mapInitializationStores(SILValue ElementBuffer,
                                    llvm::DenseMap<uint64_t, SILValue> &ElementValueMap) {
  assert(ElementBuffer &&
         "Must have identified an array element storage pointer");

  // Match initialization stores.
  // %83 = struct_extract %element_buffer : $UnsafeMutablePointer<Int>
  // %84 = pointer_to_address %83 : $Builtin.RawPointer to strict $*Int
  // store %85 to %84 : $*Int
  // %87 = integer_literal $Builtin.Word, 1
  // %88 = index_addr %84 : $*Int, %87 : $Builtin.Word
  // store %some_value to %88 : $*Int

//  auto *UnsafeMutablePointerExtract =
//      dyn_cast_or_null<StructExtractInst>(getSingleNonDebugUser(ElementBuffer));
//  if (!UnsafeMutablePointerExtract)
//    return false;
  auto *PointerToAddress = dyn_cast_or_null<PointerToAddressInst>(
      getSingleNonDebugUser(ElementBuffer));
  if (!PointerToAddress)
    return false;

  // Match the stores. We can have either a store directly to the address or
  // to an index_addr projection.
  for (auto *Op : PointerToAddress->getUses()) {
    auto *Inst = Op->getUser();

    // Store to the base.
    auto *SI = dyn_cast<StoreInst>(Inst);
    if (SI && SI->getDest() == PointerToAddress) {
      // We have already seen an entry for this index bail.
      if (ElementValueMap.count(0))
        return false;
      ElementValueMap[0] = SI->getSrc();
      continue;
    } else if (SI)
      return false;

    // Store an index_addr projection.
    auto *IndexAddr = dyn_cast<IndexAddrInst>(Inst);
    if (!IndexAddr)
      return false;
    SI = dyn_cast_or_null<StoreInst>(getSingleNonDebugUser(IndexAddr));
    if (!SI || SI->getDest() != IndexAddr)
      return false;
    auto *Index = dyn_cast<IntegerLiteralInst>(IndexAddr->getIndex());
    if (!Index)
      return false;
    auto IndexVal = Index->getValue();
    // Let's not blow up our map.
    if (IndexVal.getActiveBits() > 16)
      return false;
    // Already saw an entry.
    if (ElementValueMap.count(IndexVal.getZExtValue()))
      return false;

    ElementValueMap[IndexVal.getZExtValue()] = SI->getSrc();
  }
  return !ElementValueMap.empty();
}

static FixLifetimeInst *fixLifetimeUseOfArray(SILInstruction *user,
                                                SILValue array) {
  // Since an array would be passed indirectly to fixLifetime instruction,
  // we would only see a store the array into an alloc_stack here and have to
  // look at the uses of the alloc_stack.
  StoreBorrowInst *storeUser = dyn_cast<StoreBorrowInst>(user);
  if (!storeUser || storeUser->getSrc() != array)
    return nullptr;
  AllocStackInst *alloc = dyn_cast<AllocStackInst>(storeUser->getDest());
  if (!alloc)
    return nullptr;
  auto fixLifetimeUsers = alloc->getUsersOfType<FixLifetimeInst>();
  if (fixLifetimeUsers.empty())
    return nullptr;
  auto firstUser = fixLifetimeUsers.begin();
  FixLifetimeInst *result = *firstUser;
  // We need to have a unique result.
  if (++firstUser != fixLifetimeUsers.end())
    return nullptr;
  return result;
}

static TryApplyInst *forEachUseOfArray(SILInstruction *user, SILValue array) {
  // Since an array would be passed indirectly to forEach instruction,
  // we would only see a store the array into an alloc_stack here and have to
  // look at the uses of the alloc_stack.
  StoreBorrowInst *storeUser = dyn_cast<StoreBorrowInst>(user);
  if (!storeUser || storeUser->getSrc() != array)
    return nullptr;
  AllocStackInst *alloc = dyn_cast<AllocStackInst>(storeUser->getDest());
  if (!alloc)
    return nullptr;
  auto applyUsers = alloc->getUsersOfType<TryApplyInst>();
  if (applyUsers.empty())
    return nullptr;
  auto firstUser = applyUsers.begin();
  TryApplyInst *apply = *firstUser;
  // We need to have a unique result.
  if (++firstUser != applyUsers.end())
    return nullptr;
  SILFunction *callee = apply->getCalleeFunction();
  if (!callee || !callee->hasSemanticsAttr(semantics::SEQUENCE_FOR_EACH))
    return nullptr;
  return apply;
}

// TODO: move this to instruction deleter?
static void recursivelyDeleteInstructionAndUses(SILInstruction *inst,
                                                InstructionDeleter &deleter) {
  for (SILValue result : inst->getResults()) {
    while (!result->use_empty()) {
      SILInstruction *user = result->use_begin()->getUser();
      recursivelyDeleteInstructionAndUses(user, deleter);
    }
  }
  if (isIncidentalUse(inst) || isa<DestroyValueInst>(inst)) {
    deleter.forceDelete(inst);
    return;
  }
  deleter.forceDeleteAndFixLifetimes(inst);
}

/// Utility class for storing information about array literal initializations and
/// forEach operations on the array .
///
/// Array literals are initialized by allocating an array buffer, and storing
/// the elements into it.
/// This class analyzes all the code which does the array literal
/// initialization. It also collects forEach uses of the array and also
/// information about other uses.
class ArrayLiteralInfo {
  /// The array value returned by the _allocateUninitialized call.
  SILValue arrayValue;

  /// A map of Array indices to element values
  llvm::DenseMap<uint64_t, SILValue> elementValueMap;

  /// List of Sequence.forEach calls in which the array is used.
  SmallSetVector<TryApplyInst *, 4> forEachCalls;

  /// Indicates whether the array may be written/updated after initialization.
  /// The presence of such uses will prevent the forEach loops from getting
  /// unrolled.
  bool mayBeWritten = false;

  /// Indicates whether the array could be used in read-only operations such
  /// array.count, subscript etc. apart from the forEach, destroy and incidental
  /// uses. The presence of such uses will block the array from being deleted
  /// once the forEach calls are unrolled.
  bool hasNonForEachReadOnlyUses = false;

  /// destroy_value instructions of the array.
  SmallVector<DestroyValueInst *, 4> destroys;

  void classifyUsesOfArray(SILValue arrayValue) {
    for (Operand *operand : arrayValue->getUses()) {
      auto *user = operand->getUser();
      if (isIncidentalUse(user))
        continue;
      // Ignore fixLifetime uses of the array. Note this will not be subsumed
      // by isIncidentalUse check as the array would be passed indirectly.
      if (fixLifetimeUseOfArray(user, arrayValue))
        continue;
      // Check if this is a forEach call on an array. This also uses an indirect
      // calling convention and therefore must be a store use of arrayVal.
      if (TryApplyInst *forEachCall = forEachUseOfArray(user, arrayValue)) {
        forEachCalls.insert(forEachCall);
        continue;
      }
      // Recursively classify begin borrow uses.
      if (BeginBorrowInst *beginBorrow = dyn_cast<BeginBorrowInst>(user)) {
        classifyUsesOfArray(beginBorrow);
        continue;
      }
      if (auto *destroyValue = dyn_cast<DestroyValueInst>(user)) {
        destroys.push_back(destroyValue);
        continue;
      }
      // Set mayBeWritten to true if the user could modify the array. (Note that
      // mutation of the array elements can be ignored). Otherwise, set
      // hasNonForEachReadOnlyUses to true.
      ArraySemanticsCall arrayOp(user);
      if (arrayOp.doesNotChangeArray()) {
        hasNonForEachReadOnlyUses = true;
      } else {
        mayBeWritten = true;
      }
    }
  }

public:

  ArrayLiteralInfo() {}

  /// Given an apply instruction \c apply, try to initialize this ArrayLiteralInfo
  /// with it. This would succeed iff the apply instruction corresponds to the initialization
  /// of an array literal. Return true on success and false on failure.
  bool tryInitialize(ApplyInst *apply) {
    ArraySemanticsCall arrayAllocateUninitCall(apply,
                                               semantics::ARRAY_UNINITIALIZED_INTRINSIC);
    if (!arrayAllocateUninitCall)
      return false;
    arrayValue = arrayAllocateUninitCall.getArrayValue();
    if (!arrayValue)
      return false;

    SILValue elementBuffer = arrayAllocateUninitCall.getArrayElementStoragePointer();
    if (!elementBuffer)
      return false;

    if (!mapInitializationStores(elementBuffer, elementValueMap))
      return false;

    // Initialize the information about uses of the array value.
    classifyUsesOfArray(arrayValue);
    return true;
  }

  SILValue getArrayValue() {
    assert(arrayValue);
    return arrayValue;
  }

  bool mayBeModified() {
    assert(arrayValue);
    return mayBeWritten;
  }

  ArrayRef<TryApplyInst *> getForEachUses() {
    assert(arrayValue);
    return ArrayRef<TryApplyInst *>(forEachCalls.begin(), forEachCalls.end());
  }

  uint64_t getElementSize() {
    assert(arrayValue);
    return elementValueMap.size();
  }

  SILValue getElement(uint64_t index) {
    assert(arrayValue);
    return elementValueMap[index];
  }

  ArrayRef<DestroyValueInst *> getDestroys() {
    assert(arrayValue);
    return destroys;
  }

  void removeForEachCall(TryApplyInst *forEachCall) {
    assert(arrayValue);
    if (!forEachCalls.remove(forEachCall))
      return; // If the forEach call is already removed, do nothing.
    // Force delete the alloc_stack of the forEach call and its uses.
    AllocStackInst *allocStack =
        dyn_cast<AllocStackInst>(forEachCall->getArgument(1));
    assert(allocStack);
    InstructionDeleter deleter;
    recursivelyDeleteInstructionAndUses(allocStack, deleter);
    deleter.cleanUpDeadInstructions();
  }

  bool deleteArrayValueIfDead(InstructionDeleter &deleter) {
    // If the array has forEach calls, or has nonForEach uses or may be written,
    // it is not dead and cannot be cleaned up.
    if (!forEachCalls.empty() || hasNonForEachReadOnlyUses || mayBeWritten)
      return false;
    if (!getDebugUses(arrayValue).empty() &&
        arrayValue->getFunction()->getEffectiveOptimizationMode() <=
            OptimizationMode::NoOptimization) {
      // Don't delete the array with debug uses in Onone.
      return false;
    }
    recursivelyDeleteInstructionAndUses(arrayValue->getDefiningInstruction(),
                                        deleter);
    return true;
  }
};

///// Given a SILValue \p value, find the point where a copy_value of the value
///can be inserted. This is
///// either the instruction following the definingInstruction of value, if
///there is one. Otherwise, it is the
///// first instruction of the basic block that takes the value as the
///parameter.
// static SILInstruction *getInsertionPointForCopyValue(SILValue value) {
//  if (SILInstruction *definingInst = value->getDefiningInstruction()) {
//    return &(*std::next(definingInst->getIterator()));
//  }
//  SILBasicBlock *parentBlock = value->getParentBlock();
//  assert(parentBlock);
//  return &parentBlock->front();
//}

static void unrollForEach(ArrayLiteralInfo &arrayLiteralInfo,
                          TryApplyInst *forEachCall) {
  if (arrayLiteralInfo.getElementSize() == 0) {
    // If this is an empty array, delete the forEach entirely.
    arrayLiteralInfo.removeForEachCall(forEachCall);
    return;
  }

  SILFunction *fun = forEachCall->getFunction();
  SILLocation forEachLoc = forEachCall->getLoc();
  SILValue forEachBodyClosure = forEachCall->getArgument(0);

  // The forEachBodyClosure uses @in_guaranteed convention for passing
  // arguments. But, we only consider array literals where the element values
  // are "stored" into the array. That is, we only consider arrays with loadable
  // elements. Therefore, we need to create an alloc_stack to indirectly pass
  // the elements. TO do this, create copies of the array element at the
  // time when they are put into the array. Connect the lifetimes of the
  // copies to the lifetime of the array. The copies would be destroyed
  // when the array is destroyed.
  SILFunctionType *bodyClosureType =
    forEachBodyClosure->getType().getAs<SILFunctionType>();
  SILParameterInfo bodyParameterInfo = bodyClosureType->getParameters()[0];
  assert(bodyParameterInfo.getConvention() ==
         ParameterConvention::Indirect_In_Guaranteed &&
         "forEach body closure is expected to take @in_guaranteed argument");

  // TODO: use the store instructions as the insertion point for this.
  SmallVector<SILValue, 4> elementCopies;
  SILInstruction *copyInsertionPoint =
      arrayLiteralInfo.getArrayValue().getDefiningInstruction();
  for (uint64_t i = 0; i < arrayLiteralInfo.getElementSize(); i++) {
    SILValue element = arrayLiteralInfo.getElement(i);
    SILValue copy = SILBuilderWithScope(copyInsertionPoint)
                        .createCopyValue(copyInsertionPoint->getLoc(), element);
    elementCopies.push_back(copy);
  }

  // Destroy all copies wherever the array is destroyed. Note that since the
  // array doesn't have any consuming operation (except destroy) its lifetime
  // ends only in destroys.
  for (DestroyValueInst *destroy : arrayLiteralInfo.getDestroys()) {
    SILBuilderWithScope destroyBuilder(destroy);
    for (SILValue element : elementCopies) {
      destroyBuilder.createDestroyValue(destroy->getLoc(), element);
    }
  }

  // Apply the body closure on every element of the array. There are two
  // things to handle here:
  //   1. The array elements must be passed @in_guaranteed to the body closure.
  //      Therefore, create an alloc_stack and use it to store the elements
  //      and pass the alloc_stack to the calls. The alloc_stack must be
  //      deallocated in the errorBB as well as normalBB of the original
  //      forEachCall.
  //   2. Since the body closure may throw, we need to create new of basic
  //      blocks for each unrolling of the loop.
  SILType arrayElementType = arrayLiteralInfo.getElement(0)->getType();
  // Create alloc_stack to hold the array elements.
  SILValue allocStack = SILBuilderWithScope(forEachCall)
                            .createAllocStack(forEachLoc, arrayElementType);

  SILBasicBlock *normalBB = forEachCall->getNormalBB();
  SILBasicBlock *errorBB = forEachCall->getErrorBB();
  // Error and normal blocks must be taking a phi argument.
  assert(errorBB->getSILPhiArguments().size() == 1 &&
         normalBB->getSILPhiArguments().size() == 1);
  SILPhiArgument *normalArgument = normalBB->getSILPhiArguments()[0];
  SILPhiArgument *errorArgument = errorBB->getSILPhiArguments()[0];

  auto normalTargetGenerator = [&](SILBasicBlock *insertionBlock) {
    SILBasicBlock *newBB = fun->createBasicBlockBefore(insertionBlock);
    newBB->createPhiArgument(normalArgument->getType(),
                             normalArgument->getOwnershipKind());
    return newBB;
  };

  auto errorTargetGenerator = [&](SILBasicBlock *insertionBlock) {
    SILBasicBlock *newErrorBB = fun->createBasicBlockBefore(insertionBlock);
    SILValue argument = newErrorBB->createPhiArgument(
        errorArgument->getType(), errorArgument->getOwnershipKind());
    SILBuilderWithScope builder(newErrorBB, forEachCall);
    builder.createBranch(forEachLoc, errorBB, argument);
    return newErrorBB;
  };

  // Iterate through the array elements in the reverse order and create try
  // applies of the body closure.
  SILBasicBlock *nextNormalBB = normalBB;
  for (uint64_t num = arrayLiteralInfo.getElementSize(); num > 0; num--) {
    SILValue elementCopy = elementCopies[num - 1];
    // Create a new basic block from second element onwards.
    SILBasicBlock *currentBB = num > 1 ? normalTargetGenerator(nextNormalBB)
                                       : forEachCall->getParentBlock();
    SILBuilderWithScope unrollBuilder(currentBB, forEachCall);
    // Borrow the elementCopy and store it in the allocStack. Note that the
    // element's copy is guaranteed to be alive until the array is alive.
    // Therefore it is okay to use a borrow here.
    SILValue borrowedElem =
        unrollBuilder.createBeginBorrow(forEachLoc, elementCopy);
    unrollBuilder.createStoreBorrow(forEachLoc, borrowedElem, allocStack);
    unrollBuilder.createEndBorrow(forEachLoc, borrowedElem);
    // Create an error target for the try-apply.
    SILBasicBlock *errorTarget = errorTargetGenerator(nextNormalBB);
    // TODO: substitution map must be empty, is any other option possible?
    unrollBuilder.createTryApply(forEachLoc, forEachBodyClosure,
                                 SubstitutionMap(), allocStack, nextNormalBB,
                                 errorTarget);
    nextNormalBB = currentBB;
  }

  // Dealloc the stack in normalBB and also in errorBB.
  SILBuilderWithScope(&normalBB->front())
      .createDeallocStack(forEachLoc, allocStack);
  SILBuilderWithScope(&errorBB->front())
      .createDeallocStack(forEachLoc, allocStack);

  // Remove the forEach and clean up dead instructions.
  arrayLiteralInfo.removeForEachCall(forEachCall);
}

static bool tryUnrollForEachCallsOverArrayLiteral(ApplyInst *apply,
                                                  InstructionDeleter &deleter,
                                                  bool &isArrayValueDeleted) {
  isArrayValueDeleted = false;
  // Try to initialiaze the array literal.
  ArrayLiteralInfo arrayLiteralInfo;
  if (!arrayLiteralInfo.tryInitialize(apply))
    return false;
  // Bail out, if the array could be modified after initialization.
  if (arrayLiteralInfo.mayBeModified())
    return false;
  // Check if the thresholds for unrolling are met. For now, always unroll if
  // there is only one forEach call. TODO: make this parametrizable.
  ArrayRef<TryApplyInst *> forEachCalls = arrayLiteralInfo.getForEachUses();
  if (forEachCalls.empty() || forEachCalls.size() > 1)
    return false;
  unrollForEach(arrayLiteralInfo, forEachCalls.front());
  isArrayValueDeleted = arrayLiteralInfo.deleteArrayValueIfDead(deleter);
  return true;
}

class ForEachLoopUnroller : public SILFunctionTransform {

  ~ForEachLoopUnroller() override {}

  /// The entry point to the transformation.
  void run() override {
    SILFunction &fun = *getFunction();
    bool changed = false;

    InstructionDeleter deleter;

    for (SILBasicBlock &bb : fun) {
      for (auto instIter = bb.begin(); instIter != bb.end();) {
        SILInstruction *inst = &*instIter;
        ApplyInst *apply = dyn_cast<ApplyInst>(inst);
        if (!apply) {
          instIter++;
          continue;
        }
        bool isArrayValueDeleted;
        changed |= tryUnrollForEachCallsOverArrayLiteral(apply, deleter,
                                                         isArrayValueDeleted);
        instIter++;
        if (isArrayValueDeleted) {
          // We can force delete the apply here. Note that the iterator has
          // already been advanced here. Therefore, there will be no iterator
          // invalidation.
          recursivelyDeleteInstructionAndUses(inst, deleter);
        }
      }
    }

    if (changed) {
      deleter.cleanUpDeadInstructions();
      PM->invalidateAnalysis(&fun,
                  SILAnalysis::InvalidationKind::FunctionBody);
    }
  }
};

} // end anonymous namespace

SILTransform *swift::createForEachLoopUnroll() {
  return new ForEachLoopUnroller();
}
