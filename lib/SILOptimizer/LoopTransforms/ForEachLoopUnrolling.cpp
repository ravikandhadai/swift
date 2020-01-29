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
//      array will be dead.
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

using namespace swift;

namespace {

class ForEachLoopUnroller : public SILFunctionTransform {

  ~ForEachLoopUnroller() override {}

  /// The entry point to the transformation.
  void run() override {
  }
};

} // end anonymous namespace

SILTransform *swift::createForEachLoopUnroll() {
  return new ForEachLoopUnroller();
}
