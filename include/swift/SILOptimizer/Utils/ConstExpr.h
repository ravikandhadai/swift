//===--- ConstExpr.h - Constant expression evaluator -----------*- C++ -*-===//
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
//
// This defines an interface to evaluate Swift language level constant
// expressions.  Its model is intended to be general and reasonably powerful,
// with the goal of standardization in a future version of Swift.
//
// Constant expressions are functions without side effects that take constant
// values and return constant values.  These constants may be integer, and
// floating point values.   We allow abstractions to be built out of fragile
// structs and tuples.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILOPTIMIZER_CONSTEXPR_H
#define SWIFT_SILOPTIMIZER_CONSTEXPR_H

#include "swift/Basic/LLVM.h"
#include "swift/Basic/SourceLoc.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "swift/SIL/SILBasicBlock.h"

namespace swift {
class ASTContext;
class Operand;
class SILFunction;
class SILModule;
class SILNode;
class SymbolicValue;
class SymbolicValueAllocator;
class ConstExprFunctionState;
enum class UnknownReason;

/// This class is the main entrypoint for evaluating constant expressions.  It
/// also handles caching of previously computed constexpr results.
class ConstExprEvaluator {
  /// We store arguments and result values for the cached constexpr calls we
  /// have already analyzed in this ASTContext so that they are available even
  /// after this ConstExprEvaluator is gone.
  SymbolicValueAllocator &allocator;

  /// The current call stack, used for providing accurate diagnostics.
  llvm::SmallVector<SourceLoc, 4> callStack;

  ConstExprEvaluator(const ConstExprEvaluator &) = delete;
  void operator=(const ConstExprEvaluator &) = delete;

public:
  explicit ConstExprEvaluator(SymbolicValueAllocator &alloc);
  ~ConstExprEvaluator();

  SymbolicValueAllocator &getAllocator() { return allocator; }

  void pushCallStack(SourceLoc loc) { callStack.push_back(loc); }

  void popCallStack() {
    assert(!callStack.empty());
    callStack.pop_back();
  }

  const llvm::SmallVector<SourceLoc, 4> &getCallStack() { return callStack; }

  // As SymbolicValue::getUnknown(), but handles passing the call stack and
  // allocator.
  SymbolicValue getUnknown(SILNode *node, UnknownReason reason);

  /// Analyze the specified values to determine if they are constant values.
  /// This is done in code that is not necessarily itself a constexpr
  /// function.  The results are added to the results list which is a parallel
  /// structure to the input values.
  ///
  /// TODO: Return information about which callees were found to be
  /// constexprs, which would allow the caller to delete dead calls to them
  /// that occur after after folding them.
  void computeConstantValues(ArrayRef<SILValue> values,
                             SmallVectorImpl<SymbolicValue> &results);
};

/// Constant expression evaluator that performs step-by-step evaluation
/// by evaluating one instruction at a time.
class ConstExprStepEvaluator {
private:
  ConstExprEvaluator &evaluator;
  ConstExprFunctionState *internalState;
  unsigned stepsEvaluated = 0;
  /// Targets of conditional branches that were visited. This is used to detect
  /// loops during evaluation.
  SmallPtrSet<SILBasicBlock *, 8> visitedBlocks;

  ConstExprStepEvaluator(const ConstExprEvaluator &) = delete;
  void operator=(const ConstExprEvaluator &) = delete;

  Optional<SymbolicValue>
  incrementStepsAndCheckLimit(SILInstruction *inst,
                              bool includeInInstructionLimit);

public:
  explicit ConstExprStepEvaluator(ConstExprEvaluator &eval, SILFunction *fun);
  ~ConstExprStepEvaluator();

  /// Evaluate an instruction in the current interpreter state.
  /// \param instI instruction to be evaluated in the current interpreter state.
  /// \returns a pair where the first and second elements are defined as follows:
  ///   The first element is the iterator to the next instruction from the where
  ///   the evaluation can continue, if the evaluation is successful.
  ///   Otherwise, it is None.
  ///
  ///   Second element is None, if the evaluation is successful.
  ///   Otherwise, is an unknown symbolic value that contains the error.
  std::pair<Optional<SILBasicBlock::iterator>, Optional<SymbolicValue>>
  evaluate(SILBasicBlock::iterator instI,
           bool includeInInstructionLimit = true);

  /// Skip the instruction without evaluating it and conservatively account for
  /// the effects of the instruction on the internal state. This operation
  /// resets to an unknown symbolic value any portion of a
  /// SymbolicValueMemoryObject that could possibly be mutated by the skipped
  /// instruction. This function preserves soundness of interpretation.
  /// \param instI instruction to be skipped.
  /// \returns a pair where the first and second elements are defined as follows:
  ///   First element: if not None, it is the iterator to the next
  ///   instruction from the where the evaluation can continue.
  ///   The first element is None if the next instruction from where the
  ///   evaluation must continue cannot be determined when the instruction is
  ///   skipped. This would be the case if `instI` is branch e.g. like a condbr.
  ///
  ///   Second element is None if skipping the instruction was successful, .
  ///   Otherwise, it is an unknown symbolic value containing the error.
  std::pair<Optional<SILBasicBlock::iterator>, Optional<SymbolicValue>>
  skip(SILBasicBlock::iterator instI, bool includeInInstructionLimit = true);

  /// Try evaluating an instruction and if the evaluation fails, skip the
  /// instruction. See `evaluate` and `skip` functions for their semantics.
  /// \param instI instruction to be evaluated in the current interpreter state.
  /// \returns a pair where the first and second elements are defined as follows:
  ///   First element: if not None, it is the iterator to the next
  ///   instruction from the where the evaluation can continue.
  ///   The first element is None iff neither `evaluate` nor `skip` function
  ///   can determine a valid next instruction.
  ///
  ///   Second element is None if the evaluation is successful.
  ///   Otherwise, it is the error in the form of a symbolic value.
  std::pair<Optional<SILBasicBlock::iterator>, Optional<SymbolicValue>>
  tryEvaluateOrElseSkip(SILBasicBlock::iterator instI);

  Optional<SymbolicValue> lookupConstValue(SILValue value);

  bool isKnownPrimitive(SILFunction *fun);

  void dumpState();
};

} // end namespace swift
#endif
