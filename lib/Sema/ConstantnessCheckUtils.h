//===------------------------ ConstantnessCheckUtils.h -------------===//
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
//
// This file defines utilities for checking whether certain arguments to some
// specific APIs are compile-time constants (see below for the definition of
// compile-time constants). In particular, these utilities are used to check whether
// the new os_log APIs are invoked with constant arguments, and whether the primitive
// atomic operations are invoked with constant "orderings". These APIs are identified
// through @_semantics attributes.
//
// (describe where the utilities are used.
//
// A "compile-time constant" is either a literal (including
// string/integer/float/boolean/string-interpolation literal) or a call to a
// "constant_evaluable" function (or property) with compile-time constant
// arguments. A closure expression is also considered a compile-time constant
// (it is a constant of a function type).
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SEMA_CONSTANTNESS_CHECK_UTILS_H
#define SWIFT_SEMA_CONSTANTNESS_CHECK_UTILS_H

namespace swift {

#include "swift/AST/Types.h"
#include "swift/AST/Decl.h"
#include "ConstraintSystem.h"
using namespace constraints;

/// Return true iff the parameter \p param of function \c funDecl is required to
/// be a constant. This is true if either the function is an os_log function or
/// it is an atomics operation and the parameter represents the ordering.
bool isParamRequiredToBeConstant(ValueDecl *funcDecl, Type param);

/// Check whether \p expr is a compile-time constant. It must either be a
/// literal_expr, which does not include array and dictionary literal, or a
/// closure expression, which is considered a compile-time constant of a
/// function type, or a call to a "constant_evaluable" function (or property)
/// whose arguments are themselves compile-time constants.
/// \Return nullptr if \c expr is a compile-time constant. Otherwise, the
/// subexpression that is not a compile-time constant.
Expr *checkConstantness(Expr *expr, ConstraintSystem *cs);

} // namespace swift

#endif // CONSTANTNESS_CHECK_UTILS_H
