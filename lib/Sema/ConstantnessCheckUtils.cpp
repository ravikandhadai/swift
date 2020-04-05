//===------------------------ ConstantnessSemaDiagnostics.cpp -------------===//
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
// This file implements utilities for checking whether certain arguments to some
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

#include "TypeChecker.h"
#include "ConstraintSystem.h"
#include "ConstantnessCheckUtils.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/SemanticAttrs.h"
using namespace swift;
using namespace constraints;

/// Check whether a given \p decl has a @_semantics attribute with the given
/// attribute name \c attrName.
static bool hasSemanticsAttr(ValueDecl *decl, StringRef attrName) {
  for (auto semantics : decl->getAttrs().getAttributes<SemanticsAttr>()) {
    if (semantics->Value.equals(attrName))
      return true;
  }
  return false;
}

/// Return true iff  the given \p structDecl has a name that matches one of the
/// known atomic orderings structs.
static bool isAtomicOrderingDecl(StructDecl *structDecl) {
  ASTContext &astContext = structDecl->getASTContext();
  Identifier structName = structDecl->getName();
  return (structName == astContext.Id_AtomicLoadOrdering ||
          structName == astContext.Id_AtomicStoreOrdering ||
          structName == astContext.Id_AtomicUpdateOrdering);
}

bool swift::isParamRequiredToBeConstant(ValueDecl *decl, Type paramType) {
  assert(decl && "funcDecl must not be null");

  FuncDecl *funcDecl = dyn_cast<FuncDecl>(decl);
  if (!funcDecl)
    return false;
  if (hasSemanticsAttr(funcDecl, semantics::OSLOG_REQUIRES_CONSTANT_ARGUMENTS))
    return true;
  if (!hasSemanticsAttr(funcDecl,
                        semantics::ATOMICS_REQUIRES_CONSTANT_ORDERINGS))
    return false;
  StructDecl *structDecl = paramType->getStructOrBoundGenericStruct();
  if (!structDecl)
    return false;
  return isAtomicOrderingDecl(structDecl);
}

/// Return true iff the \c decl is annotated as
/// @_semantics("constant_evaluable").
static bool hasConstantEvaluableAttr(ValueDecl *decl) {
  return hasSemanticsAttr(decl, semantics::CONSTANT_EVALUABLE);
}

Expr *swift::checkConstantness(Expr *expr, ConstraintLocator *locator,
                            ConstraintSystem *cs) {
  SmallVector<Expr *, 4> expressionsToCheck;
  expressionsToCheck.push_back(expr);
  while (!expressionsToCheck.empty()) {
    Expr *expr = expressionsToCheck.pop_back_val();
    // Lookthrough identity_expr, tuple and inject_into_optional expressions.
    if (IdentityExpr *identityExpr = dyn_cast<IdentityExpr>(expr)) {
      expressionsToCheck.push_back(identityExpr->getSubExpr());
      continue;
    }
    if (TupleExpr *tupleExpr = dyn_cast<TupleExpr>(expr)) {
      for (Expr *element : tupleExpr->getElements())
        expressionsToCheck.push_back(element);
      continue;
    }
    if (InjectIntoOptionalExpr *optionalExpr =
            dyn_cast<InjectIntoOptionalExpr>(expr)) {
      expressionsToCheck.push_back(optionalExpr->getSubExpr());
      continue;
    }
    // Literal expressions also includes InterpolatedStringLiteralExpr.
    if (isa<LiteralExpr>(expr))
      continue;
    if (isa<TypeExpr>(expr))
      continue;
    // Closure expressions are always treated as constants. They are
    // constants of function types.
    if (isa<AbstractClosureExpr>(expr))
      continue;
    // Skip the constantness check for default argument expressions as it is the libraries
    // responsibility to ensure that it is a constant, when necessary.
    if (isa<DefaultArgumentExpr>(expr))
      continue;

    // If this is a member-ref, it has to be annotated constant evaluable.
    if (isa<MemberRefExpr>(expr) || isa<UnresolvedMemberExpr>(expr)) {
      ValueDecl *memberDecl =
        isa<MemberRefExpr>(expr) ?
          cast<MemberRefExpr>(expr)->getMember().getDecl() :
          cs->findResolvedMemberRef(
            cs->getConstraintLocator(cast<UnresolvedMemberExpr>(expr),
                                     ConstraintLocator::UnresolvedMember));
      if (!memberDecl) {
        llvm::errs() << "MemberDecl is null\n";
        return expr;
      }
      llvm::errs() << "MemberDecl: \n";
      memberDecl->dump();
      llvm::errs() << "\n";
      if (!memberDecl || !hasConstantEvaluableAttr(memberDecl))
        return expr;
      continue;
    }

    // If this is a variable, it has to be a known constant parameter of the
    // enclosing function.
    if (DeclRefExpr *declRef = dyn_cast<DeclRefExpr>(expr)) {
      ValueDecl *decl = declRef->getDecl();
      if (!decl)
        return expr;
      ParamDecl *paramDecl = dyn_cast<ParamDecl>(decl);
      if (!paramDecl)
        return expr;
      Decl *declContext = paramDecl->getDeclContext()->getAsDecl();
      if (!declContext)
        return expr;
      FuncDecl *funcDecl = dyn_cast<FuncDecl>(declContext);
      if (!funcDecl || !isParamRequiredToBeConstant(funcDecl, paramDecl->getType()))
        return expr;
      continue;
    }

    if (!isa<ApplyExpr>(expr))
      return expr;

    ApplyExpr *apply = cast<ApplyExpr>(expr);
    // Resolve the callee.
    ValueDecl *calledValue =
      cs->findResolvedMemberRef(
          cs->getCalleeLocator(cs->getConstraintLocator(apply)));
    //ValueDecl *calledValue = apply->getCalledValue();
    if (!calledValue)
      return expr;

    // If this is an enum case, check whether the arguments are constants.
    if (isa<EnumElementDecl>(calledValue)) {
      expressionsToCheck.push_back(apply->getArg());
      continue;
    }

    // If this is a constant_evaluable function, check whether the arguments
    // are constants.
    AbstractFunctionDecl *callee = dyn_cast<AbstractFunctionDecl>(calledValue);
    if (!callee || !hasConstantEvaluableAttr(callee))
      return expr;
    expressionsToCheck.push_back(apply->getArg());
//    SmallVector<Expr *, 4> argumentExprs;
//    Expr *argumentVector = apply->getArg();
//    if (auto *tupleExpr = dyn_cast<TupleExpr>(argumentVector)) {
//      for (Expr *element : tupleExpr->getElements())
//        argumentExprs.push_back(element);
//    } else {
//      argumentExprs.push_back(argumentVector);
//    }
//    // Skip default argument expressions.
//    auto nondefaultArgs =
//    llvm::make_filter_range(argumentExprs, [&](Expr *expr) {
//      return !isa<DefaultArgumentExpr>(expr);
//    });
//    for (Expr *argument : nondefaultArgs)
//      expressionsToCheck.push_back(argument);
  }
  return nullptr;
}

/// Return true iff the norminal type decl \c numberDecl is a known stdlib
/// integer decl.
static bool isStdlibInteger(NominalTypeDecl *numberDecl) {
  ASTContext &astCtx = numberDecl->getASTContext();
  return (numberDecl == astCtx.getIntDecl() ||
          numberDecl == astCtx.getInt8Decl() ||
          numberDecl == astCtx.getInt16Decl() ||
          numberDecl == astCtx.getInt32Decl() ||
          numberDecl == astCtx.getInt64Decl() ||
          numberDecl == astCtx.getUIntDecl() ||
          numberDecl == astCtx.getUInt8Decl() ||
          numberDecl == astCtx.getUInt16Decl() ||
          numberDecl == astCtx.getUInt32Decl() ||
          numberDecl == astCtx.getUInt64Decl());
}

/// Return true iff the given \p type is a Stdlib integer type.
static bool isIntegerType(Type type) {
  NominalTypeDecl *nominalDecl = type->getNominalOrBoundGenericNominal();
  return nominalDecl && isStdlibInteger(nominalDecl);
}

/// Return true iff the norminal type decl \c numberDecl is a known stdlib float
/// decl.
static bool isStdlibFloat(NominalTypeDecl *numberDecl) {
  ASTContext &astCtx = numberDecl->getASTContext();
  return (numberDecl == astCtx.getFloatDecl() ||
          numberDecl == astCtx.getFloat80Decl() ||
          numberDecl == astCtx.getDoubleDecl());
}

/// Return true iff the given \p type is a Bool type.
static bool isFloatType(Type type) {
  NominalTypeDecl *nominalDecl = type->getNominalOrBoundGenericNominal();
  return nominalDecl && isStdlibFloat(nominalDecl);
}

/// Return true iff the given \p type is a String type.
static bool isStringType(Type type) {
  NominalTypeDecl *nominalDecl = type->getNominalOrBoundGenericNominal();
  return nominalDecl && nominalDecl == type->getASTContext().getStringDecl();
}

/// Given an error expression \p errorExpr, diagnose the error based on the type
/// of the expression. For instance, if the expression's type is expressible by
/// a literal e.g. integer, boolean etc. report that it must be a literal.
/// Otherwise, if the expression is a nominal type, report that it must be
/// static member of the type.
static void diagnoseError(Expr *errorExpr, const ASTContext &astContext,
                          FuncDecl *funcDecl) {
  DiagnosticEngine &diags = astContext.Diags;
  Type exprType = errorExpr->getType();
  SourceLoc errorLoc = errorExpr->getLoc();

  // Diagnose atomics ordering related error here.
  if (hasSemanticsAttr(funcDecl,
                       semantics::ATOMICS_REQUIRES_CONSTANT_ORDERINGS)) {
    NominalTypeDecl *nominalDecl = exprType->getNominalOrBoundGenericNominal();
    if (!nominalDecl) {
      // This case should normally not happen. This is a safe guard against
      // possible mismatch between the atomics library and the compiler.
      diags.diagnose(errorLoc, diag::argument_must_be_constant);
    }
    diags.diagnose(errorLoc, diag::atomics_ordering_must_be_constant,
                   nominalDecl->getName());
    return;
  }

  // Diagnose os_log specific errors here.

  // Diagnose primitive stdlib types.
  if (exprType->isBool()) {
    diags.diagnose(errorLoc, diag::oslog_arg_must_be_bool_literal);
    return;
  }
  if (isStringType(exprType)) {
    diags.diagnose(errorLoc, diag::oslog_arg_must_be_string_literal);
    return;
  }
  if (isIntegerType(exprType)) {
    diags.diagnose(errorLoc, diag::oslog_arg_must_be_integer_literal);
    return;
  }
  if (isFloatType(exprType)) {
    diags.diagnose(errorLoc, diag::oslog_arg_must_be_float_literal);
    return;
  }
  if (exprType->is<MetatypeType>()) {
    diags.diagnose(errorLoc, diag::oslog_arg_must_be_metatype_literal);
    return;
  }
  if (exprType->is<AnyFunctionType>()) {
    diags.diagnose(errorLoc, diag::oslog_arg_must_be_closure);
    return;
  }
  if (EnumDecl *enumDecl = exprType->getEnumOrBoundGenericEnum()) {
    diags.diagnose(errorLoc, diag::oslog_arg_must_be_enum_case,
                   enumDecl->getName());
    return;
  }
  NominalTypeDecl *nominalDecl = exprType->getNominalOrBoundGenericNominal();
  if (!nominalDecl) {
    // This case should normally not happen. This is a safe guard against
    // possible mismatch between the os overlay and the compiler.
    diags.diagnose(errorLoc, diag::argument_must_be_constant);
    return;
  }
  // If this is OSLogMessage, it should be a string-interpolation literal.
  Identifier declName = nominalDecl->getName();
  if (declName == astContext.Id_OSLogMessage) {
    diags.diagnose(errorLoc, diag::oslog_message_must_be_string_interpolation);
    return;
  }
  diags.diagnose(errorLoc, diag::oslog_arg_must_be_type_member_access,
                 declName);
}

/// Given a call \c callExpr, if some or all of its arguments are required to be
/// constants, check that property on the arguments.
//static void diagnoseConstantArgumentRequirementOfCall(const CallExpr *callExpr,
//                                                      const ASTContext &ctx) {
//  assert(callExpr && callExpr->getType() &&
//         "callExpr should have a valid type");
//  ValueDecl *calledDecl = callExpr->getCalledValue();
//  if (!calledDecl || !isa<FuncDecl>(calledDecl))
//    return;
//  FuncDecl *callee = cast<FuncDecl>(calledDecl);
//
//  // Collect argument indices that are required to be constants.
//  SmallVector<unsigned, 4> constantArgumentIndices;
//  auto paramList = callee->getParameters();
//  for (unsigned i = 0; i < paramList->size(); i++) {
//    ParamDecl *param = paramList->get(i);
//    if (isParamRequiredToBeConstant(callee, param->getType()))
//      constantArgumentIndices.push_back(i);
//  }
//  if (constantArgumentIndices.empty())
//    return;
//
//  // Check that the arguments at the constantArgumentIndices are constants.
//  Expr *argumentExpr = callExpr->getArg();
//  SmallVector<Expr *, 4> arguments;
//  if (TupleExpr *tupleExpr = dyn_cast<TupleExpr>(argumentExpr)) {
//    auto elements = tupleExpr->getElements();
//    arguments.append(elements.begin(), elements.end());
//  } else if (ParenExpr *parenExpr = dyn_cast<ParenExpr>(argumentExpr)) {
//    arguments.push_back(parenExpr->getSubExpr());
//  } else {
//    arguments.push_back(argumentExpr);
//  }
//
//  for (unsigned constantIndex : constantArgumentIndices) {
//    assert(constantIndex < arguments.size() &&
//           "constantIndex exceeds the number of arguments to the function");
//    Expr *argument = arguments[constantIndex];
//    // Skip the constantness check for default argument expressions as it is
//    // the libraries responsibility to ensure that it is a constant, when
//    // necessary.
//    if (isa<DefaultArgumentExpr>(argument))
//      continue;
//    Expr *errorExpr = checkConstantness(argument);
//    if (errorExpr)
//      diagnoseError(errorExpr, ctx, callee);
//  }
//}
