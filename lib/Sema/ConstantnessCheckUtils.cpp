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

enum ConstantAPIKind {
  OSLog,
  AtomicOrdering,
};

static Optional<ConstantAPIKind> getConstantAPIKind(ValueDecl *decl) {
  assert(decl && "decl must not be null");
  FuncDecl *funcDecl = dyn_cast<FuncDecl>(decl);
  if (!funcDecl)
    return None;
  if (hasSemanticsAttr(funcDecl, semantics::OSLOG_REQUIRES_CONSTANT_ARGUMENTS))
    return ConstantAPIKind::OSLog;
  if (hasSemanticsAttr(funcDecl,
                       semantics::ATOMICS_REQUIRES_CONSTANT_ORDERINGS))
    return ConstantAPIKind::AtomicOrdering;
  return None;
}

bool swift::isParamRequiredToBeConstant(ValueDecl *decl, Type paramType) {
  Optional<ConstantAPIKind> constantAPI = getConstantAPIKind(decl);
  if (!constantAPI)
    return false;
  if (constantAPI.getValue() == ConstantAPIKind::OSLog)
    return true;
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

// Autoclosure arguments are trivially constants.
static Expr *
checkFunctionLikeDecl(Expr *expr, ValueDecl *calledDecl,
                      SmallVectorImpl<Expr *> &expressionsToCheck) {
  assert(calledDecl && "calledDecl is null");
  if (!isa<EnumElementDecl>(calledDecl) &&
      !hasConstantEvaluableAttr(calledDecl))
    return expr;
  if (!calledDecl->hasParameterList())
    return nullptr; // Nothing more to check here.

  ParameterList *params = nullptr;
  if (auto *callee = dyn_cast<AbstractFunctionDecl>(calledDecl)) {
    params = callee->getParameters();
  } else if (auto *enumElem = dyn_cast<EnumElementDecl>(calledDecl)) {
    params = enumElem->getParameterList();
  }
  if (!params)
    return expr; // Unexpected decl found here.
  for (unsigned i = 0; i < params->size(); i++) {
    if (params->get(i)->isAutoClosure())
      continue;
    Expr *argExpr = getArgumentExpr(expr, i);
    if (!argExpr) {
      // There is a mismatch between the parameters and arguments?
      return argExpr;
    }
    expressionsToCheck.push_back(argExpr);
  }
  return nullptr;
}

Expr *swift::checkConstantnessOfArgument(Expr *argExpr, ConstraintSystem *cs) {
  SmallVector<Expr *, 4> expressionsToCheck;
  expressionsToCheck.push_back(argExpr);
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

    // If this is a member-ref, it has to be annotated constant evaluable.
    if (MemberRefExpr *memberRefExpr = dyn_cast<MemberRefExpr>(expr)) {
      ValueDecl *memberDecl = memberRefExpr->getMember().getDecl();
      if (!memberDecl || !hasConstantEvaluableAttr(memberDecl))
        return expr;
      continue;
    }
    // In the rest of the code handle dotSyntaxCall, enumElementAccess, property
    // access and function calls. In all cases, retrieve the "decl"
    // corresponding to the access and check if the decl is constant evaluable.
    // If so, check the arguments, if any, iteratively.

    ValueDecl *calledDecl = nullptr;

    // If it is a simple member access without arguments, just lookup the member
    // decl. Otherwise, try to determine the overload it resolves to.
    if (isa<UnresolvedMemberExpr>(expr) &&
        !cast<UnresolvedMemberExpr>(expr)->hasArguments()) {
      Type exprType = cs->simplifyType(cs->getType(expr))->getRValueType();
      LookupResult &result = cs->lookupMember(
          exprType, cast<UnresolvedMemberExpr>(expr)->getName());
      if (result.size() == 1)
        calledDecl = result.front().getValueDecl();
    } else {
      calledDecl = cs->findResolvedMemberRef(
          cs->getCalleeLocator(cs->getConstraintLocator(expr)));
    }
    if (!calledDecl) {
      llvm::errs() << "Cannot resolve member \n";
      llvm::errs() << "Type: "
                   <<
                   cs->simplifyType(cs->getType(expr))->getRValueType()
                   << "\n";
      return expr;
    }
    Expr *errorExpr =
        checkFunctionLikeDecl(expr, calledDecl, expressionsToCheck);
    if (errorExpr)
      return errorExpr;
    //    auto *params = callee->getParameters();
    //    for (unsigned i = 0; i < params->size(); i++) {
    //      // Autoclosure arguments are trivially constants.
    //      if (params->get(i)->isAutoClosure())
    //        continue;
    //      Expr *argExpr = getArgumentExpr(apply, i);
    //      if (!argExpr) {
    //        // This is a partial apply, which is an error.
    //        return argExpr;
    //      }
    //      expressionsToCheck.push_back(argExpr);
    //    }
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
void swift::diagnoseConstantnessViolation(Type typeOfConstant,
                                          SourceLoc errorLoc, FuncDecl *callee,
                                          ASTContext &ctx) {
  DiagnosticEngine &diags = ctx.Diags;
  Optional<ConstantAPIKind> kindOpt = getConstantAPIKind(callee);
  assert(kindOpt.hasValue() && "Unknown constant API kind");
  ConstantAPIKind kind = kindOpt.getValue();

  // Diagnose atomics ordering related error here.
  if (kind == ConstantAPIKind::AtomicOrdering) {
    NominalTypeDecl *nominalDecl =
        typeOfConstant->getNominalOrBoundGenericNominal();
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
  if (typeOfConstant->isBool()) {
    diags.diagnose(errorLoc, diag::oslog_arg_must_be_bool_literal);
    return;
  }
  if (isStringType(typeOfConstant)) {
    diags.diagnose(errorLoc, diag::oslog_arg_must_be_string_literal);
    return;
  }
  if (isIntegerType(typeOfConstant)) {
    diags.diagnose(errorLoc, diag::oslog_arg_must_be_integer_literal);
    return;
  }
  if (isFloatType(typeOfConstant)) {
    diags.diagnose(errorLoc, diag::oslog_arg_must_be_float_literal);
    return;
  }
  if (typeOfConstant->is<MetatypeType>()) {
    diags.diagnose(errorLoc, diag::oslog_arg_must_be_metatype_literal);
    return;
  }
  if (typeOfConstant->is<AnyFunctionType>()) {
    diags.diagnose(errorLoc, diag::oslog_arg_must_be_closure);
    return;
  }
  if (EnumDecl *enumDecl = typeOfConstant->getEnumOrBoundGenericEnum()) {
    diags.diagnose(errorLoc, diag::oslog_arg_must_be_enum_case,
                   enumDecl->getName());
    return;
  }
  NominalTypeDecl *nominalDecl =
      typeOfConstant->getNominalOrBoundGenericNominal();
  if (!nominalDecl) {
    // This case should normally not happen. This is a safe guard against
    // possible mismatch between the os overlay and the compiler.
    diags.diagnose(errorLoc, diag::argument_must_be_constant);
    return;
  }
  // If this is OSLogMessage, it should be a string-interpolation literal.
  Identifier declName = nominalDecl->getName();
  if (declName == ctx.Id_OSLogMessage) {
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
