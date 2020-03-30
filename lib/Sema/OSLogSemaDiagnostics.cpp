//===--- OSLogSemaDiagnostics.cpp - AST-Level Diagnostics -----------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file implements AST-level diagnostics for the os log APIs based on
// string interpolation. It implements the function `diagnoseOSLogUsageErrors`
// declared in MiscDiagnostics.h.
//
//===----------------------------------------------------------------------===//

#include "MiscDiagnostics.h"
#include "TypeChecker.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/ParameterList.h"
using namespace swift;

static bool hasConstantEvaluableAttr(ValueDecl *decl) {
  for (auto semantics : decl->getAttrs().getAttributes<SemanticsAttr>()) {
    if (semantics->Value.equals("constant_evaluable")) {
      return true;
    }
  }
  return false;
}

static void getConstantArgumentNames(FuncDecl *funcDecl,
                            SmallVectorImpl<StringRef> &constantArgumentNames) {
  StringRef prefix = StringRef("requires_constant_");
  Optional<StringRef> requiresConstantSemanticsAttr = None;
  for (auto semantics : funcDecl->getAttrs().getAttributes<SemanticsAttr>()) {
    if (semantics->Value.startswith(prefix)) {
      requiresConstantSemanticsAttr = semantics->Value;
      break;
    }
  }
  if (!requiresConstantSemanticsAttr)
    return;

  StringRef unparsedSuffix =
    requiresConstantSemanticsAttr->drop_front(prefix.size());
  while (!unparsedSuffix.empty()) {
    auto pair = unparsedSuffix.split('_');
    constantArgumentNames.push_back(pair.first);
    unparsedSuffix = pair.second;
  }
  return;
}

/// Check whether \p expr is constant valued i.e., it  uses only literals or
/// enum elements whose arguments are constant valued.
static Expr * checkConstantness(Expr *expr) {
  SmallVector<Expr *, 4> expressionsToCheck;
  expressionsToCheck.push_back(expr);
  while (!expressionsToCheck.empty()) {
    Expr *expr = expressionsToCheck.pop_back_val();
//    llvm::errs() << "Looking at expression: \n";
//    expr->dump();
//    llvm::errs() << "\n";
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
    if (InjectIntoOptionalExpr *optionalExpr
          = dyn_cast<InjectIntoOptionalExpr>(expr)) {
      expressionsToCheck.push_back(optionalExpr->getSubExpr());
      continue;
    }
    // Literal expressions also includes InterpolatedStringLiteralExpr.
    if (isa<LiteralExpr>(expr))
      continue;
    if (isa<TypeExpr>(expr))
      continue;

    // If this is a member-ref, it has to be annotated constant evaluable.
    if (MemberRefExpr *memberRef = dyn_cast<MemberRefExpr>(expr)) {
      if (ValueDecl *memberDecl = memberRef->getMember().getDecl()) {
        if (hasConstantEvaluableAttr(memberDecl))
          continue;
      }
      return expr;
    }

    // If this is a variable, it has to be a constant parameter of the enclosing
    // function. In principle, a local variable that is defined by a constant expression
    // can be supported, but that requires folding constant expression in all contexts,
    // which is not supported yet.
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
      if (!funcDecl)
        return expr;
      SmallVector<StringRef, 4> constantArgumentNames;
      getConstantArgumentNames(funcDecl, constantArgumentNames);
      auto findIter =
        llvm::find(constantArgumentNames, paramDecl->getParameterName().str());
      if (findIter == constantArgumentNames.end())
        return expr;
      // Here, the parameter is declared as a constant argument.
      continue;
    }

    if (!isa<ApplyExpr>(expr))
      return expr;

    ApplyExpr *apply = cast<ApplyExpr>(expr);
    ValueDecl *calledValue = apply->getCalledValue();
    if (!calledValue)
      return expr;

    // If this is an enum case, check whether the arguments are constants.
    if (isa<EnumElementDecl>(calledValue)) {
      expressionsToCheck.push_back(apply->getArg());
      continue;
    }

    // If this is a constant_evaluable function, check whether the arguments
    // are constants. Here, we can skip the default argument expressions as
    // the default arguments of a constant_evaluable function must be a
    // constant.
    AbstractFunctionDecl *callee = dyn_cast<AbstractFunctionDecl>(calledValue);
    if (!callee || !hasConstantEvaluableAttr(callee))
      return expr;

    //llvm::errs() << "found constant evaluable expression \n";

    SmallVector<Expr *, 4> argumentExprs;
    Expr *uncurriedArguments = apply->getArg();
    if (auto *tupleExpr = dyn_cast<TupleExpr>(uncurriedArguments)) {
      for (Expr *element : tupleExpr->getElements())
        argumentExprs.push_back(element);
    } else {
      argumentExprs.push_back(uncurriedArguments);
    }
    // Skip default argument expressions. Also skip closure expressions, which
    // are always treated as constants in a constant-evaluable function.
    auto nondefaultArgs =
      llvm::make_filter_range(argumentExprs, [&](Expr *expr) {
        return !isa<DefaultArgumentExpr>(expr) &&
          !isa<AbstractClosureExpr>(expr);
      });
    for (Expr *argument : nondefaultArgs)
      expressionsToCheck.push_back(argument);
  }
  return nullptr;
}

static void diagnoseConstantArgumentRequirementOfCall(const CallExpr *callExpr,
                                                 const ASTContext& ctx) {
  assert(callExpr && callExpr->getType());
//  llvm::errs() << "Looking at expr: \n";
//  expr->dump();
//  llvm::errs() << "\n";
  DiagnosticEngine &diags = ctx.Diags;

  // Is expr a direct call with semantics attribute "requires_constant_argument"?
  ValueDecl *calledDecl = callExpr->getCalledValue();
  if (!calledDecl || !isa<FuncDecl>(calledDecl))
    return;
  FuncDecl *callee = cast<FuncDecl>(calledDecl);
  SmallVector<StringRef, 4> constantArgumentNames;
  // Extract arguments that are required to be constants, if any.
  getConstantArgumentNames(callee, constantArgumentNames);
  // Give up if the argument names are empty or do not match the function
  // argument names. We should not diganose in those cases as the semantics
  // attribute on the definition of the function is wrong. The caller is not to
  // blame.
  if (constantArgumentNames.empty())
    return;

  SmallVector<unsigned, 4> constantArgumentIndices;
  auto paramList = callee->getParameters();
  for (unsigned i = 0; i < paramList->size(); i++) {
    ParamDecl *param = paramList->get(i);
    auto findIter =
      llvm::find(constantArgumentNames, param->getParameterName().str());
    if (findIter != constantArgumentNames.end()) {
      constantArgumentIndices.push_back(i);
    }
  }
  if (constantArgumentIndices.empty())
    return;

  auto diagnose = [&](Expr *argument, Identifier paramName, Expr *errorExpr) {
    diags.diagnose(argument->getLoc(), diag::argument_not_constant, paramName);
    // Point to the error if it is inside the argument expression.
    if (errorExpr != argument) {
      diags.diagnose(errorExpr->getLoc(), diag::expression_not_constant);
    }
  };

  // Check that the arguments at the constantArgumentIndices are constants.
  Expr *argumentExpr = callExpr->getArg();
  SmallVector<Expr *, 4> arguments;
  if (TupleExpr *tupleExpr = dyn_cast<TupleExpr>(argumentExpr)) {
    auto elements = tupleExpr->getElements();
    arguments.append(elements.begin(), elements.end());
  } else if (ParenExpr *parenExpr = dyn_cast<ParenExpr>(argumentExpr)) {
    arguments.push_back(parenExpr->getSubExpr());
  } else {
    arguments.push_back(argumentExpr);
  }

  for (unsigned constantIndex : constantArgumentIndices) {
    assert (constantIndex < arguments.size() &&
            "constantIndex exceeds the number of arguments to the function.");
    Expr *argument= arguments[constantIndex];
    // Skip the constantness check for default argument expressions as it is
    // the libraries responsibility to ensure that it is a constant, when
    // necessary.
    if (isa<DefaultArgumentExpr>(argument))
      continue;
    Expr *errorExpr = checkConstantness(argument);
    if (errorExpr) {
      diagnose(argument, paramList->get(constantIndex)->getParameterName(),
               errorExpr);
    }
  }
  return;
}

void swift::diagnoseConstantArgumentRequirement(const Expr *expr,
                                           const DeclContext *declContext) {
  class ConstantReqCallWalker : public ASTWalker {
    const ASTContext &astContext;

  public:
    ConstantReqCallWalker(ASTContext &ctx) : astContext(ctx) {}

    // Descend until we find a call expressions. Note that the input expression
    // could be an assign expression or another expression that contains the call.
    std::pair<bool, Expr *> walkToExprPre(Expr *expr) override {
      if (!expr || isa<ErrorExpr>(expr) || !expr->getType())
        return { false, expr };
      if (auto *callExpr = dyn_cast<CallExpr>(expr)) {
        diagnoseConstantArgumentRequirementOfCall(callExpr, astContext);
        return {false, expr};
      }
      return {true, expr};
    }
  };

  ConstantReqCallWalker walker(declContext->getASTContext());
  const_cast<Expr *>(expr)->walk(walker);
}


//  Expr *logMessage = nullptr;
//  bool logMessageIsLiteral = false;
//
//  if (auto *tupleExpr = dyn_cast<TupleExpr>(argumentExpr)) {
//    ArrayRef<Expr *> elements = tupleExpr->getElements();
//    for (Expr *element : elements) {
//      if (isa<StringLiteralExpr>(element) ||
//          isa<InterpolatedStringLiteralExpr>(element)) {
//        assert(!logMessage &&
//               "os log must accept only one string literal or interpolation");
//        logMessageIsLiteral = true;
//        logMessage = element;
//      }
//    }
//    if (!logMessage) {
//      // If there are no literals in the call, set the log message to the last
//      // argument, which is only used to emit diagnostics.
//      logMessage = elements[elements.size() - 1];
//    }
//  } else {
//    logMessage = argumentExpr;
//    if (isa<StringLiteralExpr>(logMessage) ||
//        isa<InterpolatedStringLiteralExpr>(logMessage)) {
//      logMessageIsLiteral = true;
//    }
//  }
//  assert(logMessage);
//
//  if (!logMessageIsLiteral) {
//    astContext.Diags.diagnose(logMessage->getLoc(), diag::os_log_message_not_literal);
//    return;
//  }
//
//  assert(isa<StringLiteralExpr>(logMessage) ||
//         isa<InterpolatedStringLiteralExpr>(logMessage));
//  if (isa<StringLiteralExpr>(logMessage))
//    return; // Nothing to check for string literals.
//
//  InterpolatedStringLiteralExpr *interpolation =
//      cast<InterpolatedStringLiteralExpr>(logMessage);
//
//  interpolation->forEachSegment(
//      DC->getASTContext(),
//      [&](bool isAppendInterpolation, CallExpr *appendCall) {
//        if (!isAppendInterpolation || !appendCall->getType())
//          return;
//
//        ValueDecl *calledValue = appendCall->getCalledValue();
//        assert(calledValue);
//
//        // Reject user-defined appendInterpolation extensions. These will be
//        // supported in the future but not yet.
//        auto *appendCalleeDecl = dyn_cast<FuncDecl>(calledValue);
//        assert(appendCalleeDecl &&
//               "os log's appendInterpolation must be a function");
//
//        if (!appendCalleeDecl->getModuleContext()->isSystemModule()) {
//          astContext.Diags.diagnose(appendCall->getLoc(),
//                      diag::os_log_external_append_interpolation);
//          return;
//        }
//
//        // Check whether every argument to `appendInterpolation` except the
//        // first is a constant-valued expression.
//        Expr *appendArg = appendCall->getArg();
//        if (!isa<TupleExpr>(appendArg))
//          return; // There is just one argument here.
//
//        ArrayRef<Expr *> interpolationArgs =
//            cast<TupleExpr>(appendArg)->getElements();
//
//        assert(appendCalleeDecl->getParameters()->size() >=
//               interpolationArgs.size());
//
//        for (unsigned i = 1; i < interpolationArgs.size(); i++) {
//          Expr *interpolationArg = interpolationArgs[i];
//          Optional<Expr *> errorExpr =
//              isConstantValuedOSLogArgument(interpolationArg);
//          if (errorExpr.hasValue()) {
//            ParamDecl *errorParam = appendCalleeDecl->getParameters()->get(i);
//            StringRef errorParamName = errorParam->getNameStr();
//            astContext.Diags.diagnose(interpolationArg->getLoc(),
//                        diag::os_log_interpolation_argument_nonconstant,
//                        errorParamName, errorParam->hasName());
//            astContext.Diags.diagnose(errorExpr.getValue()->getLoc(),
//                        diag::os_log_interpolation_argument_nonconstant_note);
//          }
//        }
//      });
