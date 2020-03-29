// RUN: %target-typecheck-verify-swift -swift-version 5

// Tests for the diagnostics emitted in Sema for checking constantness of function
// arguments annotated to be so. This is currently accomplished through a
// @_semantics("requires_constant_<param1>_<param2>") annotation.

// Check simple literals.

@_semantics("requires_constant_constArg")
func constantArgumentFunction<T>(_ constArg: T) {
}

// Correct test cases.
func literalTest(x: Int) {
  constantArgumentFunction(1)
  constantArgumentFunction("Some string")
  constantArgumentFunction(1.9)
  constantArgumentFunction(true)
  constantArgumentFunction(x)
    // expected-error@-1 {{argument 'constArg' must be a constant}}
  constantArgumentFunction(x + 2)
    // expected-error@-1 {{argument 'constArg' must be a constant}}
}

@_semantics("requires_constant_constArg")
func constantOptionalArgument(_ constArg: Optional<Int>) {
}

// Correct test cases.
func optionalTest(x: Int) {
  constantOptionalArgument(nil)
  constantOptionalArgument(0)
  constantArgumentFunction(x + 2)
    // expected-error@-1 {{argument 'constArg' must be a constant}}
}

// Test string interpolation literals. We can only enforce constantness on custom string
// interpolation types. For string types, the constant is a string literal.

struct CustomStringInterpolation {

}

// Test enum uses.

// Test type expressions.

// Test constant evaluable function calls.

// Test constant evaluable function calls with default arguments.

// Test nested use of constant parameter.

// Test use of constant parameter in constant evaluable function.

// Struct and class constructions are not supported yet.

// Array and dictionary literals are not supported yet.

// Test that the check is resilient to errors in the semantics attribute.

// Test that the check is resilient to other type errors.

// Test constantness of the ordering used in the atomic operations.
