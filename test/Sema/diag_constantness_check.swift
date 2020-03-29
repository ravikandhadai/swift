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
