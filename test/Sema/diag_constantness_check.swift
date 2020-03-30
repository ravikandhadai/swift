// RUN: %target-typecheck-verify-swift -swift-version 5

// Tests for the diagnostics emitted in Sema for checking constantness of function
// arguments annotated to be so. This is currently accomplished through a
// @_semantics("requires_constant_<param1>_<param2>") annotation.

// Check simple literals.
@_semantics("requires_constant_constArg")
func constantArgumentFunction<T>(_ constArg: T) {
}

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
func constArgFunctionWithReturn<T>(_ constArg: T) -> T {
  return constArg
}

func testConstArgFuncWithReturn(x: Int, str: String) -> Int {
  _ = constArgFunctionWithReturn("")
  _ = constArgFunctionWithReturn(str)
    // expected-error@-1 {{argument 'constArg' must be a constant}}
  constArgFunctionWithReturn(10)
    // expected-warning@-1 {{result of call to 'constArgFunctionWithReturn' is unused}}
  return constArgFunctionWithReturn(x)
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

struct CustomStringInterpolation : ExpressibleByStringLiteral,
  ExpressibleByStringInterpolation {
  struct StringInterpolation : StringInterpolationProtocol {
    init(literalCapacity: Int, interpolationCount: Int) { }
    mutating func appendLiteral(_ x: String) { }

    @_semantics("requires_constant_x")
    mutating func appendInterpolation(_ x: Int) { }
  }
  init(stringLiteral value: String) { }
  init(stringInterpolation: StringInterpolation) { }
}

@_semantics("requires_constant_constArg")
func constantStringInterpolation(_ constArg: CustomStringInterpolation) {}

func testStringInterpolationLiteral(x: Int) {
  constantStringInterpolation("a string interpolation literal \(x)")
    // expected-error@-1 {{argument 'x' must be a constant}}
  constantStringInterpolation("a string interpolation literal \(10)")
}

// Test multiple arguments.
@_semantics("requires_constant_arg1_arg2_arg4")
func multipleArguments(_ arg1: Int, _ arg2: Bool, _ arg3: String, _ arg4: Double) {
}

func testMultipleArguments(_ x: String, _ y: Double) {
  multipleArguments(56, false, x, 23.3)
  multipleArguments(56, false, x, y)
    // expected-error@-1 {{argument 'arg4' must be a constant}}
}

// Test enum uses.
enum Color {
  case red
  case blue
  case green
  case rgb(r: Int, g: Int, b: Int)
}

@_semantics("requires_constant_color")
func enumArgument(_ color: Color) { }

func testEnumArgument(r: Int) {
  enumArgument(.rgb(r: 12, g: 0, b: 1))
  enumArgument(.green)
  enumArgument(.rgb(r: r, g: 200, b: 453))
    // expected-error@-1 {{argument 'color' must be a constant}}
    // expected-note@-2 {{expression not constant}}
}

// Test type expressions.
@_semantics("requires_constant_t")
func typeArgument<T>(_ t: T.Type) { }

func testTypeArgument<S>(_ t: S.Type) {
  typeArgument(Int.self)
  typeArgument(S.self)
  typeArgument(t)
   // expected-error@-1 {{argument 't' must be a constant}}
}

// Test constant evaluable function calls.
@_semantics("constant_evaluable")
func constantEval(_ x: Int, _ y: Bool) -> Int { x + 100 }

func testConstantEvalArgument(x: Int) {
  constantArgumentFunction(constantEval(90, true))
  constantArgumentFunction(constantEval(constantEval(500, true), false))
  constantArgumentFunction(constantEval(x, true))
    // expected-error@-1 {{argument 'constArg' must be a constant}}
    // expected-note@-2 {{expression not constant}}
}

// Test constant evaluable function calls with default arguments.
@_semantics("constant_evaluable")
func constantEvalAdvanced(_ x: () -> Int, _ y: Bool = true, z: String) { }

func testConstantEvalAdvanced(arg: Int) {
  constantArgumentFunction(constantEvalAdvanced({ arg }, z: ""))
}

// Test nested use of constant parameter.
@_semantics("requires_constant_constParam")
func testConstantArgumentRequirementPropagation(constParam: Int) {
  constantArgumentFunction(constParam)
}

// Test nested use of constant parameter in constant evaluable function.
@_semantics("requires_constant_constParam")
func testConstantArgumentWithConstEval(constParam: Int) {
  constantArgumentFunction(constantEval(constParam, true))
}

// Test struct and class constructions. Structs whose initializers are marked as
// constant_evaluable are considered as constants.

struct AStruct {
  var i: Int
}

struct BStruct {
  var i: Int
  @_semantics("constant_evaluable")
  init(_ value: Int) {
    i = value
  }
}

class CClass {
  var str: String
  init() {
    str = ""
  }
}

func testStructAndClasses(arg: Int) {
  constantArgumentFunction(AStruct(i: 9))
    // expected-error@-1 {{argument 'constArg' must be a constant}}
  constantArgumentFunction(BStruct(340))
  constantArgumentFunction(CClass())
    // expected-error@-1 {{argument 'constArg' must be a constant}}
}

// Test "requires_constant" annotation on protocol requirements.
protocol Proto {
  @_semantics("requires_constant_arg2")
  func method(arg1: Int, arg2: Bool)
}

struct SConf : Proto {
  func method(arg1: Int, arg2: Bool) { }
}

func testProtocolMethods<T: Proto>(b: Bool, p: T, p2:  Proto, s: SConf) {
  p.method(arg1: 6, arg2: true)
  p.method(arg1: 6, arg2: b)
    // expected-error@-1 {{argument 'arg2' must be a constant}}
  p2.method(arg1: 6, arg2: b)
    // expected-error@-1 {{argument 'arg2' must be a constant}}
  // Note that even though 's' conforms to Proto, since its method is not
  // annotated as requiring constant arg2, there will be no error here.
  s.method(arg1: 6, arg2: b)
}

// Check requiers annotation on a class method.
class ClassD {
  @_semantics("requires_constant_arg2")
  func method(_ arg1: Int, _ arg2: Bool)
}

func testClassMethod(d: ClassD, b: Bool) {
  d.method(10, true)
  d.method(10, b)
    // expected-error@-1 {{argument 'arg2' must be a constant}}
}

// Array and dictionary literals are not supported yet.
func testArrayAndDictionaryLiterals() {
  constantArgumentFunction([0, 1])
    // expected-error@-1 {{argument 'constArg' must be a constant}}
  constantArgumentFunction(["" : 2])
    // expected-error@-1 {{argument 'constArg' must be a constant}}
}

// Test that the check is resilient to errors in the semantics attribute.
@_semantics("requires_constant_y")
func funcWithWrongSemantics(x: Int) {}
@_semantics("requires_constant_")
func funcWithUnparseableSemantics(x: Int) {}

func testFunctionWithWrongSemantics(x: Int) {
  funcWithWrongSemantics(x: x)
  funcWithUnparseableSemantics(x: x)
}

// Test that the check is resilient to other type errors.
func testOtherTypeErrors() {
  constantArgumentFunction(x)
    // expected-error@-1 {{use of unresolved identifier 'x'}}
  constantArgumentFunction(10 as String)
    // expected-error@-1 {{cannot convert value of type 'Int' to type 'String' in coercion}}
}

// Test constantness of the ordering used in the atomic operations. The
// following code uses stub for ordering.
internal struct AtomicLoadOrderingStub {
  @_semantics("constant_evaluable")
  internal static var acquiring: Self { Self() }

  @_semantics("constant_evaluable")
  internal static var sequentiallyConsistent: Self { Self() }
}

internal struct UnsafeAtomicIntStub {
  @_semantics("requires_constant_ordering")
  internal func load(
    ordering: AtomicLoadOrderingStub = .sequentiallyConsistent
  ) -> Int {
    return 0
  }
}

func testAtomicOrderingConstantness(
  atomicInt: UnsafeAtomicIntStub,
  myOrder: AtomicLoadOrderingStub) {
  _ = atomicInt.load()
  _ = atomicInt.load(ordering: .acquiring)
  _ = atomicInt.load(ordering: .sequentiallyConsistent)
  _ = atomicInt.load(ordering: myOrder)
    // expected-error@-1 {{argument 'ordering' must be a constant}}
}
