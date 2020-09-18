// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-sil -primary-file %s -o %t/sil-output.sil
// RUN: %FileCheck %s < %t/sil-output.sil

@_semantics("constant_evaluable")
internal func leftShift(x: Int, y: Int) -> Int {
  return x &<< y
}

// CHECK-LABEL: @{{.*}}leftShiftUser
// CHECK: integer_literal $Builtin.Int64, 40
// CHECK-NOT: function_ref leftShift(x:y:)
internal func leftShiftUser() -> Int {
  return leftShift(x: 10, y: 2)
}

@_semantics("constant_evaluable")
internal func rightShift(x: Int, y: Int) -> Int {
  return x &>> y
}

// CHECK-LABEL: @{{.*}}rightShiftUser
// CHECK: integer_literal $Builtin.Int64, 2
// CHECK-NOT: function_ref rightShift(x:y:)
internal func rightShiftUser() -> Int {
  return rightShift(x: 10, y: 2)
}

@_semantics("constant_evaluable")
internal func rightShiftWithTraps(x: Int, y: Int) -> Int {
  return x >> y
}

// CHECK-LABEL: @{{.*}}interpretRightShiftWithTraps
// CHECK: integer_literal $Builtin.Int64, 4
// CHECK-NOT: function_ref rightShiftWithTraps(x:y:)
internal func interpretRightShiftWithTraps() -> Int {
  return rightShiftWithTraps(x: 34, y: 3)
}

@_semantics("constant_evaluable")
internal func arithmetic(x: Int, y: Int) -> Int {
  let z = x + y
  let w = x &+ z
  let a = w * y
  let b = a &* z
  let c = b - a
  let d = c &- x
  let e = d / x
  return e
}

// CHECK-LABEL: @{{.*}}interpretArithmetic
// CHECK-NOT: function_ref arithmetic
internal func interpretArithmetic() -> Int {
  return arithmetic(x: 142, y: 212)
}

//@_semantics("constant_evaluable")
//internal func testInvalidSingedUnsignedConversions(a: Int64) -> UInt64 {
//  return UInt64(a)
//}
//
//// CHECK-LABEL @interpretInvalidSingedUnsignedConversions
//internal func interpretInvalidSingedUnsignedConversions() -> UInt64 {
//  return testInvalidSingedUnsignedConversions(a: -130)
//}

@_semantics("constant_evaluable")
func testLoop() -> Int {
  var x = 0
  while x <= 42 {
    x += 1
  }
  return x
}

// CHECK-LABEL: @{{.*}}interpretLoop
func interpretLoop() -> Int {
  return testLoop()
}
