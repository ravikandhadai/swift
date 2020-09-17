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

