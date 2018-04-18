// RUN: %target-swift-frontend -O -emit-sil -primary-file %s -o /dev/null -verify
//
// Tests for checking the behavior of constant propagation diagnostics on
// `reportingOverflow`  and `unsafeAdding` APIs of integer arithmetic.
// In these cases,  there should be no static overflow diagnostics for the
// operations.

// Here the behavior of diagnostics is tested when there is inlining in the
// source code. Currently we can have false positives (see <rdar://39293788>)
func testWithInlining() {
  let min : Int8 = -128
  let zero : Int8 = 0
  let mone : Int8 = -1

  // FIXME: the following errors are false positives due to inlining
  var _ = testDiv(min, zero) // expected-error {{division by zero}}
  var _ = testDiv(min, mone) // expected-error {{division '-128 / -1' results in an overflow}}
}

//@inline(__always)
//func testDiv<T: FixedWidthInteger>(_ lhs: T, _ rhs: T) -> T {
//  //let (r, _) = lhs.dividedReportingOverflow(by: rhs)
//  let r = lhs / rhs
//  return r
//}

@inline(__always)
func testDiv(_ lhs: Int8, _ rhs: Int8) -> Int8 {
  //let (r, _) = lhs.dividedReportingOverflow(by: rhs)
  let r = lhs / rhs
  return r
}
