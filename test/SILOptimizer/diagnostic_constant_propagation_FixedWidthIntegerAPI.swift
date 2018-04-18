// RUN: %target-swift-frontend -emit-sil -primary-file %s -o /dev/null -verify
//
// Tests for checking the behavior of constant propagation diagnostics on
// `reportingOverflow`  and `unsafeAdding` APIs of integer arithmetic.
// In these cases,  there should be no static overflow diagnostics for the
// operations.

func testReportingOverflowInt8() {
  let zero : Int8 = 0
  let one : Int8 = 1
  let two : Int8 = 2
  let mone : Int8 = -1
  let max8 : Int8 = 127
  let min8 : Int8 = -128

  var _ = one.addingReportingOverflow(one)
  var _ = one.addingReportingOverflow(max8)

  var _ = min8.dividedReportingOverflow(by: mone)
    // FIXME: false positive: <rdar://39293788>
    // Need to disable overflow reporting

  var _ = min8.dividedReportingOverflow(by: zero)
    // FIXME: false positive: <rdar://39293788>
    // Need to disable divide-by-zero diagnostics.
    // Divide-by-zero is not an error under this API.

  var _ = UInt8.max.dividedReportingOverflow(by: 0)

  var _ = mone.dividingFullWidth((-1, 0x80))
    // This is equivalent to -128/-1 (an overflow).
    // There is no static or runtime check in this case.
    // TODO: should we statically diagnose these errors? what does the language
    // spec require in this case?

  var _ = mone.dividingFullWidth((-1, 0x81))
    // This is equivalent to -127/-1 (no overflow)

  var _ = two.dividingFullWidth((0, 255))

  var _ = two.dividingFullWidth((0, 256)) // expected-error {{integer literal '256' overflows when stored into 'Int8.Magnitude' (aka 'UInt8')}}

  var _ = zero.dividingFullWidth((0, 255))
    // FIXME: <rdar://39294829> False negative:
    // diagnose this divide-by-zero error.

  var _ = min8.multipliedFullWidth(by: max8)

  var _ = min8.multipliedReportingOverflow(by: 2)

  var _ = min8.remainderReportingOverflow(dividingBy: mone)
  // FIXME: false positive: <rdar://39293788> Need to disable overflow reporting

  var _ = min8.subtractingReportingOverflow(one)
  var _ = max8.subtractingReportingOverflow(mone)
}

// FIXME: <rdar://39295527> false negatives: missing overflow diagnostics when
// using `unsafe` APIs. Overflows in unsafe APIs are undefined behavior.
func testUnsafeArithmeticInt8() {
  let zero : Int8 = 0
  let one : Int8 = 1
  let mone : Int8 = -1
  let max8 : Int8 = 127
  let min8 : Int8 = -128

  var _ = max8.unsafeAdding(one) // see the FIXME above
  var _ = min8.unsafeAdding(mone) // see the FIXME above

  var _ = min8.unsafeDivided(by: mone) // expected-error {{division '-128 / -1' results in an overflow}}
  var _ = min8.unsafeDivided(by: zero) // expected-error {{division by zero}}

  var _ = min8.unsafeMultiplied(by: min8) // see the FIXME above
  var _ = min8.unsafeSubtracting(one) // see the FIXME above
}

func testBoundedArithmeticInt8() {
  let one : Int8 = 1
  let mone : Int8 = -1
  let max8 : Int8 = 127
  let min8 : Int8 = -128

  var _ = max8 &+ one
  var _ = min8 &+ mone
  var _ = min8 &* min8
  var _ = min8 &- one
}

func testReportingOverflowInt64() {
  let zero : Int64 = 0
  let one : Int64 = 1
  let mone : Int64 = -1
  let two : Int64 = 2
  let max64 : Int64 = Int64.max
  let min64 : Int64 = Int64.min

  var _ = one.addingReportingOverflow(one)
  var _ = one.addingReportingOverflow(max64)

  var _ = min64.dividedReportingOverflow(by: mone)
    // FIXME: false positive: <rdar://39293788>
    // Need to disable overflow reporting

  var _ = min64.dividedReportingOverflow(by: zero)
    // FIXME: false positive: <rdar://39293788>
    // Need to disable divide-by-zero diagnostics.
    // Divide-by-zero is not an error under this API.

  var _ = UInt64.max.dividedReportingOverflow(by: 0)

  var _ = mone.dividingFullWidth((-1, 0x8000_0000_0000_0000))
    // This is equivalent to Int64.min/-1 (an overflow).
    // There is no static or runtime check in this case.
    // TODO: should we statically diagnose these errors? what does the language
    // spec require in this case?

  var _ = mone.dividingFullWidth((-1, 0x8000_0000_0000_0001))

  var _ = two.dividingFullWidth((0, 0xffff_ffff_ffff_ffff))

  var _ = two.dividingFullWidth((0, UInt64.max + 1)) // expected-error {{arithmetic operation '18446744073709551615 + 1' (on type 'UInt64') results in an overflow}}

  var _ = zero.dividingFullWidth((0, 0xffff_ffff_ffff_ffff))
    // FIXME: <rdar://39294829> False negative:
    // diagnose this divide-by-zero error.

  var _ = min64.multipliedFullWidth(by: max64)

  var _ = min64.multipliedReportingOverflow(by: 2)

  var _ = min64.remainderReportingOverflow(dividingBy: mone)
  // FIXME: false positive: <rdar://39293788> Need to disable overflow reporting

  var _ = min64.subtractingReportingOverflow(one)
  var _ = max64.subtractingReportingOverflow(mone)
}

// FIXME: <rdar://39295527> false negatives: missing overflow diagnostics when
// using `unsafe` APIs. Overflows in unsafe APIs are undefined behavior.
func testUnsafeArithmeticInt64() {
  let zero : Int64 = 0
  let one : Int64 = 1
  let mone : Int64 = -1
  let max64 : Int64 = Int64.max
  let min64 : Int64 = Int64.min

  var _ = max64.unsafeAdding(one) // see the FIXME above
  var _ = min64.unsafeAdding(mone) // see the FIXME above

  var _ = min64.unsafeDivided(by: mone) // expected-error {{division '-9223372036854775808 / -1' results in an overflow}}
  var _ = min64.unsafeDivided(by: zero) // expected-error {{division by zero}}

  var _ = min64.unsafeMultiplied(by: min64) // see the FIXME above
  var _ = min64.unsafeSubtracting(one) // see the FIXME above
}

func testBoundedArithmeticInt64() {
  let one : Int64 = 1
  let mone : Int64 = -1
  let max64 : Int64 = Int64.max
  let min64 : Int64 = Int64.min

  var _ = max64 &+ one
  var _ = min64 &+ mone
  var _ = min64 &* min64
  var _ = min64 &- one
}
