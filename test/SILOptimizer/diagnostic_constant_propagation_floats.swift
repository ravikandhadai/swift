// RUN: %target-swift-frontend -emit-sil -primary-file %s -o /dev/null -verify
//
// These are tests for diagnostics produced by constant propagation pass
// on floating-point operations

import StdlibUnittest

func testFPToIntConversion() {
  _blackHole(Int8(-1.28E2))
  _blackHole(Int8(-1.27E2))
  _blackHole(Int8(-0))
  _blackHole(Int8(3.33333))
  _blackHole(Int8(-2E2)) // expected-error {{floating-point literal '-200' overflows 8-bit signed integer type}}

  _blackHole(UInt8(2E2))
  _blackHole(UInt8(3E2)) // expected-error {{floating-point literal '300' overflows 8-bit unsigned integer type}}
  _blackHole(UInt8(-0E0))
  _blackHole(UInt8(-2E2)) // expected-error {{cannot convert negative floating-point literal '-200' to unsigned integer type}}

  _blackHole(Int16(3.2767E4))
  _blackHole(Int16(3.2768E4)) // expected-error {{floating-point literal '32768' overflows 16-bit signed integer type}}
  _blackHole(Int16(-4E4)) // expected-error {{floating-point literal '-4.0E+4' overflows 16-bit signed integer type}}

  _blackHole(UInt16(6.5535E4))
  _blackHole(UInt16(6.5536E4)) // expected-error {{floating-point literal '65536' overflows 16-bit unsigned integer type}}0
  _blackHole(UInt16(7E4)) // expected-error {{floating-point literal '7.0E+4' overflows 16-bit unsigned integer type}}
  _blackHole(UInt16(-0E0))
  _blackHole(UInt16(-2E2)) // expected-error {{cannot convert negative floating-point literal '-200' to unsigned integer type}}

  _blackHole(Int32(-2.147483648E9))
  _blackHole(Int32(-2.147483649E9)) // expected-error {{floating-point literal '-2147483649' overflows 32-bit signed integer type}}
  _blackHole(Int32(3E9)) // expected-error {{floating-point literal '3.0E+9' overflows 32-bit signed integer type}}

  _blackHole(UInt32(4.294967295E9))
  _blackHole(UInt32(4.294967296E9)) // expected-error {{floating-point literal '4294967296' overflows 32-bit unsigned integer type}}
  _blackHole(UInt32(5E9)) // expected-error {{floating-point literal '5.0E+9' overflows 32-bit unsigned integer type}}
  _blackHole(UInt32(-0E0))
  _blackHole(UInt32(-2E2)) // expected-error {{cannot convert negative floating-point literal '-200' to unsigned integer type}}

  _blackHole(Int64(9.223372036854775E18))

  // A case where lost precision in double can result in overflow
  // in conversion to int.
  _blackHole(Int64(9.223372036854775807E18)) // expected-error {{floating-point literal '9.2233720368547758E+18' overflows 64-bit signed integer type}}
  let i64max: Float80 = 9.223372036854775807E18
  _blackHole(Int64(i64max))

  // Cases of definite overflow.
  _blackHole(Int64(9.223372036854775808E18)) // expected-error {{floating-point literal '9.2233720368547758E+18' overflows 64-bit signed integer type}}
  let j: Float80 = 9.223372036854775808E18
  _blackHole(Int64(j)) // expected-error {{floating-point literal '9223372036854775808' overflows 64-bit signed integer type}}
  _blackHole(Int64(1E19)) // expected-error {{floating-point literal '1.0E+19' overflows 64-bit signed integer type}}

  // A case where lost precision in double can result in overflow
  // in conversion to unsigned int.
  _blackHole(UInt64(1.844674407370955E19))
  _blackHole(UInt64(1.8446744073709551615E19)) // expected-error {{floating-point literal '1.8446744073709552E+19' overflows 64-bit unsigned integer type}}
  let u64max: Float80 = 1.8446744073709551615E19
  _blackHole(UInt64(u64max))

  let uj: Float80 = 1.8446744073709551616E19
  _blackHole(UInt64(uj)) // expected-error {{floating-point literal '18446744073709551616' overflows 64-bit unsigned integer type}}

  _blackHole(UInt64(2E19)) // expected-error {{floating-point literal '2.0E+19' overflows 64-bit unsigned integer type}}
  _blackHole(UInt64(-0E0))
  _blackHole(UInt64(-2E2)) // expected-error {{cannot convert negative floating-point literal '-200' to unsigned integer type}}
}
