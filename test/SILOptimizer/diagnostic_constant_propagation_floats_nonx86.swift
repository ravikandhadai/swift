// RUN: %target-swift-frontend -emit-sil -primary-file %s -o /dev/null -verify
//
// REQUIRES: !(CPU=i386 || CPU=x86_64)
//
// These are tests for diagnostics produced by constant propagation pass
// on floating-point operations that are specific to non-x86 architectures,
// which do not support Float80.

import StdlibUnittest

func testFPToIntConversion() {
  _blackHole(Int8(1E309)) // expected-error {{invalid conversion: '1E309' overflows 'Int8'}}
                          // expected-warning@-1 {{overflow: '1E309' exceeds limit, represented as inf}}

  _blackHole(UInt8(-1E309)) // expected-error {{negative literal '-1E309' cannot be converted to 'UInt8'}}
                            // expected-warning@-1 {{overflow: '-1E309' exceeds limit, represented as -inf}}

  _blackHole(Int64(1E309)) // expected-error {{invalid conversion: '1E309' overflows 'Int64'}}
                           // expected-warning@-1 {{overflow: '1E309' exceeds limit, represented as inf}}

  _blackHole(UInt64(-1E309)) // expected-error {{negative literal '-1E309' cannot be converted to 'UInt64'}}
                             // expected-warning@-1 {{overflow: '-1E309' exceeds limit, represented as -inf}}
}

func testFloatConvertOverflow() {
  let f1: Float = 1E309 // expected-warning {{overflow: '1E309' exceeds limit, represented as inf}}
  _blackHole(f1)
  let f2: Float32 = -1.0E999 // expected-warning {{overflow: '-1.0E999' exceeds limit, represented as -inf}}
  _blackHole(f2)
  _blackHole(Float(1E309)) // expected-warning {{overflow: '1E309' exceeds limit, represented as inf}}

  let d4: Double = 1E309 // expected-warning {{overflow: '1E309' exceeds limit, represented as inf}}
  _blackHole(d4)
  let d6: Float64 = -1.0E999 // expected-warning {{overflow: '-1.0E999' exceeds limit, represented as -inf}}
  _blackHole(d6)
  let d8: Float64 = -1.7976931348623159E+308 // expected-warning {{overflow: '-1.7976931348623159E+308' exceeds limit, represented as -inf}}
  _blackHole(d8)
  _blackHole(Double(1E309)) // expected-warning {{overflow: '1E309' exceeds limit, represented as inf}}
}

func testFloatConvertUnderflow() {
  // FIXME: in the following cases the numbers are so tiny that they underflow
  // even MaxBuiltinFloatType. The imprecision goes undetected in these cases.
  let f1: Float = 1E-400
  _blackHole(f1)
  _blackHole(Float(1E-400))

  let d2: Double = 1E-309
  _blackHole(d2)
  let d4: Double = 5E-324
  _blackHole(d4)

  _blackHole(Double(1E-309))
  _blackHole(Double(5E-324))
}

func testHexFloatImprecision() {
  _blackHole(Float(0x1.0000000000001p-1023)) // expected-warning {{'0x1.0000000000001p-1023' cannot be precisely represented by 'Float'}}

  // FIXME: in the following cases the numbers are so tiny that they underflow
  // even MaxBuiltinFloatType. The imprecision goes undetected in these cases.
  _blackHole(Float(0x1.00000000000001p-127))

  let d3: Double = 0x1.0000000000001p-1023
  _blackHole(d3)
  let d4: Double = 0x1.00000000000001p-1000
  _blackHole(d4)

  _blackHole(Double(0x1.0000000000001p-1023))
  _blackHole(Double(0x1.00000000000001p-1000))
}
