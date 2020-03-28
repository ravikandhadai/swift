// RUN: %target-typecheck-verify-swift -swift-version 5

// Tests for the diagnostics emitted in Sema for the new os log APIs.

import OSLogTestHelper

func testDynamicLogMessage(message: OSLogMessage) {
  _osLogTestHelper(message)
    // expected-error@-1 {{argument 'message' must be a constant}}
}

func testNonconstantFormatOption(
  formatOpt: OSLogIntegerFormatting,
  explicitPositiveSign: Bool) {
  _osLogTestHelper("Minimum integer value: \(Int.min, format: formatOpt)")
    // expected-error@-1 {{argument 'format' must be a constant}}

  let uppercase = true
  _osLogTestHelper("\(UInt.max, format: .hex(uppercase: uppercase))")
    // expected-error@-1 {{argument 'format' must be a constant}}
    // expected-note@-2 {{expression not constant}}

  _osLogTestHelper("\(UInt.max, format: .hex)") // No error is expected here.
}

func testNonconstantPrivacyOption(privacyOpt: OSLogPrivacy) {
  _osLogTestHelper("Integer: \(Int.max, privacy: privacyOpt)")
    // expected-error@-1 {{argument 'privacy' must be a constant}}
}

func testNonconstantAlignmentOption(alignOpt: OSLogStringAlignment) {
  _osLogTestHelper("Integer: \(Int.max, align: alignOpt)")
    // expected-error@-1 {{argument 'align' must be a constant}}
}

func testMultipleOptions(
  formatOpt: OSLogIntegerFormatting,
  privacyOpt: OSLogPrivacy
) {
  _osLogTestHelper(
    """
    \(2, format: formatOpt, align: .right(columns: 10), privacy: privacyOpt)
    """)
    // expected-error@-2 {{argument 'format' must be a constant}}
    // expected-error@-3 {{argument 'privacy' must be a constant}}
}

func testNoninlinedOSLogMessage() {
  let logMessage: OSLogMessage = "Minimum integer value: \(Int.min)"
  _osLogTestHelper(logMessage)
    // expected-error@-1 {{argument 'message' must be a constant}}
}

let globalLogMessage: OSLogMessage = "A global message"

func testGlobalLogMessage() {
  _osLogTestHelper(globalLogMessage)
    // expected-error@-1 {{argument 'message' must be a constant}}
}

// No errors are expected here.
func testValidLogCalls(x: Int) {
  _osLogTestHelper("\(x, format: .hex, privacy: .private)")
  _osLogTestHelper("\(x, format: OSLogIntegerFormatting.hex, privacy: .public)")
  _osLogTestHelper("\(x, privacy: OSLogPrivacy.public)")
  _osLogTestHelper("\((x + 1) * 2, privacy: .public)")
}


// Check whether os-log-specific diagnostics do not crash when there
// are type errors.
func testTypeIncorrectLogCalls() {
  let message = "test message"

  _osLogTestHelper(message)
  // expected-error@-1 {{cannot convert value of type 'String' to expected argument type 'OSLogMessage'}}
  _osLogTestHelper("prefix" + "\(x)")
  // expected-error@-1 {{cannot convert value of type 'String' to expected argument type 'OSLogMessage'}}
  _osLogTestHelper("prefix", "\(x)")
  // expected-error@-1 {{cannot convert value of type 'String' to expected argument type '(String, UnsafeBufferPointer<UInt8>) -> Void'}}
  // expected-error@-2 {{missing argument label 'assertion:' in call}}

  class TestClass {
  }
  let x = TestClass()
  _osLogTestHelper("\(x, format: .hex)")
  //expected-error@-1 {{no exact matches in call to instance method 'appendInterpolation'}}

  _osLogTestHelper("\(10, format: .myFormat, privacy: .private)")
  //expected-error@-1 {{type 'OSLogIntegerFormatting' has no member 'myFormat'}}
}

// Test diagnostics in extensions to OSLogInterpolation. This is not officially
// supported yet.
struct A {
  var i: Int
}

extension OSLogInterpolation {
  mutating func appendInterpolation(a: A) {
    self.appendInterpolation(a.i)
  }

  mutating func appendInterpolation(a: A, format: OSLogIntegerFormatting) {
    self.appendInterpolation(a.i, format: format)
      // expected-error@-1 {{argument 'format' must be a constant}}
  }

  @_semantics("requires_constant_constFormat")
  mutating func appendInterpolation(a: A, constFormat: OSLogIntegerFormatting) {
    self.appendInterpolation(a.i, format: constFormat)
  }
}

func testOSLogInterpolationExtension(a: A) {
  // The following is not a Sema error but would result in a SIL diagnostics as
  // the appendInterpolation overload is not marked as constant_evaluable.
  _osLogTestHelper("Error at line: \(a: a)")
}

