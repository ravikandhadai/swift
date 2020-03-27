// RUN: %target-typecheck-verify-swift -swift-version 5

// Tests for the diagnostics emitted in Sema for the new os log APIs.

let assertion = { (buffer: UnsafePointer<UInt8>, formatString: String) in
  return
}

func testDynamicLogMessage(message: OSLogMessage) {
  _checkFormatStringAndBuffer(message, assertion)
  // expected-error@-1 {{argument 'message' must be a constant}}
}

func testNonconstantFormatOption(h: Logger, formatOpt: OSLogIntegerFormatting) {
  _checkFormatStringAndBuffer(
    "Minimum integer value: \(Int.min, format: formatOpt)",
    assertion)
  // expected-error@-1 {{argument 'format' must be a constant}}
}

func testNonconstantPrivacyOption(h: Logger,  privacyOpt: OSLogPrivacy) {
  _checkFormatStringAndBuffer(
    "Integer: \(Int.max, privacy: privacyOpt)",
    
  )
  // expected-error@-1 {{argument 'privacy' must be a constant}}
}

func testNonconstantAlignmentOption(h: Logger,  privacyOpt: OSLogPrivacy) {
  h.log(level: .debug, "Integer: \(Int.max, privacy: privacyOpt)")
  // expected-error@-1 {{argument 'privacy' must be a constant}}
}

func testNoninlinedOSLogMessage(h: Logger) {
  let logMessage: OSLogMessage = "Minimum integer value: \(Int.min)"
  h.log(level: .debug, logMessage)
  // @-1 {{log message must be a string literal or string interpolation}}
}
//
//  let globalLogMessage: OSLogMessage = "A global message"
//
//  func testNoninlinedOSLogMessageComplex(h: Logger, b: Bool) {
//    h.log(level: .debug, globalLogMessage)
//    // @-1 {{log message must be a string literal or string interpolation}}
//
//    let logMessage: OSLogMessage = "Maximum integer value: \(Int.max)"
//    if !b {
//      return;
//    }
//    h.log(level: .debug, logMessage)
//    // @-1 {{log message must be a string literal or string interpolation}}
//  }
//
//  // No errors are expected here.
//  func testValidLogCalls(h: Logger, x: Int) {
//    h.log("\(x, format: .hex, privacy: .private)")
//    h.log("\(x, format: IntFormat.hex, privacy: .public)")
//    h.log("\(x, privacy: Privacy.public)")
//    h.log(level: .debug, "\((x + 1) * 2, privacy: .public)")
//  }
//
//  // Check whether os-log-specific diagnostics do not crash when there
//  // are type errors.
//  func testTypeIncorrectLogCalls(h: Logger) {
//    let message = "test message"
//
//    h.log(message)
//    // @-1 {{cannot convert value of type 'String' to expected argument type 'OSLogMessage'}}
//    h.log("prefix" + "\(x)")
//    // @-1 {{cannot convert value of type 'String' to expected argument type 'OSLogMessage'}}
//    h.log("prefix", "\(x)")
//    // @-1 {{missing argument label 'level:' in call}}
//
//    class TestClass {
//    }
//    let x = TestClass()
//    h.log("\(x, format: .hex)")
//    //@-1 {{cannot convert value of type 'TestClass' to expected argument type}}
//
//    h.log("\(10, format: .myFormat, privacy: .private)")
//    // @-1 {{type 'IntFormat' has no member 'myFormat'}}
//  }

//
//// Extensions to OSLogInterpolation are not supported as yet, but will be
//// supported in the future.
//extension OSLogInterpolation {
//   mutating func appendInterpolation(line: Int) {
//     self.appendInterpolation(line)
//   }
//}
//
//@available(OSX 10.12, iOS 10.0, watchOS 3.0, tvOS 10.0, *)
//func testOSLogInterpolationExtension(h: Logger, line: Int) {
//  h.log(level: .debug, "Error at line: \(line: line)")
//  // @-1 {{externally defined interpolation is not supported in os log}}
//}
