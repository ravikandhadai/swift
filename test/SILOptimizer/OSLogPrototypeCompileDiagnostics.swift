// RUN: %target-swift-frontend -swift-version 5 -emit-sil -primary-file %s -o /dev/null -verify
// REQUIRES: OS=macosx || OS=ios || OS=tvos || OS=watchos

// Tests for the diagnostics produced by the OSLogOptimization pass that
// performs compile-time analysis and optimization of the new os log prototype
// APIs. The tests here check whether bad user inputs are diagnosed correctly.
// The tests here model possible invalid inputs to the os log methods.
// TODO: the diagnostics will be improved.

import OSLogPrototype

if #available(OSX 10.12, iOS 10.0, watchOS 3.0, tvOS 10.0, *) {

  func testDynamicLogMessage(h: Logger, message: OSLogMessage) {
    h.log(level: .debug, message) // expected-error {{os log methods must be passed a string interpolation literal. 'OSLogMessage' must not be constructed explicitly}}
  }

  func testNonconstantFormatOption(h: Logger, formatOpt: IntFormat) {
    h.log(level: .debug, "Minimum integer value: \(Int.min, format: formatOpt)")
    // expected-error @-1 {{'OSLogInterpolation.formatString' is not a constant: formatting and privacy options must be literals}}
  }

  func testNonconstantPrivacyOption(h: Logger,  privacyOpt: Privacy) {
    h.log(level: .debug, "Minimum integer value: \(Int.min, privacy: privacyOpt)")
    // expected-error @-1 {{'OSLogInterpolation.formatString' is not a constant: formatting and privacy options must be literals}}
  }

  // FIXME: the following two tests should produce diagnostics and are not
  // valid uses of the log APIs. Only string interpolation literal should be
  // passed to log methods.
  func testNoninlinedOSLogMessage(h: Logger) {
    let formatString: OSLogMessage = "Minimum integer value: \(Int.min)"
    h.log(level: .debug, formatString)
  }

  func testNoninlinedOSLogMessageComplex(h: Logger, b: Bool) {
    let formatString: OSLogMessage = "Maximum integer value: \(Int.max)"
    if !b {
      return;
    }
    h.log(level: .debug, formatString)
  }
}

