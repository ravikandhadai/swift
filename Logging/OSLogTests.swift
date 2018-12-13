//===--- OSLogTests.swift -------------------------------------------------===//
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Foundation
import OSLogPrototype
import OSLogExts
import OSLogSpecExts

func osLogCommands() {
  let errno = 32
  osLog("Process exited abnormally: \(errno, .hex)")
    // Compiler automatically generates the following code snippet
    // var packedMsg = PackedOSLogMessage(...)
    // packedMsg.appendLiteral("Process exited abnormally: ")
    // packedMsg.appendInterpolation(errno, .darwinErrno)
    // osLog(packedMsg)

  let timeout = 100
  osLog("Connection timed out in \(Double(timeout)/1000, .decimal(1))s")

  let str = "Launching Diagnostics"
  osLog("\(private: str)")

  let set = NSOrderedSet(array: [0,1,2,3])
  osLog("Test Object: \(set)")

  ////
  //// Extending to custom types and new specifiers
  ////
  let dev = Developer(1, "iOS Developer", .swift)
  osLog("A commit was made by the developer: \(dev)")

  let customType = CustomType()
  osLog("Test custom specifiers: \(customType)")
}

func osLogEscapingTests() {
  // Test backslash escapes work correctly.
  osLog("\"Imagination is more important than knowledge\" - Einstein")
  osLog("\'Imagination is more important than knowledge\' - Einstein")
  osLog("Imagination is more important than knowledge \n - Einstein")
  osLog("Imagination is more important than knowledge - \\Einstein")

  // TODO: should warn on strings with null escape character as it will
  // mark the end of the string in a osLog message.
  osLog("The log message will be truncated here. \0 You wont see this")

  // Test Unicode characters
  osLog("dollar sign: \u{24}")
  osLog("black heart: \u{2665}")
  osLog("sparkling heart: \u{1F496}")
  let sparklingHeart = "\u{1F496}"
  osLog("sparkling heart: \(sparklingHeart)")

  // Test multiline string interpolations
  osLog("""
        Sparkling heart should appear in the next line:
        \(sparklingHeart)
        """)
  osLog("""
        Sparkling heart should appear in the same line: \
        \(sparklingHeart)
        """)
  osLog("""
        Sparkling heart should appear three lines below: \n\n\t
        \(sparklingHeart)
        """)

  // Test escaping of format strings
  osLog("a = c%d")
  osLog("This is 100% pure Swift code")
}

func osLogRawStringTests() {
  let interpolated = 10
  osLog(#"This is not \(interpolated)"#)
  osLog(#"This is not escaped \n"#)
  osLog(##"This is a printf escape character: \b"##)
  osLog(##"The interpolated value is \##(interpolated)"##)
  osLog(#"Sparkling heart should appear in the next line. \#n \#u{1F496}"#)
  osLog(#"A windows path: c:\windows\system32"#)
}

osLogCommands()
osLogEscapingTests()
osLogRawStringTests()
