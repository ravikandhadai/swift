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

osLogCommands()




