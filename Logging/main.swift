//
//  main.swift
//  StringInterpolTest
//
//  Created by Ravi on 8/23/18.
//  Copyright © 2018 swe. All rights reserved.
//

import Foundation
import OSLogPrototype
import OSLogExts
import OSLogSpecExts

func osLogCommands() {
  let errno: Int32 = 32
  osLog("Process exited abnormally: \(errno, .darwinErrno)")
  // compiler transformation
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
  let complexNum = ComplexNumber(1,2)
  osLog("Test complex number: \(complexNum)")

  let customType = CustomType()
  osLog("Test custom specifiers: \(customType)")
}

osLogCommands()




