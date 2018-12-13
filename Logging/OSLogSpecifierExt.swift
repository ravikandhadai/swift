//===--- OSLogSpecifierExts.swift -----------------------------------------===//
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import OSLogPrototype
import ByteEncoder

public struct CustomType {
  let fld: Int32
  
  public init() {
    fld = 0
  }
}

/// Extend appendInterpolation to accept the custom type
extension PackedLogMsg.StringInterpolation {
  public mutating func appendInterpolation(_ foo: @autoclosure @escaping () -> CustomType) {
    argCount += 1
    let flag: UInt8 = 2
    let typeBits: UInt8 = 7
    
    formatString += "%foo"
    encodeOps.append(flag | (typeBits << 4))
    encodeOps.append(UInt8(4)) // Byte size of CustomType.
    encodeOps.append(foo)
  }
}

/// Implement encoding for the CustomType.
extension EncodeOperations {
  public mutating func append(_ x: @escaping () -> CustomType) {
    append({ x().fld })
  }
}


