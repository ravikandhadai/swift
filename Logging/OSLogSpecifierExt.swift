//
//  OSLogExtensions.swift
//  StringInterpolTest
//
//  Created by Ravi on 8/23/18.
//  Copyright © 2018 swe. All rights reserved.
//

import OSLogPrototype
import ByteEncoder

public struct CustomType {
  let fld: Int32
  
  public init() {
    fld = 0
  }
}

/// Extend appendInterpolation to accept the custom type
extension OS.LogMessage.StringInterpolation {
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
extension OS.EncodeOperations {
  public mutating func append(_ x: @escaping () -> CustomType) {
    append({ x().fld })
    // let encoder = { (enc: inout ByteEncoder) in enc.encode(x()) }
    // ops.append(encoder)
    // totalByteCount += ByteEncoder.getBytesForEncoding(CustomType.self)
    // extension ByteEncode {
    //   func encode(x: CustomType)
    //   func getBytesForEncoding(...)
    // }
  }
}


