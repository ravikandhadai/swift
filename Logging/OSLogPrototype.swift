//===--- OSLogPrototype.swift ---------------------------------------------===//
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Foundation
import Swift
import ByteEncoder

public enum IntLogFormat {
  case decimal
  case hex
  case octal
  case ipAddress
  case time
  //case darwin.errno, darwin.signal, filemode, bitrate ...
}

public enum FloatLogFormat {
  case hexfloat
  case scientific(Int)  // The parameter specifies the precision.
  case decimal(Int)
}

public func escapePercents(_ str: String) -> String {
  return str.replacingOccurrences(of: "%", with: "%%")
}

public struct PackedLogMsg : ExpressibleByStringInterpolation,
ExpressibleByStringLiteral {

  let stringInterpol : StringInterpolation
  
  public struct StringInterpolation : StringInterpolationProtocol {
    public var preamble: UInt8 // The first, summary byte of the header.
    public var argCount: UInt8 // The number of arguments.
    public var formatString: String
    public var encodeOps: EncodeOperations

    public init(literalCapacity: Int, interpolationCount: Int) {
      formatString = ""
      preamble = 0
      argCount = 0
      encodeOps = EncodeOperations()
    }

    public mutating func appendLiteral(_ literal: String) {
      formatString += escapePercents(literal)
    }

    public mutating func appendInterpolation(_ number: @autoclosure @escaping () -> Int,
                                             _ format: IntLogFormat = .decimal) {
      argCount += 1
      appendInteger(number, format, false)
    }

    public mutating func appendInterpolation(private number: @autoclosure @escaping () -> Int,
                                             _ format: IntLogFormat = .decimal) {
      argCount += 1
      appendInteger(number, format, true)
    }

    public mutating func appendInterpolation(_ number: @autoclosure @escaping () -> Double,
                                             _ format: FloatLogFormat = .decimal(6)) {
      argCount += 1
      appendDouble(number, format, false)
    }

    public mutating func appendInterpolation(private number: @autoclosure @escaping () -> Double,
                                             _ format: FloatLogFormat = .decimal(6)) {
      argCount += 1
      appendDouble(number, format, true)
    }

    public mutating func appendInterpolation(_ obj: @autoclosure @escaping () -> NSObject) {
      argCount += 1
      appendNSObject(obj, false)
    }

    public mutating func appendInterpolation(private obj: @autoclosure @escaping () -> NSObject) {
      argCount += 1
      appendNSObject(obj, true)
    }

    public mutating func appendInterpolation(_ str: @autoclosure @escaping () -> String) {
      argCount += 1
      appendString(str, false)
    }

    public mutating func appendInterpolation(private str: @autoclosure @escaping () -> String) {
      argCount += 1
      appendString(str, true)
    }

    mutating func appendInteger(_ number: @escaping () -> Int,
                                _ format: IntLogFormat,
                                _ isPrivate: Bool) {
      // Set flags and preamble based on the private flag.
      var flag: UInt8 = 0
      if (isPrivate) {
        preamble |= 1
        flag = 1
      }

      // Construct a format string.
      let byteCount = MemoryLayout<Int>.size
      let prefix = byteCount == 4 ? "" : "l"
      var specifier = "d"
      var formatTag: String? = nil

      switch (format) {
      case .time:
        formatTag = "time_t"

      case .ipAddress:
        formatTag = "network:in_addr"

      case .hex:
        specifier = "x"

      case .octal:
        specifier = "o"

      default:
        break
      }

      formatString += "%"
      if (isPrivate || formatTag != nil) {
        formatString += "{"
        if (isPrivate) {
          formatString += "private"
        }
        if let tag = formatTag {
          if (isPrivate) {
            formatString += ","
          }
          formatString += tag
        }
        formatString += "}"
      }
      formatString += prefix + specifier
      encodeOps.append(flag) // A preamble for the argument.
      encodeOps.append(UInt8(byteCount)) // Byte count of the argument
      encodeOps.append(number)
    }

    mutating func appendDouble(_ number: @escaping () -> Double, _ format: FloatLogFormat,
                               _ isPrivate: Bool) {
      var formatStr: String = ""
      var flag: UInt8 = 0 // this is 4 bits

      // First add precision, if applicable.
      switch (format) {
      case .decimal(let precision):
        formatStr = "." + String(precision)
      case .scientific(let precision):
        formatStr = "." + String(precision)
      default:
        formatStr = ""
      }
      // Add format specifier.
      switch (format) {
      case .hexfloat:
        formatStr += "la"
      case .scientific(_):
        formatStr += "le"
      default:
        formatStr += "lf"
      }

      formatString += "%"
      if (isPrivate) {
        formatString += "{private}"
        preamble |= 1
        flag = 1
      }
      formatString += formatStr
      encodeOps.append(flag) // A preamble for the argument.
      encodeOps.append(UInt8(8)) // byte size of Double.
      encodeOps.append(number)
    }

    mutating func appendNSObject(_ obj: @escaping () -> NSObject, _ isPrivate: Bool) {
      var flag: UInt8 = 0 // this is 4 bits
      let typeBits: UInt8 = 4 // this is 4 bits

      // Set the non-scalar bits in the preamble.
      preamble |= 2
      formatString += "%"
      if (isPrivate) {
        formatString += "{private}"
        preamble |= 1
        flag = 1
      }
      formatString += "@"
      encodeOps.append(flag | (typeBits << 4)) // A preamble for the argument.

      let ptrSize = UInt8(MemoryLayout<NSObject>.size)
      encodeOps.append(ptrSize)
      encodeOps.appendObject(obj) // Appends a pointer to the object
    }

    mutating func appendString(_ str: @escaping () -> String, _ isPrivate: Bool) {
      var flag: UInt8 = 0 // this is 4 bits
      let typeBits: UInt8 = 2 // this is 4 bits

      // Set the non-scalar bits in the preamble.
      preamble |= 2
      formatString += "%"
      if (isPrivate) {
        formatString += "{private}"
        preamble |= 1
        flag = 1
      }
      formatString += "s"
      encodeOps.append(flag | (typeBits << 4)) // A preamble for the argument.

      // Consider strings as char *, which is a pointer value.
      let ptrSize = UInt8(MemoryLayout<UnsafeRawPointer>.size)
      encodeOps.append(ptrSize)
      encodeOps.appendString(str) // Appends a pointer to the string after converting to char *
    }

    /// Concatenates one instance of PackedLogMsg with another
    /// Could be used to perform recursive string interpolations.
    public mutating func appendInterpolation(_ msg: PackedLogMsg) {
      let strInterpol = msg.stringInterpol
      preamble |= strInterpol.preamble
      argCount += strInterpol.argCount
      formatString += strInterpol.formatString
      encodeOps += strInterpol.encodeOps
    }
  }

  public let formatString: String
  public let totalArgByteCount: Int
  let encodeOps: [(inout ByteEncoder) -> ()]

  public init(stringInterpolation: StringInterpolation) {
    stringInterpol = stringInterpolation
    formatString = stringInterpol.formatString
    totalArgByteCount = stringInterpol.encodeOps.totalByteCount
    encodeOps = stringInterpol.encodeOps.ops
  }

  public init(stringLiteral value: String) {
    var s = StringInterpolation(literalCapacity: 1, interpolationCount: 0)
    s.appendLiteral(value)
    self.init(stringInterpolation: s)
  }

  public func encode(_ enc: inout ByteEncoder) {
    // Encode the headers
    enc.encode(stringInterpol.preamble)
    enc.encode(stringInterpol.argCount)
    // Encode the body
    encodeOps.forEach { $0(&enc) }
  }
}


public func osLog(_ packedMsg: PackedLogMsg) {
  let logger: OSLog = .default
  let type: OSLogType = .default
  guard logger.isEnabled(type: type) else { return }

  let byteCount = packedMsg.totalArgByteCount + 2 // Two bytes are for the header.
  var bufferPtr = UnsafeMutablePointer<UInt8>.allocate(capacity: byteCount)
  var byteEncoder = ByteEncoder(bufferPtr) // Create a byte encoder.

  // Encode the args tracked by packedMsg.
  packedMsg.encode(&byteEncoder)

  let byteBuffer = UnsafePointer<UInt8>(bufferPtr._rawValue)
  //print("Format string: \(packedMsg.stringInterpol.formatString)")
  // Convert Swift string to a C string
  let cstring = packedMsg.stringInterpol.formatString._bridgeToObjectiveC().utf8String!

  // A wrapper to C ABI _os_log_impl.
  my_os_log_impl_wrapper(#dsohandle, logger, type, cstring, byteBuffer,
                         UInt32(byteCount))

  // Cleanup all storage
  bufferPtr.deallocate()
  byteEncoder.destroy()
}
