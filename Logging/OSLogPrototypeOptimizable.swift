//
//  StringInterpolTest.swift
//  OSLogMixed
//
//  Created by Ravi on 8/2/18.
//  Copyright © 2018 swe. All rights reserved.
//

// Change the syntax to consider the suggestions in the  Post #20 of the Swift forum
// (https://forums.swift.org/t/custom-string-interpolation-and-compile-time-interpretation-applied-to-logging/18799/30?u=ravikandhadai)

// TODO: make sure that the @_effects(readonly) doesn't remove the string interpolation
// methods when the optimizer is not run. It is better to remove the struct
// iff the optimizer is run. (Or compile-time should help here, if we want this
// to be an error)

import Swift
import ByteEncoder

extension OS {

public enum IntFormat {
  case decimal
  case hex
  case octal
  case darwinErrno
  case ipaddr
  //case darwin.signal, filemode, time, bitrate ...
}

public enum FPFormat {
  case hexfloat
  case scientific(Int)  // The parameter specifies the precision.
  case decimal(Int)
}

//@_fixed_layout
public struct PackedOSLogMessage : StringInterpolationProtocol,
ExpressibleByStringInterpolation, ExpressibleByStringLiteral  {

  public var preamble: UInt8 // The first, summary byte of the header.
  public var argCount: UInt8 // The number of arguments.
  public var formatString: String
  public var encoderRegistry: EncodeOperations

  @_transparent
  public init(literalCapacity: Int, interpolationCount: Int) {
    formatString = ""
    preamble = 0
    argCount = 0
    encoderRegistry = EncodeOperations()
  }

  @_transparent
  public mutating func appendLiteral(_ literal: String) {
    formatString += literal
  }

  @_transparent
  public mutating func appendInterpolation( _ number: @autoclosure @escaping () -> Int,
                                            _ format: IntFormat = .decimal) {
    appendInteger(number, format, false)
  }

  @_transparent
  public mutating func appendInterpolation( private number: @autoclosure @escaping () -> Int,
                                            format: IntFormat = .decimal) {
    appendInteger(number, format, true)
  }

  @usableFromInline
  @_effects(readonly)
  @_semantics("compiler.evaluable")
  func getIntegerFormatString(_ format: IntFormat,
                              _ isPrivate: Bool,
                              isLong: Bool) -> String {
    var formatSpec: String = isLong ? "l" : ""
    // A single tag is enough for now. Expand this to more later if needed
    //var tags: [String] = []
    var formatTag: String? = nil

    switch (format) {
    case .hex:
      formatSpec += "x"

    case .octal:
      formatSpec += "o"

    case .darwinErrno:
      formatTag = "darwin.errno"
      formatSpec += "d"

    case .ipaddr:
      formatTag = "network:in_addr"
      formatSpec += "d"

    default:
      formatSpec += "d"
    }

    var formatString: String = "%"
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
      formatString += "}" // tags.joined(separator: ",") + "}"
    }
    formatString += formatSpec
    return formatString;
  }

  @inlinable
  @_semantics("compiler.evaluable")
  mutating func getFlagAndSetPreamble(_ isPrivate: Bool) -> UInt8 {
    if (isPrivate) {
      preamble |= 1
      return 1
    } else {
      return 0
    }
  }

  @_transparent
  @usableFromInline
  mutating func appendInteger(_ number: @escaping () -> Int,
                              _ format: IntFormat,
                              _ isPrivate: Bool) {
    argCount += 1
    var flag = getFlagAndSetPreamble(isPrivate)
    let byteSize = MemoryLayout<Int>.size
    formatString += getIntegerFormatString(format,
                                           isPrivate,
                                           isLong: byteSize >= 32);
    encoderRegistry.append(flag) // A preamble for the argument.
    encoderRegistry.append(UInt8(byteSize))
    encoderRegistry.append(number)
  }

  @_transparent
  public mutating func appendInterpolation(_ number: @autoclosure @escaping () -> Double,
                                           format: FPFormat = .decimal(6)) {
    appendDouble(number, format, false)
  }

  @_transparent
  public mutating func appendInterpolation(private number: @autoclosure @escaping () -> Double,
                                           format: FPFormat = .decimal(6)) {
    appendDouble(number, format, true)
  }


  @_effects(readonly)
  @usableFromInline
  //@_compilerEvaluable
  func getFPFormatString(_ format: FPFormat,
                         _ isPrivate: Bool,
                         isLong: Bool) -> String {
    var formatSpec: String = ""
    // First add precision, if applicable.
    switch (format) {
    case .decimal(let precision):
      formatSpec = "."
      formatSpec += String(precision)  //TODO: handle append
    case .scientific(let precision):
      formatSpec = "."
      formatSpec += String(precision)
    default:
      formatSpec = ""
    }

    if isLong {
      formatSpec += "l"
    }

    // add format specifier
    switch (format) {
    case .hexfloat:
      formatSpec += "a"
    case .scientific(_):
      formatSpec += "e"
    default:
      formatSpec += "f"
    }

    var formatString: String = "%"
    if (isPrivate) {
      formatString += "{private}"
    }
    formatString += formatSpec
    return formatString
  }

  @_transparent
  @usableFromInline
  mutating func appendDouble(_ number: @escaping () -> Double,
                             _ format: FPFormat,
                             _ isPrivate: Bool) {
    argCount += 1
    var flag = getFlagAndSetPreamble(isPrivate)
    formatString += getFPFormatString(format, isPrivate, isLong: true)
    encoderRegistry.append(flag) // A preamble for the argument.
    encoderRegistry.append(UInt8(8))
    encoderRegistry.append(number)
  }

  //  @_effects(readonly)
  //  public mutating func appendInterpolation(_ obj: @autoclosure @escaping () -> NSObject) {
  //    argCount += 1
  //    appendNSObject(obj, false)
  //  }
  //
  //  @_effects(readonly)
  //  public mutating func appendInterpolation( private obj: @autoclosure @escaping () -> NSObject) {
  //    argCount += 1
  //    appendNSObject(obj, true)
  //  }
  //
  //  @_effects(readonly)
  //  mutating func appendNSObject(_ obj: @escaping () -> NSObject,
  //                               _ isPrivate: Bool) {
  //    // Note: we can add static checks using #assert.
  //    var flag: UInt8 = 0 // this is 4 bits
  //    let typeBits: UInt8 = 4 // this is 4 bits
  //
  //    // Set the non-scalar bits in the preamble.
  //    preamble |= 2
  //    formatString += "%"
  //    if (isPrivate) {
  //      formatString += "{private}"
  //      preamble |= 1
  //      flag = 1
  //    }
  //    formatString += "@"
  //    encoderRegistry.append(flag | (typeBits << 4)) // A preamble for the argument.
  //    encoderRegistry.append(UInt8(8))
  //    encoderRegistry.append(obj)
  //  }


  @_transparent
  public init(stringInterpolation: StringInterpolation) {
    self = stringInterpolation
  }

  public typealias StringInterpolation = PackedOSLogMessage

  public init(stringLiteral value: String) {
    self.init(literalCapacity: 1, interpolationCount: 0)
    appendLiteral(value)
  }

  @_transparent
  public func encode(_ enc: inout ByteEncoder) {
    // Encode the headers
    enc.encode(preamble)
    enc.encode(argCount)
    // Encode the body
    let encodeOps = encoderRegistry.ops
    encodeOps.forEach { $0(&enc) }
  }
}

@_semantics("oslog")
// TODO: mark this inlinable.
//@inlinable
public static func osLog(_ packedMsg: PackedOSLogMessage) {
  let logger: OSLog = .default
  let type: OSLogType = .default
  guard logger.isEnabled(type: type) else { return }

  let registry = packedMsg.encoderRegistry
  let bufSize = registry.totalByteCount + 2 // 2 header bytes which are tracked separately.

  // TODO: make sure we do not do unnecessary work in trying to fold these
  // instructions. Probably, disable getting into calls (that are not transparent obviously)
  var bufferPtr = UnsafeMutablePointer<UInt8>.allocate(capacity: bufSize)
  var byteEnc = ByteEncoder(bufferPtr) // Create a byte encoder.

  // Encode the args tracked by packedMsg.
  packedMsg.encode(&byteEnc)

  let byteBuffer = UnsafePointer<UInt8>(bufferPtr._rawValue)
  print("Format string: \(packedMsg.formatString) Buf size: \(bufSize) \n")

  (0 ..< bufSize).forEach { i  in
    print(" Byte \(i) of buffer: \(bufferPtr[i])\n")
  }

  // A wrapper to C ABI _os_log_impl.
  let fstr = packedMsg.formatString
  fstr.withCString { cfstr in
    my_os_log_impl_wrapper(#dsohandle, logger, type, cfstr, byteBuffer,
                           UInt32(bufSize))
  }

  // Cleanup all storage
  bufferPtr.deallocate()
  byteEnc.destroy()
}
}

func osLogClient(x: Double) {
  OS.osLog("Test float: \(x, format: .decimal(1))")
}

osLogClient(x: 108)

//func myClient(x: Double) {
//func osLogClient(x: Double, y: Int) {
//  osLog("Test float: \(x, format: .decimal(1)), \(y, .ipaddr), \(y, .hex)")
//  //osLog("Test hex float: \(100, format: .hexfloat)")
////  let set = NSOrderedSet(array: [0,1,2,3])
////  osLog("Test Object: \(set)")
//}
//osLogClient(x: 12, y: 13)
