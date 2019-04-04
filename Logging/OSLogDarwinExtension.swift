import OSLog
import Darwin

// These are formats specific to Darwin for 32 bit signed integers.
public enum DarwinInt32Format {
  case darwinErrno
  case ipaddr
  //case darwin.signal, filemode, time, bitrate ...
}

// Note that his enum is not publicly visible. But only used for internal purposes.
public enum PointerFormat {
  case timeval
  case timespec
  case uuid
  case sockaddr
  case in6_addr
}

extension OSLogMessage.StringInterpolation {

  @inlinable
  @_effects(readonly)
  @_semantics("compiler.evaluable")
  func getDarwinInt32FormatString(_ format: DarwinInt32Format,
                                  _ isPrivate: Bool) -> String {
    let formatTag: String

    switch (format) {
    case .darwinErrno:
      formatTag = "darwin.errno"

    case .ipaddr:
      formatTag = "network:in_addr"
    }

    var formatString: String = isPrivate ? "%{private" : "%{public"
    formatString += ","
    formatString += formatTag
    formatString += "}d"
    return formatString;
  }

  @_transparent
  public mutating func appendInterpolation(_ number: @autoclosure @escaping () -> Int32,
                                           as format: DarwinInt32Format,
                                           is privacy: Privacy = .public) {
    argCount += 1
    let isPrivate = privacy == .private
    let flag = self.getFlagAndSetPreamble(isPrivate)
    formatString += getDarwinInt32FormatString(format, isPrivate);
    encodeOps.append(flag) // A preamble for the argument.
    encodeOps.append(4) // Byte count of the argument
    encodeOps.append(number)
  }

  // Pointers of standard types

  @inlinable
  @_effects(readonly)
  @_semantics("compiler.evaluable")
  func getPointerFormatString(_ format: PointerFormat,
                              _ isPrivate: Bool,
                              staticWidth: Int?) -> String {
    let formatTag: String

    switch (format) {
    case .sockaddr:
      formatTag = "network:sockaddr"

    case .in6_addr:
      formatTag = "network:in6_addr"

    case .timeval:
      formatTag = "timeval"

    case .timespec:
      formatTag = "timespec"

    case .uuid:
      formatTag = "uuid_t"
    }

    var formatString: String = isPrivate ? "%{private" : "%{public"
    formatString += ","
    formatString += formatTag
    formatString += "}."

    if let w = staticWidth {
      formatString += String(w)
    } else {
      formatString += "*"
    }
    formatString += "P"
    return formatString;
  }

  // This function can also be factored out
  @inlinable
  @_semantics("compiler.evaluable")
  mutating func getFlagAndSetPreambleOfPointer(_ isPrivate: Bool) -> UInt8 {
    let typeBits: UInt8 = 3 // this is 4 bits
    var privacyBits: UInt8 = 0 // this is 4 bits LSB
    // Set the non-scalar bits in the preamble.
    preamble |= 2

    if (isPrivate) {
      preamble |= 1
      privacyBits = 1
    }
    return privacyBits | (typeBits << 4)
  }

  @_transparent
  public mutating func appendPrecisionCount(_ precision: @escaping () -> Int32) {
    argCount += 1
    encodeOps.append(UInt8(1 << 4)) // Indicate that this argument is a count
    encodeOps.append(UInt8(4)) // Byte count of the argument
    encodeOps.append(precision)
  }

  @_transparent
  public mutating func appendPointer<T>(_ ptr: @escaping () -> UnsafePointer<T>,
                                        _ format: PointerFormat,
                                        _ isPrivate: Bool,
                                        staticWidth: Int?) {
    argCount += 1
    let flag = getFlagAndSetPreambleOfPointer(isPrivate)
    formatString += getPointerFormatString(format,
                                           isPrivate,
                                           staticWidth: staticWidth)
    encodeOps.append(flag) // A preamble for the argument.
    encodeOps.append(UInt8(MemoryLayout<UnsafeRawPointer>.size)) // Byte count of the argument
    encodeOps.append(ptr)
  }

  @_transparent
  public mutating func appendInterpolation(_ sockaddr: @autoclosure @escaping () -> UnsafePointer<sockaddr>,
                                           is privacy: Privacy = .private,
                                           precision: @autoclosure @escaping () -> Int32) { // We need to have some precision which is always dyanmic
    // First append a 32 bit integer denoting the precision count
    appendPrecisionCount(precision)
    // Now append the pointer itself
    appendPointer(sockaddr, .sockaddr, privacy == .private, staticWidth: nil) // We have a dynamic width here
  }

  @_transparent
  public mutating func appendInterpolation(_ ipaddr: @autoclosure @escaping () -> UnsafePointer<in6_addr>,
                                           is privacy: Privacy = .private) {
    appendPrecisionCount({ Int32(16) })
    appendPointer(ipaddr, .in6_addr, privacy == .private, staticWidth: 16)
  }

  @_transparent
  public mutating func appendInterpolation(_ timeval: @autoclosure @escaping () -> UnsafePointer<timeval>,
                                           is privacy: Privacy = .private,
                                           precision: @autoclosure @escaping () -> Int32) {
    appendPrecisionCount(precision)
    appendPointer(timeval, .timeval, privacy == .private, staticWidth: nil)
  }

  @_transparent
  public mutating func appendInterpolation(_ timespec: @autoclosure @escaping () -> UnsafePointer<timespec>,
                                           is privacy: Privacy = .private,
                                           precision: @autoclosure @escaping () -> Int32) {
    appendPrecisionCount(precision)
    appendPointer(timespec, .timespec, privacy == .private, staticWidth: nil)
  }

  @_transparent
  public mutating func appendInterpolation(_ uuid: @autoclosure @escaping () -> UnsafePointer<uuid_t>,
                                           is privacy: Privacy = .private) {
    appendPrecisionCount({ Int32(16) })
    appendPointer(uuid, .uuid, privacy == .private, staticWidth: 16)
  }
}

extension OSLogArgumentEncoder {
  @inlinable
  public mutating func append<T>(_ x: @escaping () -> UnsafePointer<T>) {
    let encoder = { (enc: inout OSLogByteBufferBuilder) in enc.encode(x()) }
    argumentEncodeOps.append(encoder)
    totalByteCount += OSLogByteBufferBuilder.numBytesForEncoding(UnsafeRawPointer.self)
  }
}

extension OSLogByteBufferBuilder {
  @inlinable
  public mutating func encode<T>(_ x: UnsafePointer<T>) {
    let byteSize = MemoryLayout<UnsafeRawPointer>.size
    let rawDest = UnsafeMutableRawBufferPointer(start: currPtr,
                              count: byteSize)
    withUnsafeBytes(of: x) { xptr in
      rawDest.copyMemory(from: xptr)
    }
    currPtr += byteSize
  }

  @inlinable
  @_effects(readonly)
  public static func numBytesForEncoding(_ typ: UnsafeRawPointer.Type) -> Int {
    return MemoryLayout<UnsafeRawPointer>.size
  }
}


