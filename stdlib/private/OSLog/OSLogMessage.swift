//===----------------- OSLogMessage.swift ---------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// This file contains data structures and helper functions that are used by
// the new OS log APIs. These are prototype implementations and should not be
// used outside of tests.

/// Formatting options supported by the logging APIs for logging integers.
/// These can be specified in the string interpolation passed to the log APIs.
/// E.g.
///     log.info("Writing to file with permissions: \(perm, as: .octal)")
///
/// See `OSLogInterpolation.appendInterpolation` definitions for default options
/// for integer types.
public enum IntFormat {
  case decimal
  case hex
  case octal
}

/// Privacy qualifiers for indicating the privacy level of the logged data
/// to the logging system. These can be specified in the string interpolation
/// passed to the log APIs.
/// E.g.
///     log.info("Login request from user id \(userid, is: .private)")
///
/// See `OSLogInterpolation.appendInterpolation` definitions for default options
/// for each supported type.
public enum Privacy {
  case `private`
  case `public`
}

/// Maximum number of arguments i.e., interpolated expressions that can
/// be used in the string interpolations passed to the log APIs.
/// This limit is imposed by the OS logging system.
public let maxOSLogArgumentCount = 48

/// Represents a string interpolation passed to the log APIs.
///
/// This type converts (through its methods) the given string interpolation into
/// a C-style format string and a sequence of arguments, which is represented
/// by the type `OSLogArgumentSequence`.
///
/// Do not create an instance of this type directly. It is used by the compiler
/// when you pass a string interpolation to the log APIs.
public struct OSLogInterpolation : StringInterpolationProtocol {
  /// An OS log format string constructed from the given string interpolation.
  public var formatString: String

  /// A representation of a sequence of arguments that must be serialized
  /// to bytes and passed to the OS logging system. The arguments are
  /// (autoclosures of) expressions that are interpolated along with some
  /// headers.
  public var argumentSequence: OSLogArgumentSequence

  /// The summary byte, as defined by the OS logging system, that summarizes the
  /// nature/type of the arguments.
  public var preamble: UInt8

  /// Number of arguments i.e, number of interpolated expressions.
  /// This will be determined on the fly in order to support concatenation
  /// and interpolation of instances of `OSLogMessage`.
  public var argumentCount: UInt8

  public init(literalCapacity: Int, interpolationCount: Int) {
    // TODO: format string must be fully constructed at compile time.
    // The parameters `literalCapacity` and `interpolationCount` are ignored.
    formatString = ""
    argumentSequence = OSLogArgumentSequence()
    preamble = 0
    argumentCount = 0
  }

  public mutating func appendLiteral(_ literal: String) {
    formatString += literal.percentEscapedString
  }

  /// Define interpolation for expressions of type Int. This definition enables
  /// passing a formatting option and a privacy qualifier along with the
  /// interpolated expression as shown below:
  ///
  ///         "\(x, as: .hex, is: .private\)"
  ///
  /// - Parameters:
  ///  - number: the interpolated expression of type Int, which is autoclosured.
  ///  - format: a formatting option available for Int types, defined by the
  ///    enum `IntFormat`.
  ///  - privacy: a privacy qualifier which is either private or public.
  ///    The default is public.
  public mutating func appendInterpolation(
    _ number: @autoclosure @escaping () -> Int,
    as format: IntFormat = .decimal,
    is privacy: Privacy = .public) {
    guard argumentCount < maxOSLogArgumentCount else { return }

    addIntHeadersAndFormatSpecifier(
      format,
      isPrivate: privacy == .private,
      byteSize: UInt8(MemoryLayout<Int>.size),
      isUnsigned: false)
    argumentSequence.append(number)
  }

  /// Construct/update format string and headers from the qualifiers (of the
  /// interpolated expression) passed as parameters.
  ///
  /// All arguments to this function must be known at compile time.
  public mutating func addIntHeadersAndFormatSpecifier(
    _ format: IntFormat,
    isPrivate: Bool,
    byteSize: UInt8,
    isUnsigned: Bool) {
    // Always explicitly set either public or private flag bit.
    let flag: UInt8 = isPrivate ? 1 : 2
    formatString += getIntegerFormatSpecifier(
      format,
      isPrivate: isPrivate,
      isLong: byteSize > 4,
      isUnsigned: isUnsigned);

    // Append the argument header and size.
    argumentSequence.append(flag)
    argumentSequence.append(byteSize)

    // Update the summary bytes.
    preamble |= isPrivate ? 1 : 0
    argumentCount += 1
  }

  /// Construct an OS log format specifier from the given parameters.
  /// All arguments to this function must be known at compile time.
  public func getIntegerFormatSpecifier(
    _ format: IntFormat,
    isPrivate: Bool,
    isLong: Bool,
    isUnsigned: Bool)
    -> String {
    var formatSpecifier: String = isPrivate ? "%{private}" : "%{public}"
    formatSpecifier += isLong ? "l" : ""

    switch (format) {
    case .hex:
      formatSpecifier += "x"
    case .octal:
      formatSpecifier += "o"
    default:
      formatSpecifier += isUnsigned ? "u" : "d"
    }
    return formatSpecifier;
  }
}

extension String {
  /// Replace all percents "%" in the string by "%%" so that the string can be
  /// interpreted as a C format string.
  public var percentEscapedString: String {
    get {
      return self.reduce(into: "") { (result, char) in
        result += (char == "%") ? "%%" : String(char)
      }
    }
  }
}

public struct OSLogMessage :
  ExpressibleByStringInterpolation, ExpressibleByStringLiteral
{
  let oslogInterpolation : OSLogInterpolation

  /// Initializer for accepting string interpolations.
  public init(stringInterpolation: OSLogInterpolation) {
    oslogInterpolation = stringInterpolation
  }

  /// Initializer for accepting string literals.
  public init(stringLiteral value: String) {
    // Note that the actual value of `literalCapacity` is not important as it
    // is ignored by `OSLogInterpolation.init`. However, it must be a literal.
    var s = OSLogInterpolation(literalCapacity: 1, interpolationCount: 0)
    s.appendLiteral(value)
    self.init(stringInterpolation: s)
  }

  /// Format string constructed from the string interpolation.
  public var formatString: String {
    get { return oslogInterpolation.formatString }
  }

  /// The byte size of the argument buffer that will contain the preamble
  /// and the elements of `oslogInterpolation.argumentSequence`.
  public var argumentBufferByteSize: Int {
    get { return oslogInterpolation.argumentSequence.byteCount + 2 }
  }

  /// Serialize the summary bytes and arguments into the given byte-buffer
  /// builder.
  public func serializeArguments(
    into bufferBuilder: inout OSLogByteBufferBuilder) {
    bufferBuilder.serialize(oslogInterpolation.preamble)
    bufferBuilder.serialize(oslogInterpolation.argumentCount)
    oslogInterpolation.argumentSequence.serialize(into: &bufferBuilder)
  }
}

/// A representation of a sequence of arguments and headers (of possibly
/// different types) that have to be serialized to a byte buffer. The arguments
/// are captured within closures and stored in an array. The closures accept an
/// instance of `OSLogByteBufferBuilder`, and when invoked, serialize the
/// argument using the passed `OSLogByteBufferBuilder` instance.
public struct OSLogArgumentSequence {
  /// An array of closures that captures arguments of possibly different types.
  public var argumentClosures: [(inout OSLogByteBufferBuilder) -> ()]
  /// Sum total of the byte size of the arguments that are tracked.
  public var byteCount: Int

  public init() {
    argumentClosures = []
    byteCount = 0
  }

  /// Append a byte-sized header, constructed by
  /// `OSLogMessage.appendInterpolation`, to the tracked array of closures.
  public mutating func append(_ header: UInt8) {
    argumentClosures.append({ $0.serialize(header) })
    byteCount += OSLogByteBufferBuilder.numBytesForEncoding(UInt8.self)
  }

  /// Append an (autoclosured) interpolated expression of type Int, passed to
  /// `OSLogMessage.appendInterpolation`, to the tracked array of closures.
  public mutating func append(_ value: @escaping () -> Int) {
    argumentClosures.append({ $0.serialize(value()) })
    byteCount += OSLogByteBufferBuilder.numBytesForEncoding(Int.self)
  }

  public func serialize(into bufferBuilder: inout OSLogByteBufferBuilder) {
    argumentClosures.forEach { $0(&bufferBuilder) }
  }
}

/// A struct that manages serialization of instances of specific types to
/// a byte buffer. The byte buffer is provided as an argument to the initializer
/// so that its lifetime can be managed by the caller.
public struct OSLogByteBufferBuilder {
  public var position: UnsafeMutablePointer<UInt8>

  /// - Parameter bufferStart: the starting pointer to a byte buffer
  ///   that must contain the serialized bytes.
  public init(_ bufferStart: UnsafeMutablePointer<UInt8>) {
    position = bufferStart
  }

  /// Serialize a UInt8 value at the buffer location pointed to by `position`.
  public mutating func serialize(_ value: UInt8) {
    position[0] = value
    position += 1
  }

  /// Serialize an Int at the buffer location pointed to by `position`.
  public mutating func serialize(_ value: Int) {
    let byteCount = MemoryLayout<Int>.size
    let dest = UnsafeMutableRawBufferPointer(start: position, count: byteCount)
    withUnsafeBytes(of: value) { dest.copyMemory(from: $0) }
    position += byteCount
  }

  /// Return the number of bytes needed for serializing an UInt8 value.
  public static func numBytesForEncoding(_ typ: UInt8.Type) -> Int {
    return 1
  }

  /// Return the number of bytes needed for serializing an `Int` value.
  public static func numBytesForEncoding(_ typ: Int.Type) -> Int {
    return MemoryLayout<Int>.size
  }
}
