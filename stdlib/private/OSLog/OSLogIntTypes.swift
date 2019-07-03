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

extension OSLogInterpolation {
  @_transparent
  @_optimize(none)
  public mutating func appendInterpolation(
    _ number: @autoclosure @escaping () -> Int32,
    format: IntFormat = .decimal,
    privacy: Privacy = .public
  ) {
    guard argumentCount < maxOSLogArgumentCount else { return }

    let isPrivateArgument = isPrivate(privacy)
    
    formatString += getIntegerFormatSpecifier(
      format,
      isPrivate: isPrivateArgument,
      bitWidth: 32,
      isSigned: true)
    
    let argumentByteCount = OSLogSerializationInfo.sizeForEncoding(Int32.self)
    addIntHeaders(isPrivateArgument, argumentByteCount)
    
    arguments.append(number)
    argumentCount += 1
  }
}

extension OSLogArguments {
  @usableFromInline
  internal mutating func append(_ value: @escaping () -> Int32) {
    argumentClosures!.append({ $0.serialize(value()) })
  }
}

extension OSLogSerializationInfo {
  /// Return the number of bytes needed for serializing an Int32 value.
  @usableFromInline
  @_transparent
  internal static func sizeForEncoding(_ type: Int32.Type) -> Int {
    return 4
  }
}

extension OSLogByteBufferBuilder {
  /// Serialize an Int32 at the buffer location pointed to by `position`.
  @usableFromInline
  internal mutating func serialize(_ value: Int32) {
    let byteCount = OSLogSerializationInfo.sizeForEncoding(Int32.self)
    let dest = UnsafeMutableRawBufferPointer(start: position, count: byteCount)
    withUnsafeBytes(of: value) { dest.copyMemory(from: $0) }
    position += byteCount
  }
}
