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
import Darwin

// These are formats specific to Darwin for 32 bit signed integers.s
public enum DarwinInt32Format {
  case darwinErrno
  //case ipv4addr, darwin.signal, filemode, time, bitrate ...
}

extension OSLogInterpolation {
  @_transparent
  @_optimize(none)
  public mutating func appendInterpolation(
    _ number: @autoclosure @escaping () -> Int32,
    format: DarwinInt32Format,
    privacy: Privacy = .public
  ) {
    guard argumentCount < maxOSLogArgumentCount else { return }

    let isPrivateArgument = isPrivate(privacy)
    
    formatString += getDarwinInt32FormatSpecifier(format, isPrivateArgument)
    
    let argumentByteCount = OSLogSerializationInfo.sizeForEncoding(Int32.self)
    addIntHeaders(isPrivateArgument, argumentByteCount)
    
    arguments.append(number)
    argumentCount += 1
  }
  
  @inlinable
  @_semantics("oslog.interpolation.getDarwinInt32FormatSpecifier")
  @_effects(readonly)
  @_optimize(none)
  internal func getDarwinInt32FormatSpecifier(_ format: DarwinInt32Format,
                                              _ isPrivate: Bool) -> String {
    var formatSpecifier: String = isPrivate ? "%{private," : "%{public,"
    
    switch (format) {
    default:
      formatSpecifier += "darwin.errno"
    }
  
    formatSpecifier += "}d"
    return formatSpecifier
  }
}
