//===----------------- OSLogDarwinTypes.swift ----------------------------===//
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

public enum DarwinInt32Format {
  case ipaddr
  case time_t
}

extension OSLogInterpolation {
  @_semantics("constant_evaluable")
  @inlinable
  @_optimize(none)
  public mutating func appendInterpolation(
    _ number: @autoclosure @escaping () -> Int32,
    format: DarwinInt32Format,
    privacy: Privacy = .public
  ) {
    guard argumentCount < maxOSLogArgumentCount else { return }
    
    let isPrivateArgument = isPrivate(privacy)
    formatString += getDarwinFormatSpecifier(format, isPrivateArgument)
    addIntHeaders(isPrivateArgument, sizeForEncoding(Int32.self))
    arguments.append(number)
    argumentCount += 1
  }

  @inlinable
  @_semantics("constant_evaluable")
  @_effects(readonly)
  @_optimize(none)
  internal func getDarwinFormatSpecifier(
    _ format: DarwinInt32Format,
    _ isPrivate: Bool
  ) -> String  {
    var formatSpecifier: String = isPrivate ? "%{private," : "%{public,"
    switch (format) {
    case .time_t:
      formatSpecifier += "time_t"
    default:
      formatSpecifier += "network:in_addr"
    }
    formatSpecifier += "}d"
    return formatSpecifier
  }
}

