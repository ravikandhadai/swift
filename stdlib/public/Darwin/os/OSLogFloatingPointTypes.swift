//===----------------- OSLogFloatingPointTypes.swift -----------------------------===//
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

  /// Define interpolation for expressions of type String.
  /// - Parameters:
  ///  - argumentString: the interpolated expression of type String, which is autoclosured.
  ///  - privacy: a privacy qualifier which is either private or public. Default is private.
  ///  TODO: create a specifier to denote auto-inferred privacy level and make it default.
  @_semantics("constant_evaluable")
  @inlinable
  @_optimize(none)
  public mutating func appendInterpolation(
    _ argument: @autoclosure @escaping () -> Double,
    privacy: Privacy = .private
  ) {
    guard argumentCount < maxOSLogArgumentCount else { return }

    let isPrivateArgument = isPrivate(privacy)
    formatString += getDoubleFormatSpecifier(isPrivateArgument)
    addDoubleHeaders(isPrivateArgument)

    arguments.append(argument)
    argumentCount += 1
  }

  /// Update preamble and append argument headers based on the parameters of
  /// the interpolation.
  @_semantics("constant_evaluable")
  @inlinable
  @_optimize(none)
  internal mutating func addDoubleHeaders(_ isPrivate: Bool) {
    // Append argument header.
    let header = getArgumentHeader(isPrivate: isPrivate, type: .scalar)
    arguments.append(header)

    // Append number of bytes needed to serialize the argument.
    arguments.append(8)

    // Increment total byte size by the number of bytes needed for this
    // argument, which is the sum of the byte size of the argument and
    // two bytes needed for the headers.
    totalBytesForSerializingArguments += 10

    preamble = getUpdatedPreamble(isPrivate: isPrivate, isScalar: true)
  }

  /// Construct an os_log format specifier from the given parameters.
  /// This function must be constant evaluable and all its arguments
  /// must be known at compile time.
  @inlinable
  @_semantics("constant_evaluable")
  @_effects(readonly)
  @_optimize(none)
  internal func getDoubleFormatSpecifier(_ isPrivate: Bool) -> String {
    // TODO: create a specifier to denote auto-inferred privacy.
    return isPrivate ? "%{private}lf" : "%{public}lf"
  }
}

extension OSLogArguments {
  /// Append an (autoclosured) interpolated expression of Double type, passed to
  /// `OSLogMessage.appendInterpolation`, to the array of closures tracked
  /// by this instance.
  @_semantics("constant_evaluable")
  @inlinable
  @_optimize(none)
  internal mutating func append(_ value: @escaping () -> Double) {
    argumentClosures.append({ (position, _) in
      serialize(value(), at: &position)
    })
  }
}

/// Serialize a stable pointer to the string `stringValue` at the buffer location
/// pointed by `bufferPosition`. When necessary, this function would copy the
/// string contents to a storage with a stable pointer. If that happens, a reference
/// to the storage will be added to `storageObjects`.
@inlinable
@_alwaysEmitIntoClient
@inline(__always)
internal func serialize(
  _ value: Double,
  at bufferPosition: inout ByteBufferPointer
) {
  let dest =
    UnsafeMutableRawBufferPointer(start: bufferPosition, count: 8)
  withUnsafeBytes(of: value) { dest.copyMemory(from: $0) }
  bufferPosition += 8
}
