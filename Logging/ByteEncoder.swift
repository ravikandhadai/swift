//===------ ByteEncoder.swift ---------------------------------------------===//
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

/// Encodes standard Swift types to bytes using unsafe operations.
/// This meant to be a part of the standard library.
public struct ByteEncoder {
  var currPtr: UnsafeMutablePointer<UInt8>
  var auxStorage: [UnsafeMutablePointer<Int8>]

  public init(_ byteBuf: UnsafeMutablePointer<UInt8>) {
    currPtr = byteBuf
    auxStorage = []
  }

  public mutating func encode(_ x: UInt8) {
    currPtr[0] = x
    currPtr += 1
  }

  public mutating func encode(_ x: Int32) {
    var y = x
    let src = Builtin.addressof(&y)
    let dest = currPtr._rawValue

    Builtin.int_memcpy_RawPointer_RawPointer_Int64(dest, src, UInt64(4)._value,
                                                   /*volatile:*/ false._value)
    currPtr += 4
  }

  public mutating func encode(_ x: Int64) {
    var y = x
    let src = Builtin.addressof(&y)
    let dest = currPtr._rawValue

    Builtin.int_memcpy_RawPointer_RawPointer_Int64(dest, src, UInt64(8)._value,
                                                   /*volatile:*/ false._value)
    currPtr += 8
  }

  public mutating func encode(_ x: Int) {
    let byteCount = MemoryLayout<Int>.size
    var y = x
    let src = Builtin.addressof(&y)
    let dest = currPtr._rawValue

    Builtin.int_memcpy_RawPointer_RawPointer_Int64(dest, src,
                                        UInt64(byteCount)._value,
                                        /*volatile:*/ false._value)
    currPtr += byteCount
  }

  public mutating func encode(_ x: UInt32) {
    var y = x
    let src = Builtin.addressof(&y)
    let dest = currPtr._rawValue

    Builtin.int_memcpy_RawPointer_RawPointer_Int64(dest, src, UInt64(4)._value,
                                                   /*volatile:*/ false._value)
    currPtr += 4
  }

  public mutating func encode(_ x: UInt64) {
    var y = x
    let src = Builtin.addressof(&y)
    let dest = currPtr._rawValue

    Builtin.int_memcpy_RawPointer_RawPointer_Int64(dest, src, UInt64(8)._value,
                                                   /*volatile:*/ false._value)
    currPtr += 8
  }

  public mutating func encode(_ x: UInt) {
    let byteCount = MemoryLayout<UInt>.size
    var y = x
    let src = Builtin.addressof(&y)
    let dest = currPtr._rawValue

    Builtin.int_memcpy_RawPointer_RawPointer_Int64(dest, src,
                                      UInt64(byteCount)._value,
                                      /*volatile:*/ false._value)
    currPtr += byteCount
  }

  public mutating func encode(_ x: Double) {
    var y = x
    let src = Builtin.addressof(&y)
    let dest = currPtr._rawValue

    Builtin.int_memcpy_RawPointer_RawPointer_Int64(dest, src, UInt64(8)._value,
                                                   /*volatile:*/ false._value)
    currPtr += 8
  }

  public mutating func encode(_ x: Float) {
    var y = x
    let src = Builtin.addressof(&y)
    let dest = currPtr._rawValue

    Builtin.int_memcpy_RawPointer_RawPointer_Int64(dest, src, UInt64(4)._value,
                                                   /*volatile:*/ false._value)
    currPtr += 4
  }

  public mutating func encode(_ x: String) {
    let byteCount = MemoryLayout<UInt>.size
    // Note: we can take an additional parameter that can be used to store
    // these allocations and release them later.
    let nsstring = x._bridgeToObjectiveC()
    let length = nsstring.lengthOfBytes(using: 4) + 1
    var strbuf = UnsafeMutablePointer<Int8>.allocate(capacity: length)
    nsstring.getCString(strbuf, maxLength: length, encoding: 4)
    let src = Builtin.addressof(&strbuf)
    let dest = currPtr._rawValue
    Builtin.int_memcpy_RawPointer_RawPointer_Int64(
      dest, src, UInt64(byteCount)._value,
      /*volatile:*/ false._value)
    currPtr += byteCount
    auxStorage.append(strbuf)
  }

  public mutating func encode(_ x: NSObject) {
    let byteCount = MemoryLayout<UInt>.size
    var y = x
    let src = Builtin.addressof(&y)
    let dest = currPtr._rawValue
    Builtin.int_memcpy_RawPointer_RawPointer_Int64(
      dest, src, UInt64(MemoryLayout<UInt>.size)._value,
      /*volatile:*/ false._value)
    currPtr += byteCount
  }
  
  public static func getBytesForEncoding<U>(_ typ: U.Type) -> Int {
    return MemoryLayout<U>.size
  }

  public func destroy() {
    auxStorage.forEach { $0.deallocate() }
  }
}

/// A structure that tracks an array of encoding operations.
/// Each encoding operation is a closure that takes a ByteEncoder.
public struct EncodeOperations {

  public var ops: [(inout ByteEncoder) -> ()]
        // An array of closures for encoding arguments into a byte buffer
  public var totalByteCount: Int

  public init() {
    ops = []
    totalByteCount = 0
  }

  /// Creates a closure to perform the append of the value.
  /// But doesn't actually perform it.
  public mutating func append(_ x: UInt8) {
    let encoder = { (enc: inout ByteEncoder) in enc.encode(x) }
    ops.append(encoder)
    totalByteCount += ByteEncoder.getBytesForEncoding(UInt8.self)
  }

  public mutating func append(_ x: @escaping () -> Int32) {
    let encoder = { (enc: inout ByteEncoder) in enc.encode(x()) }
    ops.append(encoder)
    totalByteCount += ByteEncoder.getBytesForEncoding(Int32.self)
  }

  public mutating func append(_ x: @escaping () -> Int64) {
    let encoder = { (enc: inout ByteEncoder) in enc.encode(x()) }
    ops.append(encoder)
    totalByteCount += ByteEncoder.getBytesForEncoding(Int64.self)
  }

  public mutating func append(_ x: @escaping () -> UInt32) {
    let encoder = { (enc: inout ByteEncoder) in enc.encode(x()) }
    ops.append(encoder)
    totalByteCount += ByteEncoder.getBytesForEncoding(Int32.self)
  }

  public mutating func append(_ x: @escaping () -> UInt64) {
    let encoder = { (enc: inout ByteEncoder) in enc.encode(x()) }
    ops.append(encoder)
    totalByteCount += ByteEncoder.getBytesForEncoding(UInt64.self)
  }

  public mutating func append(_ x: @escaping () -> Int) {
    let encoder = { (enc: inout ByteEncoder) in enc.encode(x()) }
    ops.append(encoder)
    totalByteCount += ByteEncoder.getBytesForEncoding(Int.self)
  }

  public mutating func append(_ x: @escaping () -> UInt) {
    let encoder = { (enc: inout ByteEncoder) in enc.encode(x()) }
    ops.append(encoder)
    totalByteCount += ByteEncoder.getBytesForEncoding(UInt.self)
  }

  public mutating func append(_ x: @escaping () -> Double) {
    let encoder = { (enc: inout ByteEncoder) in enc.encode(x()) }
    ops.append(encoder)
    totalByteCount += ByteEncoder.getBytesForEncoding(Double.self)
  }

  public mutating func append(_ x: @escaping () -> Float) {
    let encoder = { (enc: inout ByteEncoder) in enc.encode(x()) }
    ops.append(encoder)
    totalByteCount += ByteEncoder.getBytesForEncoding(Float.self)
  }

  public mutating func appendString(_ x: @escaping () -> String) {
    let encoder = { (enc: inout ByteEncoder) in enc.encode(x()) }
    ops.append(encoder)
    totalByteCount += ByteEncoder.getBytesForEncoding(String.self)
  }

  public mutating func appendObject(_ x: @escaping () -> NSObject) {
    let encoder = { (enc: inout ByteEncoder) in enc.encode(x()) }
    ops.append(encoder)
    totalByteCount += ByteEncoder.getBytesForEncoding(NSObject.self)
  }

  public static func += (_ first: inout EncodeOperations,
                         _ other: EncodeOperations) {
    first.ops += other.ops
    first.totalByteCount += other.totalByteCount
  }
}
