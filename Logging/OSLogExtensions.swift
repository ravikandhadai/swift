//
//  OSLogExtensions.swift
//  StringInterpolTest
//
//  Created by Ravi on 8/23/18.
//  Copyright © 2018 swe. All rights reserved.
//

import ByteEncoder
import OSLogPrototype

public struct ComplexNumber {
  let real: Double
  let imaginary: Double
  
  public init(_ r: Double, _ i: Double) {
    real = r
    imaginary = i
  }
}

/// Extend appendInterpolation to accept the custom type.
extension OS.LogMessage.StringInterpolation {
  public mutating func appendInterpolation(_ num: @autoclosure @escaping () -> ComplexNumber) {
    // log this as [x,yi]
    let x: OS.LogMessage = "[\(num().real, .decimal(1)), \(num().imaginary, .decimal(1))i]"
    appendInterpolation(x)
  }
}
