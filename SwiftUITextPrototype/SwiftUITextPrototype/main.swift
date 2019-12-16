//
//  main.swift
//  SwiftUITextPrototype
//
//  Created by Ravi on 12/13/19.
//  Copyright © 2019 Apple. All rights reserved.
//

import Foundation
import SwiftUITextPrototypeLib

func test() {
  let world = "world"
  let _ = Text("hello \(world)")

  let x = 10
  let _ = Text("A number \(x, specifier: "%lld")")

  let _ = Text("Another number \(12)")

  let y = 11.2
  let _ = Text("A floating-point number: \(y)")

  let nsDate = NSDate()
  let dateFormatter = DateFormatter()
  dateFormatter.dateStyle = .medium
  dateFormatter.timeStyle = .none

  let _ = Text("A date with formatter \(nsDate, formatter: dateFormatter)")
}
