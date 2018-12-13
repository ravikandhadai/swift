//===--- OSLogExtensions.swift --------------------------------------------===//
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import OSLogPrototype

public struct Developer {
  let id: Int
  var name: String
  var preferredLanguage: Language

  public init(_ iden: Int, _ nm: String, _ prefLang: Language) {
    id = iden
    name = nm
    preferredLanguage = prefLang
  }
}

public enum Language {
  case swift
  case objectiveC
  case cpp
}

extension PackedLogMsg.StringInterpolation {
  // TODO: the parameter should be an autoclosure to enable compile-time interpretation.
  public mutating func appendInterpolation(_ dev: Developer) {
    let msg: PackedLogMsg = "Id: \(private: dev.id) Name: \(private: dev.name) Preferred Language: \(dev.preferredLanguage)"
    appendInterpolation(msg) // Combines self with msg.stringInterpol.
  }

  public mutating func appendInterpolation(_ lang: Language) {
    switch lang {
    case .swift:
      appendInterpolation("swift")
    case .objectiveC:
      appendInterpolation("objectiveC")
    case .cpp:
      appendInterpolation("C++")
    }
  }
}
