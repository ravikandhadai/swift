// RUN: %target-swift-frontend -emit-sil -primary-file %s -o /dev/null -verify
//
// Tests for yield-once diagnostics emitted for generalized accessors.

struct TestNoYield {
  var computed: Int {
    _read {
    } // expected-error {{accessor must yield before returning}}

    _modify {
    } // expected-error {{accessor must yield before returning}}
  }
}

struct TestReturnPathWithoutYield {
  var stored: Int
  var flag: Bool

  var computed: Int {
    mutating _read {
      if flag {
        yield stored
      }               // expected-note {{must yield when 'if' falls through}}
      flag = true
    } // expected-error {{accessor must yield on all paths before returning}}

    _modify {
      // Diagnostics should attach a note to the earliest conflicting branch.
      if flag {
        yield &stored
      }                 // expected-note {{must yield when 'if' falls through}}

      if !flag {
        flag = true
      }
    } // expected-error {{accessor must yield on all paths before returning}}
  }
}

struct TestIfElseWithoutYield {
  var stored: Int
  var flag: Bool

  var computed: Int {
    mutating _read {
      if flag {
        yield stored
      } else {       // expected-note {{'if' must yield along the else branch}}
        flag = true
      }
    } // expected-error {{accessor must yield on all paths before returning}}

    _modify {
      if flag {     // expected-note {{'if' must yield along the then branch}}
        flag = true
      } else {
        yield &stored
      }
    } // expected-error {{accessor must yield on all paths before returning}}
  }
}


struct TestMultipleYields {
  var stored: Int
  var flag: Bool
  var computed: Int {
    _read {
      if flag {
        yield stored // expected-note {{previous yield was here}}
      }
      yield stored // expected-error {{accessor must not yield more than once}}
    }

    _modify {
      yield &stored // expected-note {{previous yield was here}}
      if flag {
        yield &stored // expected-error {{accessor must not yield more than once}}
      }
    }
  }
}

/// There should be no errors in this function.
struct TestMultipleYields2 {
  var stored: Int
  var flag: Bool
  var computed: Int {
    _read {
      if flag {
        yield stored
        return
      }
      yield stored
    }

    _modify {
      if flag {
        yield &stored
      } else {
        yield &stored
      }
    }
  }
}

 struct TestConvolutedPaths {
  var flag: Bool
  var stored : Int

  var computed: Int {
    _read {
      if flag {
        yield stored // expected-note {{previous yield was here}}
      }
      if !flag {
        yield stored // expected-error {{accessor must not yield more than once}}
      }
    }

    _modify {
      if flag {
        yield &stored // expected-note {{previous yield was here}}
      }
      if !flag {
        yield &stored // expected-error {{accessor must not yield more than once}}
      }
    }
  }
}

struct TestYieldInLoops {
  var stored: Int
  var count: Int
  var computed: Int {
    _read {
      for _ in 0..<count {
        yield stored // expected-error {{accessor must not yield more than once}}
                     // expected-note@-1 {{previous yield was here}}
      }
    }

    _modify {
      yield &stored // expected-note {{previous yield was here}}
      for _ in 0..<count {
        yield &stored // expected-error {{accessor must not yield more than once}}
      }
    }
  }
}

enum CompassPoint {
  case north
  case south
  case east
  case west
}

struct TestYieldInSwitch {
  var stored: Int
  var cp: CompassPoint

  var computed: Int {
    get { return stored }
    _modify {
      switch cp {
      case .north:
        yield &stored
      case .south:   // expected-note {{must yield in the switch case}}
        stored = 10
      case .east:
        yield &stored
      case .west:
        stored = 12
      }
      cp = .north
    } // expected-error {{accessor must yield on all paths before returning}}
  }
}

struct TestYieldInSwitchWithFallThrough {
  var stored: Int
  var cp: CompassPoint

  var computed: Int {
    // There should be no errors here.
    _read {
      switch cp {
      case .north:
        fallthrough
      case .south:
        fallthrough
      case .east:
        fallthrough
      case .west:
        yield stored
      }
    }

    // Fix me: the warning goes on a case with fall-through.
    _modify {
      switch cp {
      case .north: // expected-note {{must yield in the switch case}}
        fallthrough
      case .south:
        yield &stored
      case .east:
        fallthrough
      case .west:
        stored = 12
      }
    } // expected-error {{accessor must yield on all paths before returning}}
  }
}

struct TestExplicitReturn {
  var stored: Int
  var flag: Bool

  var computed: Int {
    mutating _read {
      if stored > 0 { // expected-note {{'if' must yield along the then branch}}
        return
      }
      if flag {
        yield stored
      } else {
        yield stored
      }
      flag = true
    } // expected-error {{accessor must yield on all paths before returning}}

    _modify {
      if stored > 0 { // expected-note {{'if' must yield along the then branch}}
        return
      }
      if stored == 0 {
        if flag {
          yield &stored
        } else {
          yield &stored
        }
        return
      }
      flag = true
    } // expected-error {{accessor must yield on all paths before returning}}
  }

  var anotherProp: Int {
    mutating _read {
      if flag {
        stored = 2
      }
      return // expected-error {{accessor must yield before returning}}

      if !flag { // expected-warning {{code after 'return' will never be executed}}
        stored = 3
      }
    }
  }
}

struct TestYieldsInGuards {
  var storedOpt: Int?
  var defaultStorage: Int

  var computed: Int {
    _read {
      guard let stored = storedOpt else {
        yield defaultStorage
        return
      }
      yield stored
    }
  }
}

struct TestYieldsInGuards2 {
  var storedOpt: Int?
  var defaultStorage: Int

  var computed: Int {
    _read {
      guard let stored = storedOpt else { // expected-note {{'guard' must yield along the else branch}}
        return
      }
      yield stored
    } // expected-error {{accessor must yield on all paths before returning}}

    _modify {
      guard let stored = storedOpt else {
        yield &defaultStorage
        return
      }                        // expected-note {{must yield after 'guard'}}
      storedOpt = stored + 1
    } // expected-error {{accessor must yield on all paths before returning}}
  }
}

// Diagnostics for do-catch and tries are not as good as the above.

enum SoftError : Error {
  case Ignorable
}
func aThrowingFunction() throws {
  throw SoftError.Ignorable
}

struct TestYieldInDoCatch {
  var stored: Int
  var computed: Int {
    _read {
      do {
        try aThrowingFunction()
        yield stored
      } catch {
        yield stored
      }
    }
  }
}

struct TestYieldInDoCatch2 {
  var stored: Int
  var computed: Int {
    _read {
      do {
        try aThrowingFunction() // expected-note {{branch must yield on all paths}}
        yield stored  // expected-note {{found yield along one path}}
      } catch {
      }
    } // expected-error {{accessor must yield on all paths before returning}}

    _modify {
      do {
        try aThrowingFunction() // expected-note {{branch must yield on all paths}}
      }
      catch {
        yield &stored  // expected-note {{found yield along one path}}
      }
    } // expected-error {{accessor must yield on all paths before returning}}
  }
}

enum Binary {
  case one
  case two
}

struct TestMutipleTries {
  var stored: Int
  var computed: Int {
    _read {
      do {
        try aThrowingFunction()
        yield stored  // expected-note {{previous yield was here}}

        try aThrowingFunction()
        yield stored // expected-error {{accessor must not yield more than once}}
      } catch {
      }
    }

    _modify {
      do {
        try aThrowingFunction() // expected-note {{branch must yield on all paths}}
        yield &stored           // expected-note {{found yield along one path}}

        try aThrowingFunction()
      }
      catch {
      }
    } // expected-error {{accessor must yield on all paths before returning}}
  }

  var flag: Bool
  var bin: Binary

  var computed2: Int {
    // This is a contrived example. Here the blame is associated with 'if'.
    // Ideally, the error must point to an absence of yield in a 'catch' and
    // blame the 'try'.
    _read {
      do {
        if flag {     // expected-note {{'if' must yield along the then branch}}
          try aThrowingFunction()
        } else {
          try aThrowingFunction()
        }

        yield stored
      } catch {
      }
    } // expected-error {{accessor must yield on all paths before returning}}

    _modify {
      do {
        switch bin {     // expected-note {{switch must yield in all cases}}
        case .one:
          try aThrowingFunction()
          yield &stored

        case .two:
          try aThrowingFunction()
          yield &stored
        }
      } catch {
      }
    } // expected-error {{accessor must yield on all paths before returning}}
  }

  var computed3: Int {
    _read {
      do {
        if flag {     // expected-note {{'if' must yield along the then branch}}
          try aThrowingFunction()
        } else {
          try aThrowingFunction()
        }
      } catch {
        yield stored
      }
    } // expected-error {{accessor must yield on all paths before returning}}

    _modify {
      do {
        switch bin {
        case .one:           // expected-note {{must yield in the switch case}}
          try aThrowingFunction()
        case .two:
          try aThrowingFunction()
        }
      } catch {
        yield &stored
      }
    } // expected-error {{accessor must yield on all paths before returning}}
  }
}

struct TestMutipleCatches {
  enum NumError: Error {
    case One
    case Two
    case Three
  }

  var stored: Int
  var computed: Int {
    _read {
      do {
        try aThrowingFunction() // expected-note {{branch must yield on all paths}}
        yield stored            // expected-note {{found yield along one path}}
      } catch NumError.One {
        yield stored
      } catch NumError.Two {
        yield stored
      } catch NumError.Three {
      } catch {
        yield stored
      }
    }  // expected-error {{accessor must yield on all paths before returning}}
  }
}

// Test for labeled breaks.

struct TestYieldWithLabeledBreaks {
  var stored: Int
  var flag: Bool
  var bin: Binary

  var computed: Int {
    _read {
      ifstmt: if flag {
        switch bin {
        case .one:
          yield stored
        case .two:
          break ifstmt
        }
      } // expected-note {{must yield when 'if' falls through}}
    } // expected-error {{accessor must yield on all paths before returning}}

    _modify {
      loop: while flag {
        switch bin {
        case .one:
          yield &stored // expected-error {{accessor must not yield more than once}}
                        // expected-note@-1 {{previous yield was here}}
        case .two:
          break loop
        }
      }
    }
  }
}


