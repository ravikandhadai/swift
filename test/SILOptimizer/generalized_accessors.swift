// RUN: %target-swift-frontend -emit-sil -primary-file %s -o /dev/null -verify
//
// Tests for yield-once diagnostics emitted for generalized accessors.

struct TestNoYield {
  var computed: Int {
    _read {
    } // expected-error {{accessor doesn't yield before returning}}

    _modify {
    } // expected-error {{accessor doesn't yield before returning}}
  }
}

struct TestReturnPathWithoutYield {
  var stored: Int
  var flag: Bool

  var computed: Int {
    mutating _read {
      if flag {
        yield stored
      }
      flag = true // expected-note {{some paths reaching here have a yield and some don't}}
    } // expected-error {{cannot guarantee that the accessor yields a value before returning}}

    _modify {
      // The diagnostics should attach the note to the earliest conflicting branch.
      if flag {
        yield &stored
      }

      if !flag { // expected-note {{some paths reaching here have a yield and some don't}}
        flag = true
      }
    } // expected-error {{cannot guarantee that the accessor yields a value before returning}}
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
      yield stored // expected-error {{encountered multiple yields along a single path in yield_once accessor}}
    }

    _modify {
      yield &stored // expected-note {{previous yield was here}}
      if flag {
        yield &stored // expected-error {{encountered multiple yields along a single path in yield_once accessor}}
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

/// This test case has both multiple-yield error and also return-before-yield
/// error. It is okay to report one of the errors.
struct TestConvolutedPaths {
  var flag: Bool
  var stored : Int

  var computed: Int {
    _read {
      if flag {
        yield stored // expected-note {{previous yield was here}}
      }
      if !flag {
        yield stored // expected-error {{encountered multiple yields along a single path in yield_once accessor}}
      }
    }

    _modify {
      if flag {
        yield &stored // // expected-note {{previous yield was here}}
      }
      if !flag {
        yield &stored // expected-error {{encountered multiple yields along a single path in yield_once accessor}}
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
        yield stored // expected-error {{encountered multiple yields along a single path in yield_once accessor}}
                     // expected-note@-1 {{previous yield was here}}
      }
    }

    _modify {
      yield &stored // expected-note {{previous yield was here}}
      for _ in 0..<count {
        yield &stored // expected-error {{encountered multiple yields along a single path in yield_once accessor}}
      }
    }
  }
}

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
        try aThrowingFunction()
        yield stored
      } catch {
      }
    } // expected-error {{cannot guarantee that the accessor yields a value before returning}}
      // expected-note@-1 {{some paths reaching here have a yield and some don't}}

    _modify {
      do {
        try aThrowingFunction()
      }
      catch {
        yield &stored
      }
    } // expected-error {{cannot guarantee that the accessor yields a value before returning}}
      // expected-note@-1 {{some paths reaching here have a yield and some don't}}
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
      case .south:
        stored = 10
      case .east:
        yield &stored
      case .west:
        stored = 12
      }
      cp = .north // expected-note {{some paths reaching here have a yield and some don't}}
    } // expected-error {{cannot guarantee that the accessor yields a value before returning}}
  }
}

struct TestExplicitReturn {
  var stored: Int
  var flag: Bool

  var computed: Int {
    mutating _read {
      if stored > 0 {
        return
      }
      if flag {
        yield stored
      } else {
        yield stored
      }
      flag = true
    } // expected-error {{cannot guarantee that the accessor yields a value before returning}}
    // expected-note@-1 {{some paths reaching here have a yield and some don't}}

    _modify {
      if stored > 0 {
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
    } // expected-error {{cannot guarantee that the accessor yields a value before returning}}
    // expected-note@-1 {{some paths reaching here have a yield and some don't}}
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
      guard let stored = storedOpt else {
        return
      }
      yield stored
    } // expected-error {{cannot guarantee that the accessor yields a value before returning}}
    // expected-note@-1 {{some paths reaching here have a yield and some don't}}

    _modify {
      guard let stored = storedOpt else {
        yield &defaultStorage
        return
      }
      storedOpt = stored + 1
    } // expected-error {{cannot guarantee that the accessor yields a value before returning}}
    // expected-note@-1 {{some paths reaching here have a yield and some don't}}
  }
}
