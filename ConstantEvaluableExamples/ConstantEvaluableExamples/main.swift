@_semantics("constant_evaluable")
internal func leftShift(x: Int, y: Int) -> Int {
  return x << y
}

@_semantics("test_driver")
internal func interpretLeftShift() -> Int {
  return leftShift(x: 34, y: 3)
}

@_semantics("constant_evaluable")
internal func testInvalidIntTruncations(a: Int32) -> Int8 {
  return Int8(a)
}

@_semantics("test_driver")
internal func interpretInvalidIntTruncations() -> Int8 {
  return testInvalidIntTruncations(a: 130)
}

@_semantics("constant_evaluable")
func testIntAddOverflow(_ x: Int8) -> Int8 {
  return x + 1
}

@_semantics("test_driver")
func interpretIntAddOverflow() -> Int8 {
  return testIntAddOverflow(127)
}

@_semantics("constant_evaluable")
func testDivideByZero(_ x: Int, _ y: Int) -> Int {
  return x / y
}

@_semantics("test_driver")
func interpretDivideByZero() -> Int {
  return testDivideByZero(127, 0)
}

@_semantics("constant_evaluable")
func testDividingFullWidthByZero(_ x: Int, _ y: Int, _ z: UInt) -> Int {
  return x.dividingFullWidth((y, z)).1
}

@_semantics("test_driver")
func interpretDividingFullWidthByZero() -> Int {
  return testDividingFullWidthByZero(0, 1, 1)
}

@_semantics("constant_evaluable")
func testDivideOverflow(_ x: Int8, _ y: Int8) -> Int8 {
  return x / y
}

@_semantics("test_driver")
func interpretDivideOverflow() -> Int8 {
  return testDivideOverflow(-128, -1)
}

@_semantics("constant_evaluable")
func testDistance(_ x: UInt, _ y: UInt) -> Int {
  return x.distance(to: y)
}

@_semantics("test_driver")
func interpretDistanceTest() -> Int {
  return testDistance(0, UInt.max)
}

