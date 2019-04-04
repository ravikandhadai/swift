func foo(_ x: UnsafePointer<Int>) -> Int {
  return x.pointee
}

struct MyInterpolation: ExpressibleByStringInterpolation, ExpressibleByStringLiteral, StringInterpolationProtocol {
  
  typealias StringInterpolation = MyInterpolation

  public init(literalCapacity: Int, interpolationCount: Int) {
    str = ""
  }

  public mutating func appendLiteral(_ lit: String) {
   str += lit
  }
  
  public mutating func appendInterpolation(_ x: UnsafePointer<Int>, b y: Bool = false) {
    str += String(x.pointee)
  }

  public init(stringInterpolation: StringInterpolation) {
    str = stringInterpolation.str 
  }

  var str: String 

  public init(stringLiteral value: String) {
    str = value
  }
}

func bar() {
  var y = 10
  let z: MyInterpolation = "\(&y)"
  print(z.str)
}

bar()
