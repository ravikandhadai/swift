import OSLogPrototype
import Foundation 

func logMessages(_ h: Logger) {
    // Test logging of simple messages.
    h.log("A message with no data")

    // Test logging at specific levels.
    h.debug("Minimum integer value: \(Int.min, format: .hex)")
    h.info("Maximum integer value: \(Int.max, format: .hex)")

    let privateID = 0x79abcdef
    h.error("Private Identifier: \(privateID, format: .hex, privacy: .private)")
    let addr = 0x7afebabe
    h.fault("Invalid address: 0x\(addr, format: .hex, privacy: .public)")

    // Test logging with multiple arguments.
    let filePermissions = 0o777
    let pid = 122225
    h.error(
      """
      Access prevented: process \(pid) initiated by \
      user: \(privateID, privacy: .private) attempted resetting \
      permissions to \(filePermissions, format: .octal)
      """)
}

  func f4() {
    let h = Logger()
    h.log("a = c % d")
    h.log("Process failed after 99% completion")
    h.log("Double percents: %%")
  }

  func f6() {
    let h = Logger()
    h.log("\"Imagination is more important than knowledge\" - Einstein")
    h.log("\'Imagination is more important than knowledge\' - Einstein")
    h.log("Imagination is more important than knowledge \n - Einstein")
    h.log("Imagination is more important than knowledge - \\Einstein")
    h.log("The log message will be truncated here.\0 You won't see this")
  }

  func f7() {
    let h = Logger()
    h.log("dollar sign: \u{24}")
    h.log("black heart: \u{2665}")
    h.log("sparkling heart: \u{1F496}")
  }

  func f8() {
    let h = Logger()
    let x = 10

    h.log(#"There is no \(interpolated) value in this string"#)
    h.log(#"This is not escaped \n"#)
    h.log(##"'\b' is a printf escape character but not in Swift"##)
    h.log(##"The interpolated value is \##(x)"##)
    h.log(#"Sparkling heart should appear in the next line. \#n \#u{1F496}"#)
  }

  func f9() {
    let h = Logger()
    h.log("Smallest 32-bit integer value: \(Int32.min, format: .hex)")
  }

//  func f10() {
//    let h = Logger()
//
//    let smallString = "a"
//    h.log("A small string: \(smallString, privacy: .public)")
//
//    let largeString = "This is a large String"
//    h.log("\(largeString, privacy: .public)")
//
//    let concatString = "hello" + " - " + "world"
//    h.log("A dynamic string: \(concatString, privacy: .public)")
//
//    let interpolatedString = "\(31) trillion digits of pi are known so far"
//    h.log("\(interpolatedString)")
//  }

  func f11() {
    let h = Logger()

    let smallNSString: NSString = "a"
    h.log("A small string: \(smallNSString, privacy: .public)")

    let largeNSString: NSString = "This is a large String"
    h.log("\(largeNSString, privacy: .public)")

    let nsArray: NSArray = [0, 1, 2]
    h.log("NS Array: \(nsArray, privacy: .public)")

    let nsDictionary: NSDictionary = [1 : ""]
    h.log("NS Dictionary: \(nsDictionary, privacy: .public)")
  }
