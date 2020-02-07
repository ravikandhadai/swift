import Foundation 
import os

func logMessages() {
    // Test logging of simple messages.
    os_log("A message with no data")

    // Test logging at specific levels.
    os_log("Minimum integer value: %llx", type: .debug, Int.min)
    os_log("Maximum integer value: %llx", type: .info, Int.max)

    let privateID = 0x79abcdef
    os_log("Private Identifier: %{private}llx", type: .error, privateID)
    let addr = 0x7afebabe
    os_log("Invalid address: 0x%{public}llx", type: .fault, addr)

    // Test logging with multiple arguments.
    let filePermissions = 0o777
    let pid = 122225
    os_log(
      """
      Access prevented: process %lld initiated by \
      user: %{private}lld attempted resetting \
      permissions to %{public}lld
      """, 
      pid, privateID, filePermissions)
}

  func f4() {
    os_log("a = c % d")
    os_log("Process failed after 99% completion")
    os_log("Double percents: %%")
  }

  func f6() {
    
    os_log("\"Imagination is more important than knowledge\" - Einstein")
    os_log("\'Imagination is more important than knowledge\' - Einstein")
    os_log("Imagination is more important than knowledge \n - Einstein")
    os_log("Imagination is more important than knowledge - \\Einstein")
    os_log("The log message will be truncated here.\0 You won't see this")
  }

  func f7() {
    
    os_log("dollar sign: \u{24}")
    os_log("black heart: \u{2665}")
    os_log("sparkling heart: \u{1F496}")
  }

  func f8() {
    
    let x = 10

    os_log(#"There is no \(interpolated) value in this string"#)
    os_log(#"This is not escaped \n"#)
    os_log(##"'\b' is a printf escape character but not in Swift"##)
    os_log("The interpolated value is %lld", x)
    os_log(#"Sparkling heart should appear in the next line. \#n \#u{1F496}"#)
  }

  func f9() {
    os_log("Smallest 32-bit integer value: %x", Int32.min)
  }

//  func f10() {
//    let smallString = "a"
//    os_log("A small string: %{public}s", smallString)
//
//    let largeString = "This is a large String"
//    os_log("%{public}s", largeString)
//
//    let concatString = "hello" + " - " + "world"
//    os_log("A dynamic string: %{public}s", concatString)
//
//    let interpolatedString = "\(31) trillion digits of pi are known so far"
//    os_log("%s", interpolatedString)
//  }

  func f11() {
    let smallNSString: NSString = "a"
    os_log("A small string: %{public}@", smallNSString)

    let largeNSString: NSString = "This is a large String"
    os_log("%{public}@", largeNSString)

    let nsArray: NSArray = [0, 1, 2]
    os_log("NS Array: %{public}@", nsArray)

    let nsDictionary: NSDictionary = [1 : ""]
    os_log("NS Dictionary: %{public}@", nsDictionary)
  }
