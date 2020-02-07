// Notes and observations on the performance:
// Info level writes into an in memory buffer and this where there is a huge
// performance improvement with the new APIs.
// Debug level does nothing and it is huge.
// Error and fault levels are dominated by the _impl call in running and the
// savings is hardly 2%.
// New swift os log was run in -O mode. (For clang and old os_log the -O mode doesn't matter).

// Current experimental data for performance:
//  All of these experiments use default configuration. Time is for 100k log calls.
//
//  Logging scalars at "debug" level: (which is disabled. This is time taken for noop logging)
//    - new Swift os log: around 5.5ms (library call time: 0ms)
//    - old Swift os log: arond 18ms
//    - C os log: around 3.5ms
//
//  Logging scalars at "info" level:
//    - new Swift os log: around 50ms  library call time: around 30ms (Swift overheads are about 20ms here)
//    - old Swift os log: around 180ms
//    - C os log: around 40ms
//
//  Logging scalars at "error" level: (error and fault levels have similar performance characteristic)
//  he performance here is almost identical in all cases.
//    - new Swift os log: around 1500ms  library call time: around 1480ms (sometimes it is worse than the old one)
//    - old Swift os log: around 1500ms
//    - C os log: around 1500ms
//  The dominating factor seems to tbe library call.

// Logging strings at "debug" level
//    - new Swift os log: around 6ms (library call time: 0ms)
//    - old Swift os log: arond 18ms
//    - C os log: around 3.5ms
//
// Logging strings at "info" level
//    - new Swift os log: around 140ms library call time: 80ms (Swift overheads are about 60ms to 70ms for string about 50%)
//    - old Swift os log: arond 190ms
//    - C os log: around 110ms
//
// Logging strings at "error" level
//    - new Swift os log: around 3400ms library call time: 3332ms (Swift overheads are about 60ms to 70ms around 3%)
//    - old Swift os log: arond 3340ms
//    - C os log: around 3200 to 3400ms
//  Again, the library call overhead dominates for logging at error level.

// Logging NSObjects at "debug" level
//    - new Swift os log: around 6ms (library call time: 0ms)
//    - old Swift os log: arond 18ms
//    - C os log: around 3.5ms
//
// Logging NSObjects at "info" level
//    - new Swift os log: around 460ms library call time: 430ms (Swift overheads are about 30ms)
//    - old Swift os log: around 570ms
//    - C os log: around around  440ms
// Logging NSObjects at "error" level
//    - new Swift os log: around 1360ms library call time: 1325ms (Swift overheads are about 35ms)
//    - old Swift os log: around 1360ms
//    - C os log: around around  1360ms

// Binary size measurements
// A file with about 31 os log calls logging strings, scalars and NSObjects
//      - new Swift os log: 37k (about 30% bigger than the old Swift os log)
//      - old Swift os log: 28k
//
// Without strings
//        - new Swift os log: 31k
//        - old Swift os log: 27k

// Run command: bin/swiftc ~/swift-branch-1/oslogPerformanceTest.swift -sdk /Applications/Xcodes/YellowstoneE11E81.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.15.sdk -O

import OSLogPrototype
import Dispatch
import Foundation

func newLog(_ dynamicString: String, _ nsArray: NSArray) -> UInt64 {
 	// Test logging of simple messages.
// 	let privateID = 0x79abcdef
//  let filePermissions = 0o777
//  let pid = 122225
  let h = Logger()
//  let implNano = h.log(
//    level: .info,
//    """
//    Access prevented: process \(pid) initiated by \
//    user: \(privateID, privacy: .private) attempted resetting \
//    permissions to \(filePermissions, format: .octal)
//    """)
//  h.log("\(privateID)")
  //return h.log(level: .error, "some random string \(dynamicString)")
  return h.log(level: .error, "some random object \(nsArray)")
}

func oldLog(_ dynamicString: String, _ nsArray: NSArray) {
//  let privateID = 0x79abcdef
//  let filePermissions = 0o777
//  let pid = 122225
//  os_log(.info, "Access prevented: process %lld initiated by user: %{private}lld attempted resetting permissions to %llo", pid, privateID,
//      filePermissions)
//  os_log("%lld", privateID)
  //os_log(.error, "some random string %s", dynamicString)
  os_log(.error, "some random object %@", nsArray)
}

var i = 0
let dynamicString = "some large text that wouldn't fit into small string" +
  "Another suffix so that the string representation is complex"
let nsArray: NSArray = [0, 1, 2]

//let startTimeOld = DispatchTime.now()
//i = 0
//while (i < 100000) {
//  oldLog(dynamicString, nsArray)
//  i += 1
//}
//let endTimeOld = DispatchTime.now()
//let timeIntervalOld =
//  Double(
//    endTimeOld.uptimeNanoseconds -
//      startTimeOld.uptimeNanoseconds) / 1_000_000
//
//print("Old log API time: \(timeIntervalOld)ms")

let startTimeNew = DispatchTime.now()
var implNano: UInt64 = 0
i = 0
while (i < 100000) {
  implNano += newLog(dynamicString, nsArray)
  i += 1
}
let endTimeNew = DispatchTime.now()
let timeIntervalNew =
  Double(
    endTimeNew.uptimeNanoseconds -
     startTimeNew.uptimeNanoseconds) / 1_000_000

let avgImpltime = Double(implNano) / 1_000_000

print("New log API time: \(timeIntervalNew)ms Impl time: \(avgImpltime)ms")

//let startTimePrint = DispatchTime.now()
//i = 0
//while (i < 100000) {
//  let privateID = 0x79abcdef
//  let filePermissions = 0o777
//  let pid = 122225
//  //NSLog("hello world \(dynamicString)")
//  print("""
//  Access prevented: process \(pid) initiated by \
//  user: \(privateID) attempted resetting \
//  permissions to \(filePermissions)
//  """)
//  print("\(dynamicString)")
//  i += 1
//}
//let endTimePrint = DispatchTime.now()
//let timeIntervalPrint =
//  Double(
//    endTimePrint.uptimeNanoseconds -
//      startTimePrint.uptimeNanoseconds) / 1_000_000
//
//print("Printing time: \(timeIntervalPrint)ms")
