import os

func osLogExamples() {
  // New os log API
  let logger = Logger(subsystem: "com.apple.oslog.test", category: "demo")
  
  logger.info("Minimum Int value: \(Int.min, format: .hex, privacy: .public)")
  
  let rwx = 0o777
  logger.error("Cannot set file permissions to \(rwx, format: .octal)")
}

osLogExamples()











//func loggerTypeCheck(_ abstractLogger: Any) {
//  if (abstractLogger is Logger) {
//    (abstractLogger as! Logger).info("success")
//  }
//}
//
//loggerTypeCheck(Logger())
