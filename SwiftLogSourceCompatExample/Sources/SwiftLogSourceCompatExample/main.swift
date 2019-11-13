import Foundation
import Logging

func logMessages(_ logger: Logger) {
  let timeout = 10
  logger.debug("Task completed after \(timeout) seconds");
  logger.notice("Exception caught at \(#file):\(#line)");
  
  let fileName = "Package.swift"
  logger.warning("File with the given name already exists: \(fileName)")
  logger.error("Cannot reset file permissions to \(0o777)")
  logger.critical("Exiting process due to error: \(0x0f)")
}

let logger = Logger(label: "com.example.BestExampleApp.main")
logMessages(logger)
