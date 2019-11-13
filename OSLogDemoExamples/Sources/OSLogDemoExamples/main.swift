import Complex
import OSLogPrototype
import Foundation
import Network

func osLogExamples() {
  // Old os log
  let logObject = OSLog(subsystem: "com.apple.oslog.test", category: "demo")
  os_log("Minimum Int value: %{public}llx",
         log: logObject,
         type: .info,
         Int.min)

  // New os log API
  let logger = Logger(subsystem: "com.apple.oslog.test", category: "demo")
  
  logger.info("Minimum Int value: \(Int.min, format: .hex, privacy: .public)")
  
  let rwx = 0o777
  logger.error("Cannot set file permissions to \(rwx, format: .octal)")
}

osLogExamples()

extension OSLogInterpolation {
  
  @_semantics("constant_evaluable")
  @inlinable
  @_optimize(none)
  public mutating func appendInterpolation(
    _ date: @autoclosure @escaping () -> Date,
    privacy: Privacy = .public
  ) {
    appendInterpolation(
      Int32(date().timeIntervalSince1970.rounded()),
      format: .time_t,
      privacy: privacy)
  }

  @_semantics("constant_evaluable")
  @inlinable
  @_optimize(none)
  @available(macOS 10.14, *)
  public mutating func appendInterpolation(
    _ ipaddr: @autoclosure @escaping () -> IPv4Address,
    privacy: Privacy = .public
  ) {
    appendInterpolation(
      ipaddr().rawValue.withUnsafeBytes {
        $0.load(as: Int32.self)
      },
      format: .ipaddr,
      privacy: privacy)
  }
}

func osLogDarwinExamples() {
  let defaultLogger = Logger(subsystem: "test", category: "new os log")

  // Logging Date and IP address.
  let date = Date()
  
  // Old os log
  let dateFormatter = DateFormatter()
  dateFormatter.dateStyle = .medium
  dateFormatter.timeStyle = .medium
  os_log(.info, "Current date: %{public}s", dateFormatter.string(from: date))
  os_log(.info, "Current time stamp: %{time_t}d",
        Int32(date.timeIntervalSince1970.rounded()))
  
  let rawIPAddress: Int32 = 0x0100007f
  os_log("IP address: %{network:in_addr}d", rawIPAddress)
  
  // New os log
  defaultLogger.info("Time stamp: \(Date())")
  defaultLogger.info("Local host: \(IPv4Address.loopback)")
}

osLogDarwinExamples()

// Swift Numerics Package: Complex Number Example

extension OSLogInterpolation {
  @_semantics("constant_evaluable")
  mutating func appendInterpolation(
    _ complexNumber: @autoclosure @escaping () -> Complex<Double>
  ) {
    // [real, imaginary i]
    appendLiteral("(")
    appendInterpolation(complexNumber().real)
    appendLiteral(",")
    appendInterpolation(complexNumber().imaginary)
    appendLiteral("i)")
  }
}

func osLogComplexNumber() {
  let defaultLogger = Logger(subsystem: "test", category: "new os log")
  // New os log
  defaultLogger.info("Complex Number: \(Complex(1,1))")
}

osLogComplexNumber()

