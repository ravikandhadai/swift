add_swift_target_library(swiftOSLogPrototype
  IS_SDK_OVERLAY
  SHARED
  TARGET_LIBRARY

  OSLog.swift
  OSLogMessage.swift

  # This module has code that must be constant evaluated at compile time in the
  # context of the client. Optimizing this module interferes with the constant
  # evaluator by inlining/eliminating functions with @_semantics attributes.
  SWIFT_COMPILE_FLAGS "-Onone"

  SWIFT_MODULE_DEPENDS_IOS Darwin os SwiftOnoneSupport
  SWIFT_MODULE_DEPENDS_OSX Darwin os SwiftOnoneSupport
  SWIFT_MODULE_DEPENDS_TVOS Darwin os SwiftOnoneSupport
  SWIFT_MODULE_DEPENDS_WATCHOS Darwin os SwiftOnoneSupport
  TARGET_SDKS ALL_APPLE_PLATFORMS
  INSTALL_IN_COMPONENT never_install)
