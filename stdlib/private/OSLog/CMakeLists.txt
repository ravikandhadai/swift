add_swift_target_library(swiftOSLogPrototype
  IS_SDK_OVERLAY
  SHARED
  TARGET_LIBRARY

  OSLog.swift
  OSLogMessage.swift

  # This module is meant to be interpeted at compile-time and optimized in the
  # in the context of the client.. Optimizing this module interferes with the
  # interpreter by inlining/eliminating functions with @_semantics attributes.
  SWIFT_COMPILE_FLAGS "-Onone"

  SWIFT_MODULE_DEPENDS_IOS Darwin os
  SWIFT_MODULE_DEPENDS_OSX Darwin os
  SWIFT_MODULE_DEPENDS_TVOS Darwin os
  SWIFT_MODULE_DEPENDS_WATCHOS Darwin os
  TARGET_SDKS ALL_APPLE_PLATFORMS
  INSTALL_IN_COMPONENT never_install)
