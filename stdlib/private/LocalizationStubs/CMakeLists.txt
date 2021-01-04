add_swift_target_library(swiftLocalizationAnalysisTestHelper
  IS_SDK_OVERLAY
  SHARED

  LocalizationAnalysisTestHelper.swift
  
  SWIFT_MODULE_DEPENDS_IOS Darwin ObjectiveC Foundation
  SWIFT_MODULE_DEPENDS_OSX Darwin ObjectiveC Foundation
  SWIFT_MODULE_DEPENDS_TVOS Darwin ObjectiveC Foundation
  SWIFT_MODULE_DEPENDS_WATCHOS Darwin ObjectiveC Foundation
  TARGET_SDKS ALL_APPLE_PLATFORMS
  SWIFT_COMPILE_FLAGS ${SWIFT_STANDARD_LIBRARY_SWIFT_FLAGS}
  INSTALL_IN_COMPONENT never_install
  DARWIN_INSTALL_NAME_DIR "${SWIFT_DARWIN_STDLIB_PRIVATE_INSTALL_NAME_DIR}")
