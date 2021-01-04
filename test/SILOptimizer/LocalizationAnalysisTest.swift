// RUN: %target-swift-frontend -swift-version 5 -emit-sil -primary-file %s -o /dev/null 2>&1 | %FileCheck %s --check-prefix=CHECK --check-prefix=CHECK-%target-ptrsize
//
// REQUIRES: VENDOR=apple

// Tests for the OSLogOptimization pass that performs compile-time analysis
// and optimization of the new os log APIs. The tests here check whether specific
// compile-time constants such as the format string, the size of the byte buffer etc. are
// literals after the mandatory pipeline.

import LocalizationAnalysisTestHelper
import Foundation

func testSimpleInterpolation() -> Text {
  return Text("Minimum integer value: \(Int.min)")
}
