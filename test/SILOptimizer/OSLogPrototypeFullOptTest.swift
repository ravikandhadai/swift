// RUN: %target-swift-frontend -emit-ir -swift-version 5 -O -primary-file %s | %FileCheck %s --check-prefix=CHECK --check-prefix=CHECK-%target-ptrsize
// REQUIRES: OS=macosx || OS=ios || OS=tvos || OS=watchos

// This tests the optimality of the IR generated for the new os log APIs. This
// is not testing the output of a specific optimization pass (which has separate
// tests) but that all optimizations together result in optimal IR. If this test
// fails, it implies that some expected optimizations fail to get triggered on
// os log APIs. TODO: eventually these optimization should also happen in Onone
// mode.

import OSLogPrototype
import Foundation

// CHECK-LABEL: define hidden swiftcc void @"${{.*}}testSimpleInterpolation
@available(OSX 10.12, iOS 10.0, watchOS 3.0, tvOS 10.0, *)
func testSimpleInterpolation(h: Logger) {
  h.log(level: .debug, "Minimum integer value: \(Int.min)")
    // CHECK: entry:
    // CHECK-NEXT: [[LOGLEVEL:%.+]] = tail call swiftcc i8 @"$sSo13os_log_type_ta0A0E5debugABvgZ"()
    // CHECK-NEXT: tail call swiftcc %TSo9OS_os_logC* @"$s14OSLogPrototype6LoggerV9logObjectSo06OS_os_D0Cvg"
    // CHECK-NEXT: [[LOGOBJ:%.+]] = bitcast %TSo9OS_os_logC*
    // CHECK-NEXT: tail call zeroext i1 @os_log_type_enabled
    // CHECK-NEXT: br i1 {{%.*}}, label %[[ENABLED:[0-9]+]], label %[[NOT_ENABLED:[0-9]+]]

    // CHECK: [[ENABLED]]:
    // CHECK-NEXT: [[BUFFER:%.+]] = tail call noalias i8* @swift_slowAlloc(i64 12
    // CHECK-NEXT: store i8 0, i8* [[BUFFER]], align 1
    // CHECK-NEXT: [[OFFSET1:%.+]] = getelementptr inbounds i8, i8* [[BUFFER]], i64 1
    // CHECK-NEXT: store i8 1, i8* [[OFFSET1]], align 1
    // CHECK-NEXT: [[OFFSET2:%.+]] = getelementptr inbounds i8, i8* [[BUFFER]], i64 2
    // CHECK-NEXT: store i8 2, i8* [[OFFSET2]], align 1
    // CHECK-NEXT: [[OFFSET3:%.+]] = getelementptr inbounds i8, i8* [[BUFFER]], i64 3
    // CHECK-NEXT: store i8 8, i8* [[OFFSET3]], align 1
    // CHECK-NEXT: [[OFFSET4:%.+]] = getelementptr inbounds i8, i8* [[BUFFER]], i64 4
    // CHECK-NEXT: [[BITCASTED:%.+]] = bitcast i8* [[OFFSET4]] to i64*
    // CHECK-NEXT: store i64 -9223372036854775808, i64* [[BITCASTED]], align 1
    // CHECK-NEXT: tail call void @_os_log_impl({{.*}}, {{.*}} [[LOGOBJ]], i8 zeroext [[LOGLEVEL]], i8* getelementptr inbounds ([36 x i8], [36 x i8]* @{{.*}}, i64 0, i64 0), i8* [[BUFFER]], i32 12)
    // CHECK-NEXT: tail call void @swift_slowDealloc(i8* [[BUFFER]]
    // CHECK-NEXT: br label %[[NOT_ENABLED]]

    // CHECK: [[NOT_ENABLED]]:
    // CHECK-NEXT: bitcast
    // CHECK-NEXT: tail call void @llvm.objc.release
    // CHECK-NEXT: ret void
}
