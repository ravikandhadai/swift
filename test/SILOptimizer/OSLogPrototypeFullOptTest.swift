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
    //
    // Header bytes.
    //
    // CHECK-NEXT: [[BUFFER:%.+]] = tail call noalias i8* @swift_slowAlloc(i64 12
    // CHECK-NEXT: store i8 0, i8* [[BUFFER]], align 1
    // CHECK-NEXT: [[OFFSET1:%.+]] = getelementptr inbounds i8, i8* [[BUFFER]], i64 1
    // CHECK-NEXT: store i8 1, i8* [[OFFSET1]], align 1
    //
    // Argument bytes.
    //
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

// CHECK-LABEL: define hidden swiftcc void @"${{.*}}testInterpolationWithMultipleArguments
@available(OSX 10.12, iOS 10.0, watchOS 3.0, tvOS 10.0, *)
func testInterpolationWithMultipleArguments(h: Logger) {
  let privateID = 0x79abcdef
  let filePermissions = 0o777
  let pid = 122225
  h.log(
    level: .error,
    """
    Access prevented: process \(pid) initiated by \
    user: \(privateID, privacy: .private) attempted resetting \
    permissions to \(filePermissions, format: .octal)
    """)
    // CHECK: entry:
    // CHECK-NEXT: [[LOGLEVEL:%.+]] = tail call swiftcc i8 @"$sSo13os_log_type_ta0A0E5errorABvgZ"()
    // CHECK-NEXT: tail call swiftcc %TSo9OS_os_logC* @"$s14OSLogPrototype6LoggerV9logObjectSo06OS_os_D0Cvg"
    // CHECK-NEXT: [[LOGOBJ:%.+]] = bitcast %TSo9OS_os_logC*
    // CHECK-NEXT: tail call zeroext i1 @os_log_type_enabled
    // CHECK-NEXT: br i1 {{%.*}}, label %[[ENABLED:[0-9]+]], label %[[NOT_ENABLED:[0-9]+]]

    // CHECK: [[ENABLED]]:
    //
    // Header bytes.
    //
    // CHECK-NEXT: [[BUFFER:%.+]] = tail call noalias i8* @swift_slowAlloc(i64 32
    // CHECK-NEXT: store i8 1, i8* [[BUFFER]], align 1
    // CHECK-NEXT: [[OFFSET1:%.+]] = getelementptr inbounds i8, i8* [[BUFFER]], i64 1
    // CHECK-NEXT: store i8 3, i8* [[OFFSET1]], align 1
    //
    // First argument bytes.
    //
    // CHECK-NEXT: [[OFFSET2:%.+]] = getelementptr inbounds i8, i8* [[BUFFER]], i64 2
    // CHECK-NEXT: store i8 2, i8* [[OFFSET2]], align 1
    // CHECK-NEXT: [[OFFSET3:%.+]] = getelementptr inbounds i8, i8* [[BUFFER]], i64 3
    // CHECK-NEXT: store i8 8, i8* [[OFFSET3]], align 1
    // CHECK-NEXT: [[OFFSET4:%.+]] = getelementptr inbounds i8, i8* [[BUFFER]], i64 4
    // CHECK-NEXT: [[BITCASTED:%.+]] = bitcast i8* [[OFFSET4]] to i64*
    // CHECK-NEXT: store i64 122225, i64* [[BITCASTED]], align 1
    //
    // Second argument
    //
    // CHECK-NEXT: [[OFFSET12:%.+]] = getelementptr inbounds i8, i8* [[BUFFER]], i64 12
    // CHECK-NEXT: store i8 1, i8* [[OFFSET12]], align 1
    // CHECK-NEXT: [[OFFSET13:%.+]] = getelementptr inbounds i8, i8* [[BUFFER]], i64 13
    // CHECK-NEXT: store i8 8, i8* [[OFFSET13]], align 1
    // CHECK-NEXT: [[OFFSET14:%.+]] = getelementptr inbounds i8, i8* [[BUFFER]], i64 14
    // CHECK-NEXT: [[BITCASTED2:%.+]] = bitcast i8* [[OFFSET14]] to i64*
    // CHECK-NEXT: store i64 2041302511, i64* [[BITCASTED2]], align 1
    //
    // Third argument
    //
    // CHECK-NEXT: [[OFFSET22:%.+]] = getelementptr inbounds i8, i8* [[BUFFER]], i64 22
    // CHECK-NEXT: store i8 2, i8* [[OFFSET22]], align 1
    // CHECK-NEXT: [[OFFSET23:%.+]] = getelementptr inbounds i8, i8* [[BUFFER]], i64 23
    // CHECK-NEXT: store i8 8, i8* [[OFFSET23]], align 1
    // CHECK-NEXT: [[OFFSET24:%.+]] = getelementptr inbounds i8, i8* [[BUFFER]], i64 24
    // CHECK-NEXT: [[BITCASTED3:%.+]] = bitcast i8* [[OFFSET24]] to i64*
    // CHECK-NEXT: store i64 511, i64* [[BITCASTED3]], align 1
    //
    // os_log_impl call.
    // CHECK-NEXT: tail call void @_os_log_impl({{.*}}, {{.*}} [[LOGOBJ]], i8 zeroext [[LOGLEVEL]], i8* getelementptr inbounds ([120 x i8], [120 x i8]* @{{.*}}, i64 0, i64 0), i8* [[BUFFER]], i32 32)
    // CHECK-NEXT: tail call void @swift_slowDealloc(i8* [[BUFFER]]
    // CHECK-NEXT: br label %[[NOT_ENABLED]]

    // CHECK: [[NOT_ENABLED]]:
    // CHECK-NEXT: bitcast
    // CHECK-NEXT: tail call void @llvm.objc.release
    // CHECK-NEXT: ret void
}

// CHECK-LABEL: define hidden swiftcc void @"${{.*}}testNSObjectInterpolation
@available(OSX 10.12, iOS 10.0, watchOS 3.0, tvOS 10.0, *)
func testNSObjectInterpolation(h: Logger, nsArray: NSArray) {
  h.log("NSArray: \(nsArray, privacy: .public)")
    // CHECK: entry:
    // CHECK-NEXT: bitcast %TSo7NSArrayC* %1 to i8*
    // CHECK-NEXT: [[NSARRAY_ARG:%.+]] = tail call i8* @llvm.objc.retain
    // CHECK-NEXT: [[LOGLEVEL:%.+]] = tail call swiftcc i8 @"$sSo13os_log_type_ta0A0E7defaultABvgZ"()
    // CHECK-NEXT: tail call swiftcc %TSo9OS_os_logC* @"$s14OSLogPrototype6LoggerV9logObjectSo06OS_os_D0Cvg"
    // CHECK-NEXT: [[LOGOBJ:%.+]] = bitcast %TSo9OS_os_logC*
    // CHECK-NEXT: tail call zeroext i1 @os_log_type_enabled
    // CHECK-NEXT: br i1 {{%.*}}, label %[[ENABLED:[0-9]+]], label %[[NOT_ENABLED:[0-9]+]]

    // CHECK: [[ENABLED]]:
    //
    // Header bytes.
    //
    // CHECK-NEXT: [[BUFFER:%.+]] = tail call noalias i8* @swift_slowAlloc(i64 12
    // CHECK-NEXT: store i8 2, i8* [[BUFFER]], align 1
    // CHECK-NEXT: [[OFFSET1:%.+]] = getelementptr inbounds i8, i8* [[BUFFER]], i64 1
    // CHECK-NEXT: store i8 1, i8* [[OFFSET1]], align 1
    //
    // Argument bytes.
    //
    // CHECK-NEXT: [[OFFSET2:%.+]] = getelementptr inbounds i8, i8* [[BUFFER]], i64 2
    // CHECK-NEXT: store i8 66, i8* [[OFFSET2]], align 1
    // CHECK-NEXT: [[OFFSET3:%.+]] = getelementptr inbounds i8, i8* [[BUFFER]], i64 3
    // CHECK-NEXT: store i8 8, i8* [[OFFSET3]], align 1
    // CHECK-NEXT: [[OFFSET4:%.+]] = getelementptr inbounds i8, i8* [[BUFFER]], i64 4
    // CHECK-NEXT: [[BITCASTED_DEST:%.+]] = bitcast i8* [[OFFSET4]] to %TSo7NSArrayC**
    // CHECK-NEXT: [[BITCASTED_SRC:%.+]] = bitcast i8* [[NSARRAY_ARG]] to %TSo7NSArrayC*
    // CHECK-NEXT: store %TSo7NSArrayC*  [[BITCASTED_SRC]], %TSo7NSArrayC*  [[BITCASTED_DEST]], align 1
    // CHECK-NEXT: tail call void @_os_log_impl({{.*}}, {{.*}} [[LOGOBJ]], i8 zeroext [[LOGLEVEL]], i8* getelementptr inbounds ([20 x i8], [20 x i8]* @{{.*}}, i64 0, i64 0), i8* [[BUFFER]], i32 12)
    // CHECK-NEXT: tail call void @swift_slowDealloc(i8* [[BUFFER]]
    // CHECK-NEXT: br label %[[NOT_ENABLED]]

    // CHECK: [[NOT_ENABLED]]:
    // CHECK-NEXT: tail call void @llvm.objc.release(i8* [[NSARRAY_ARG]])
    // CHECK-NEXT: bitcast
    // CHECK-NEXT: tail call void @llvm.objc.release
    // CHECK-NEXT: ret void
}

// TODO: add test for String.
