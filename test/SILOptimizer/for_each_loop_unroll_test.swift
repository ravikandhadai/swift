// RUN: %target-swift-frontend -emit-sil -primary-file %s | %FileCheck %s

// Tests for the ForEachLoopUnroll mandatory optimization pass that unrolls
// Sequence.forEach calls over array literals.

// CHECK-LABEL: sil hidden @$s25for_each_loop_unroll_test0D19LetArrayLiteralTestyyF : $@convention(thin) () -> ()
func unrollLetArrayLiteralTest() {
  let a = [Int64(15), Int64(27)]
  a.forEach { print($0) }
    // CHECK: [[LIT1:%[0-9]+]] = integer_literal $Builtin.Int64, 15
    // CHECK: [[INT1:%[0-9]+]] = struct $Int64 ([[LIT1]] : $Builtin.Int64)
    // CHECK: [[LIT2:%[0-9]+]] = integer_literal $Builtin.Int64, 27
    // CHECK: [[INT2:%[0-9]+]] = struct $Int64 ([[LIT2]] : $Builtin.Int64)
    // CHECK-NOT: forEach
    // CHECK: [[STACK:%[0-9]+]] = alloc_stack $Int64
    // CHECK: store [[INT1]] to [[STACK]]
    // CHECK: try_apply %{{.*}}([[STACK]]) : $@noescape @callee_guaranteed (@in_guaranteed Int64) -> @error Error, normal [[NORMAL:bb[0-9]+]], error [[ERROR:bb[0-9]+]]

    // CHECK: [[NORMAL]](%{{.*}} : $()):
    // CHECK: store [[INT2]] to [[STACK]] : $*Int64
    // CHECK: try_apply {{.*}}([[STACK]])
}
