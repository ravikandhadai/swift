// RUN: %target-swift-frontend -O -emit-sil -primary-file %s | %FileCheck %s

// These tests check whether DeadObjectElimination pass runs as a part of the
// optimization pipeline and eliminates dead array literals in Swift code.
// Note that DeadObjectElimination pass relies on @_semantics annotations on
// the array initializer that is used by the compiler to create array literals.
// This test would fail if in case the initializer used by the compiler to
// initialize array literals doesn't match the one expected by the pass.

// CHECK-LABEL: sil hidden @$s15dead_array_elim24testDeadArrayEliminationyyF
func testDeadArrayElimination() {
  _ = [1, 2, 3]
    // CHECK: bb0:
    // CHECK-NEXT: %{{.*}} = tuple ()
    // CHECK-NEXT: return %{{.*}} : $()
}

// CHECK-LABEL: sil hidden @$s15dead_array_elim29testEmptyDeadArrayEliminationyyF
func testEmptyDeadArrayElimination() {
  _ = []
    // CHECK: bb0:
    // CHECK-NEXT: %{{.*}} = tuple ()
    // CHECK-NEXT: return %{{.*}} : $()
}

// CHECK-LABEL: sil hidden @$s15dead_array_elim35testDeadArrayElimWithFixLifetimeUseyyF
func testDeadArrayElimWithFixLifetimeUse() {
  let a: [Int] = []
  _fixLifetime(a)
    // CHECK: bb0:
    // CHECK-NEXT: %{{.*}} = tuple ()
    // CHECK-NEXT: return %{{.*}} : $()
}
