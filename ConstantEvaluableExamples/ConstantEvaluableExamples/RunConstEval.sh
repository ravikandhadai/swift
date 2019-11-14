#!/bin/sh

SWIFT_TOOLS_DIR="/Users/ravi/swift-branch-1/build/Ninja-RelWithDebInfoAssert/swift-macosx-x86_64/bin"
SWIFT_TEMP_DIR="/Users/ravi/swift-branch-1/swift/ConstantEvaluableExamples/TempSILs"
SWIFT_SOURCE="/Users/ravi/swift-branch-1/swift/test/SILOptimizer/constant_evaluable_subset_test.swift"
#${SWIFT_TOOLS_DIR}/swiftc -frontend -emit-silgen -primary-file ${SWIFT_SOURCE} -o ${SWIFT_TEMP_DIR}/constant_evaluable_subset_test_silgen.sil
${SWIFT_TOOLS_DIR}/swiftc -frontend -emit-sil -enable-experimental-static-assert  ${SWIFT_TEMP_DIR}/constant_evaluable_subset_test_silgen.sil > /dev/null 2> /dev/null
