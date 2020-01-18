// RUN: %empty-directory(%t)
// RUN: %{python} %utils/chex.py < %s > %t/class_resilience.swift
// RUN: %target-swift-frontend -emit-module -enable-library-evolution -emit-module-path=%t/resilient_struct.swiftmodule -module-name=resilient_struct %S/../Inputs/resilient_struct.swift
// RUN: %target-swift-frontend -emit-module -enable-library-evolution -emit-module-path=%t/resilient_enum.swiftmodule -module-name=resilient_enum -I %t %S/../Inputs/resilient_enum.swift
// RUN: %target-swift-frontend -emit-module -enable-library-evolution -emit-module-path=%t/resilient_class.swiftmodule -module-name=resilient_class -I %t %S/../Inputs/resilient_class.swift
// RUN: %target-swift-frontend -emit-module -enable-library-evolution -emit-module-path=%t/fixed_layout_class.swiftmodule -module-name=fixed_layout_class -I %t %S/../Inputs/fixed_layout_class.swift
// RUN: %target-swift-frontend -I %t -emit-ir -enable-library-evolution %t/class_resilience.swift | %FileCheck %t/class_resilience.swift --check-prefix=CHECK --check-prefix=CHECK-%target-ptrsize --check-prefix=CHECK-%target-runtime -DINT=i%target-ptrsize
// RUN: %target-swift-frontend -I %t -emit-ir -enable-library-evolution -O %t/class_resilience.swift

// This tests @_fixed_layout classes in resilient modules.
import fixed_layout_class

// CHECK-LABEL: define{{( dllexport)?}}{{( protected)?}} swiftcc i32 @"$s16class_resilience20useRootClassPropertyys5Int32V013fixed_layout_A0026OutsideParentWithResilientF0CF"(%T18fixed_layout_class34OutsideParentWithResilientPropertyC*)
public func useRootClassProperty(_ o: OutsideParentWithResilientProperty) -> Int32 {
  // CHECK: getelementptr inbounds %T18fixed_layout_class34OutsideParentWithResilientPropertyC, %T18fixed_layout_class34OutsideParentWithResilientPropertyC* %0, i32 0, i32 1
  let point = o.p
  // CHECK: load [[INT]], [[INT]]* @"$s18fixed_layout_class34OutsideParentWithResilientPropertyC1s16resilient_struct4SizeVvpWvd"
  let size = o.s
  // CHECK: load [[INT]], [[INT]]* @"$s18fixed_layout_class34OutsideParentWithResilientPropertyC5colors5Int32VvpWvd"
  let color = o.color
  return Int32(point.x) + Int32(size.h) + color
  // CHECK: ret i32
}

// CHECK-LABEL: define{{( dllexport)?}}{{( protected)?}} swiftcc void @"$s16class_resilience19useSubclassPropertyyy013fixed_layout_A012OutsideChildCF"(%T18fixed_layout_class12OutsideChildC*)
public func useSubclassProperty(_ o: OutsideChild) {
  // CHECK: getelementptr inbounds %T18fixed_layout_class13OutsideParentC, %T18fixed_layout_class13OutsideParentC* %4, i32 0, i32 1
  _ = o.property
  // CHECK: getelementptr inbounds %T18fixed_layout_class12OutsideChildC, %T18fixed_layout_class12OutsideChildC* %0, i32 0, i32 2
  _ = o.childProperty
  // CHECK: ret void
}

// CHECK-LABEL: define{{( dllexport)?}}{{( protected)?}} swiftcc void @"$s16class_resilience27useGenericRootClassPropertyyx013fixed_layout_A00D13OutsideParentCyxGlF"(%swift.opaque* noalias nocapture sret, %T18fixed_layout_class20GenericOutsideParentC*)
public func useGenericRootClassProperty<A>(_ o: GenericOutsideParent<A>) -> A {
  // -- we load the base offset twice, first to get the generic parameter out and
  // then for the property itself.

  // CHECK: [[BASE:%.*]] = load [[INT]], [[INT]]* getelementptr inbounds ({ [[INT]], i32, i32 }, { [[INT]], i32, i32 }* @"$s18fixed_layout_class20GenericOutsideParentCMo", i32 0, i32 0)

  // CHECK: [[METADATA_ADDR:%.*]] = getelementptr inbounds %T18fixed_layout_class20GenericOutsideParentC, %T18fixed_layout_class20GenericOutsideParentC* %1, i32 0, i32 0, i32 0
  // CHECK: [[METADATA:%.*]] = load %swift.type*, %swift.type** [[METADATA_ADDR]]

  // CHECK: [[BASE:%.*]] = load [[INT]], [[INT]]* getelementptr inbounds ({ [[INT]], i32, i32 }, { [[INT]], i32, i32 }* @"$s18fixed_layout_class20GenericOutsideParentCMo", i32 0, i32 0)
  // CHECK: [[FIELD_OFFSET_OFFSET:%.*]] = add [[INT]] [[BASE]], {{4|8}}

  // CHECK: [[METADATA_ADDR:%.*]] = bitcast %swift.type* [[METADATA]] to i8*
  // CHECK: [[FIELD_OFFSET_ADDR:%.*]] = getelementptr inbounds i8, i8* [[METADATA_ADDR]], [[INT]] [[FIELD_OFFSET_OFFSET]]
  // CHECK: [[FIELD_OFFSET_PTR:%.*]] = bitcast i8* [[FIELD_OFFSET_ADDR]] to [[INT]]*
  // CHECK: [[FIELD_OFFSET:%.*]] = load [[INT]], [[INT]]* [[FIELD_OFFSET_PTR]]
  return o.property

  // CHECK: ret void
}

// CHECK-LABEL: define{{( dllexport)?}}{{( protected)?}} swiftcc void @"$s16class_resilience27useGenericRootClassPropertyyy013fixed_layout_A00D13OutsideParentCySiGF"(%T18fixed_layout_class20GenericOutsideParentCySiG*)
public func useGenericRootClassProperty(_ o: GenericOutsideParent<Int>) {
  // CHECK: getelementptr inbounds %T18fixed_layout_class20GenericOutsideParentCySiG, %T18fixed_layout_class20GenericOutsideParentCySiG* %0, i32 0, i32 1
  _ = o.property

  // CHECK: ret void
}

// CHECK-LABEL: define{{( dllexport)?}}{{( protected)?}} swiftcc void @"$s16class_resilience26useGenericSubclassPropertyyy013fixed_layout_A00D12OutsideChildCyxGlF"(%T18fixed_layout_class19GenericOutsideChildC*)
public func useGenericSubclassProperty<A>(_ o: GenericOutsideChild<A>) {
  // -- we load the base offset twice, first to get the generic parameter out and
  // then for the property itself.

  // CHECK: [[BASE:%.*]] = load [[INT]], [[INT]]* getelementptr inbounds ({ [[INT]], i32, i32 }, { [[INT]], i32, i32 }* @"$s18fixed_layout_class19GenericOutsideChildCMo", i32 0, i32 0)

  // CHECK: [[UPCAST:%.*]] = bitcast %T18fixed_layout_class19GenericOutsideChildC* %0 to %T18fixed_layout_class20GenericOutsideParentC*
  // CHECK: [[METADATA_ADDR:%.*]] = getelementptr inbounds %T18fixed_layout_class20GenericOutsideParentC, %T18fixed_layout_class20GenericOutsideParentC* [[UPCAST]], i32 0, i32 0, i32 0
  // CHECK: [[METADATA:%.*]] = load %swift.type*, %swift.type** [[METADATA_ADDR]]

  // CHECK: [[BASE:%.*]] = load [[INT]], [[INT]]* getelementptr inbounds ({ [[INT]], i32, i32 }, { [[INT]], i32, i32 }* @"$s18fixed_layout_class20GenericOutsideParentCMo", i32 0, i32 0)
  // CHECK: [[FIELD_OFFSET_OFFSET:%.*]] = add [[INT]] [[BASE]], {{4|8}}

  // CHECK: [[METADATA_ADDR:%.*]] = bitcast %swift.type* [[METADATA]] to i8*
  // CHECK: [[FIELD_OFFSET_ADDR:%.*]] = getelementptr inbounds i8, i8* [[METADATA_ADDR]], [[INT]] [[FIELD_OFFSET_OFFSET]]
  // CHECK: [[FIELD_OFFSET_PTR:%.*]] = bitcast i8* [[FIELD_OFFSET_ADDR]] to [[INT]]*
  // CHECK: [[FIELD_OFFSET:%.*]] = load [[INT]], [[INT]]* [[FIELD_OFFSET_PTR]]

  // Note that the The presence of named let variables here creates a debug_info
  // instruction and prevents some dead-code elimination in Onone. Otherwise
  // some of the instructions checked above are cleaned up by dead-code
  // elimination.
  let x = o.property

  // CHECK: [[METADATA_ADDR:%.*]] = getelementptr inbounds %T18fixed_layout_class19GenericOutsideChildC, %T18fixed_layout_class19GenericOutsideChildC* %0, i32 0, i32 0, i32 0
  // CHECK: [[METADATA:%.*]] = load %swift.type*, %swift.type** [[METADATA_ADDR]]

  // CHECK: [[BASE:%.*]] = load [[INT]], [[INT]]* getelementptr inbounds ({ [[INT]], i32, i32 }, { [[INT]], i32, i32 }* @"$s18fixed_layout_class19GenericOutsideChildCMo", i32 0, i32 0)
  // CHECK: [[FIELD_OFFSET_OFFSET:%.*]] = add [[INT]] [[BASE]], {{4|8}}

  // CHECK: [[METADATA_ADDR:%.*]] = bitcast %swift.type* [[METADATA]] to i8*
  // CHECK: [[FIELD_OFFSET_ADDR:%.*]] = getelementptr inbounds i8, i8* [[METADATA_ADDR]], [[INT]] [[FIELD_OFFSET_OFFSET]]
  // CHECK: [[FIELD_OFFSET_PTR:%.*]] = bitcast i8* [[FIELD_OFFSET_ADDR]] to [[INT]]*
  // CHECK: [[FIELD_OFFSET:%.*]] = load [[INT]], [[INT]]* [[FIELD_OFFSET_PTR]]
  let y = o.childProperty

  // CHECK: ret void
}

// CHECK-LABEL: define{{( dllexport)?}}{{( protected)?}} swiftcc void @"$s16class_resilience26useGenericSubclassPropertyyy013fixed_layout_A00D12OutsideChildCySiGF"(%T18fixed_layout_class19GenericOutsideChildCySiG*)
public func useGenericSubclassProperty(_ o: GenericOutsideChild<Int>) {
  // CHECK: [[UPCAST:%.*]] = bitcast %T18fixed_layout_class19GenericOutsideChildCySiG* %0 to %T18fixed_layout_class20GenericOutsideParentCySiG*
  // CHECK: getelementptr inbounds %T18fixed_layout_class20GenericOutsideParentCySiG, %T18fixed_layout_class20GenericOutsideParentCySiG* [[UPCAST]], i32 0, i32 1
  _ = o.property

  // CHECK: getelementptr inbounds %T18fixed_layout_class19GenericOutsideChildCySiG, %T18fixed_layout_class19GenericOutsideChildCySiG* %0, i32 0, i32 2
  _ = o.childProperty

  // CHECK: ret void
}

// CHECK-LABEL: define{{( dllexport)?}}{{( protected)?}} swiftcc void @"$s16class_resilience17callVirtualMethodyy013fixed_layout_A013OutsideParentCF"(%T18fixed_layout_class13OutsideParentC*)
public func callVirtualMethod(_ o: OutsideParent) {
  // Note: virtual method calls still use dispatch thunks

  // CHECK: call swiftcc void @"$s18fixed_layout_class13OutsideParentC6methodyyFTj"
  _ = o.method()

  // CHECK: ret void
}

@_fixed_layout open class MyChildOfOutsideParent : OutsideParent {
  public func newMethod() {}
}

// Make sure we emit the dispatch thunk:

// CHECK-LABEL: define{{( dllexport)?}}{{( protected)?}} swiftcc void @"$s16class_resilience22MyChildOfOutsideParentC9newMethodyyFTj"(%T16class_resilience22MyChildOfOutsideParentC* swiftself)
