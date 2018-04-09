#import <Foundation.h>
extern int ANTGlobalValue;

@interface NewType
@end
@interface OldType
@end

@protocol TypeWithMethod
  -(void) minusPrint;
  +(void) plusPrint;
  -(int) getPropertyA;
@end

@protocol ObjcProt
  -(void) ProtMemberFunc;
  -(void) ProtMemberFunc2;
  -(void) ProtMemberFunc3;
@end

@interface AnimalStatusDescriptor
- (nonnull AnimalStatusDescriptor *)animalStatusDescriptorByAddingAttributes:(nonnull NSDictionary<NSString*, id> *)attributes;
- (nonnull AnimalStatusDescriptor *)animalStatusDescriptorByAddingOptionalAttributes:(nullable NSDictionary<NSString*, id> *)attributes;
@end
