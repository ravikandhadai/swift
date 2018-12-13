# Improving and Optimizing Unified Logging APIs for Swift

## Summary

We propose to change the Swift APIs for the Apple's unified logging system (namely, `os_log` and `os_signpost`)
to accept string interpolations instead of the printf-style format string and varargs combination that they currently accept.
This means Swift users can pass in string interpolations to the logging functions e.g. like `osLog("Fatal Error \(errno)")`.
(We use camelCase in this proposal for the logging functions.)
We propose an implementation of the APIs, based on [custom string interpolation](https://github.com/apple/swift-evolution/blob/master/proposals/0228-fix-expressiblebystringinterpolation.md),
that is meant to be a part of the Swift standard library.
The implementation is designed to be customizable so that library authors and application developers can extend these
APIs to provide succinct interfaces to log their custom types.
We propose to implement an optimization pass for the new logging APIs in the Swift compiler
using [compile-time interpretation for Swift](https://forums.swift.org/t/compile-time-constant-expressions-for-swift/12879).
The proposed optimizations can completely eliminate the overheads due to string interpolation abstractions
and can achieve a performance that is comparable to the C logging APIs.

## Brief Overview of the Unified Logging System

Apple operating systems provide a fast and efficient logging system, referred to as unified logging system,
that can be accessed by programs through a set of APIs such as `os_log`, `os_signpost` etc.
These APIs can be used to log as well as trace/profile programs with little performance overhead.
Besides being fast, they also offer several other benefits. Most importantly,
they compress and archive log messages for offline querying,
allow formating the data using `printf`-style format specifiers,
and provide ways to control privacy of the log data.

For example, the following call, shown in C syntax, records a log message in which the floating-point
data `delay` is rendered with a precision of 2 decimal places in the log.

    os_log(OS_LOG_DEFAULT, "Network delay: %.2lf ms", delay)

This formatting/pretty-printing of the log data is obtained at almost zero cost,
because the `os_log` function constructs the actual log message asynchronously in a background process.
It packs all the varargs into a buffer of raw bytes with some headers, and passes the format string and the packed buffer to a log daemon,
which constructs the log message in the background and archives it.

Besides the standard C format specifiers, the logging system provides a few
additional formatting options for pretty-printing values that represent certain predefined
types such as system time, IP addresses etc.
They are specified using a keyword within curly braces after the `%` symbol.
For instance, `%{time_t}d`  indicates that the logged integer should be treated as the seconds
elapsed since the system epoch. This will render the integer as time when the logs are displayed.

The same syntax is also used to indicate that a log value is _private_, which informs
to the logging system that it must _not_ store the actual value in the logs, when the
the logging system is configured to be in privacy mode.
For example, `%{private}d` indicates that the logged integer should not be stored in logs
in privacy mode.

The WWDC presentation [Unified logging and activity tracing](https://developer.apple.com/videos/play/wwdc2016/721/)
provides more details on how the logging system works.

## Problems with Existing Swift APIs

The unified logging APIs are available in Swift, but currently they have a C-like syntax: they take a format string and varargs.
They are also less performant compared to their C counterparts because Swift does not have optimizations for these APIs that the C compiler _Clang_ has.

## Features of this Proposal

1. *Enables Swift users to log string interpolations instead of C-style format strings and varargs.*
    Importantly, the string-interpolation-based APIs expose all formatting options that are
    supported by the logging system such as tagging data as _private_, specifying the
    precision of floating-point number etc., through the extended string interpolation syntax.

2. *Presents a standard library implementation of the logging APIs based on custom string interpolation.*
    The implementation will interface with the native C ABIs provided by the logging system,
    which expects a  `printf`-style format string and a contiguous sequence of bytes
    containing the varargs.
    The implementation constructs the required inputs to the C ABIs from the    string interpolation that is passed in, thereby relieving the users of the loggin APIs
    from these low-level considerations.    All of this construction logic will be implemented in Swift in the standard library.


3. *Enables Swift users to extend the logging APIs to log their custom user-defined types.*
     Swift users can define extensions to the new logging APIs. The extensions may accept
     values of user-defined types and may define how to render the type
     and define which fields are private etc.
     Having created an extension for a user-defined type, instances of the type can be interpolated
     in the log calls: `osLog("User-defined type: \(instance)")`.

4. *Makes Swift logging API invocations  _fast_ and matches the performance of log calls in C
    programs compiled with Clang.*
    This is achieved by an optimization pass that uses [compile-time interpretation](https://forums.swift.org/t/compile-time-constant-expressions-for-swift/12879).
    Using such a general-purpose framework reduces the need for hardcoded knowledge
    within in the optimization passes.
    This makes it easier for the implementation to evolve more freely,
    and allows the extensions to the logging APIs to benefit from the optimizations.
    However, the use of the interpreter means that implementing the logging APIs requires
    more sophistication. It requires separating out the implementation into
    compile-time interpretable parts and runtime parts.
    Nonetheless, this cognitive overhead is hidden from the users and is a concern only for the implementers/extenders of the logging API.

# Proposal

## Syntax

Currently, the logging APIs in Swift are global functions and have a C-like syntax, i.e, they
are written in snake_case, and take a format string and varargs.
We propose to use camelCase for the function names (i.e, `osLog` and `osSignpost`),
and change their signature to accept a string interpolation in place of the format string
and varargs.
We assume that every other parameter to these functions is same as what it is currently
([current implementation](https://github.com/apple/swift/blob/master/stdlib/public/SDK/os/)).
The proposal requires the logging APIs to accept string interpolations,
but is oblivious to other parameters of the logging APIs.
For the optimizations proposed here to be fully effective, the log calls should be direct
calls and not dynamic dispatches. Relaxing this requirement is possible,
if it is needed in the future.

## The New User Interface for Logging

The following table shows some examples of `osLog` calls
with formatting options in the proposed syntax.
An equivalent call in the existing `os_log` syntax is also shown for comparison.
The new [custom string interpolation](https://github.com/apple/swift-evolution/blob/master/proposals/0228-fix-expressiblebystringinterpolation.md)feature allows interpolated terms to be sequences of labeled expressions (similar to parameters of functions).
We propose to use these features to specify formatting options with the
interpolated expressions.

|  Proposed Syntax | Existing syntax  |
|---|---|
|  `osLog("Network delay: \(delay) ms")`  |  `os_log("Network delay: %lf ms", delay)` |
|  `osLog("Network delay: \(delay, .decimal(2)) ms")`  |  `os_log("Network delay: %.2lf ms", delay)` |
|  `osLog("Network delay: \(private: delay, .decimal(2)) ms")`  |  `os_log("Network delay: %{private}.2lf ms", delay)` |
|  `osLog("Login time: \(private: currentTime, .time) ms")`  |  `os_log("Login time: %{private, time_t}d ms", currentTime)` |
|  `osLog("My user-defined type: \(myinstance, .myoption(myparam))")`  |  - |

The formatting options are cases of `enum`s. For standard types the enums could be defined in the standard library.(For user-defined types, it can be defined by the user creating the logging extension for the type.)
E.g., the enum sketched below lists (some of) the formatting options for an `Int` type supported by `osLog`.

```swift
public enum IntLogFormat {
  case hex
  case octal
  case time
  case ipAddress
  ...
}
```

By defining the string interpolation functions appropriately (described shortly),
the type checker  can ensure that every interpolated value is paired with a
formatting option that is permitted for the type.
This provides more type safety than the existing APIs, where formatting options such as `time_t` are specified as strings.
We propose to specify the `private` tags as a label, since it is a binary value.
The interpolated values can also be user-defined types and can also accept user-defined
options much like the natively defined types, provided the logging APIs have been
appropriately extended to handle the user-defined types
(see section "Implementing Logging for User Defined Types").
This feature does not have an equivalent in the existing `os_log` APIs.

**Formatting options are required to be static i.e, compile-time known values**

A important restriction on the API is that the formatting options are required to be compile-time constants.
If the enum cases accept parameters (e.g. like `.decimal(2)`), those parameters should also be
compile-time constants. This is necessary as the logging system requires the format string to be a static string
in the compiled binary.
(This is essential for performance as well as for enforcing the privacy policy.)
We propose to generate a compile-time error if this restriction is violated.

## Logging API Implementation Model

This section describes the programming model for the implementers of the logging APIs.
The logging APIs `osLog` and `osSignpost` will be implemented in the standard library.
However, the implementation can be extended by Swift users
(particularly,  authors of Swift libraries) to enable logging of user-defined types,
and to enable interfacing with their custom logging backends.
This section is aimed at readers interested in implementing such extensions.

There is also an important restriction on the implementation model, which is required for satisfying
the requirements of the unified logging system and also for enabling deep optimizations.
The following section "Designing String Interpolation Methods to be Compile-time Interpretable"
discusses this restriction along with some guidelines/examples for conforming to this restriction.
Understanding this restriction is important for readers interested in extending the logging APIs.

In the rest of the document we focus only on `osLog` implementation. `osSignpost` can be implemented in a very similar way.
All source code discussed in this document is available here:
[prototype](https://github.com/ravikandhadai/swift/tree/logging-writeup/Logging).

### Implementation Strategy

The following are the main steps involved in the implementation of the logging APIs:

 1. Constructing a C-style format string from the string interpolation.

 2. Constructing a byte buffer from the string interpolation.
    The byte buffer must consist of the bytes of the interpolated expressions

 2. Invokes the C ABI `_os_log_impl`  with those values.

For instance, given a call of the form `osLog("Login time: \(private: currentTime, .time) ms")`
the implementation will construct a format string of the form: `"Login time: %{private,time_t}d ms"`
and a buffer consisting of the bytes of the interpolated expression: `currentTime`.

To accomplish step 1 and parts of step 2, we use custom string interpolation of Swift.
The remaining tasks are implemented as part of the `osLog` function implementation.
We define `osLog` as a function that accepts a custom string interpolation type:
`PackedLogMsg`, which conforms to the new custom string interpolation protocol: [`ExpressibleByStringInterpolation` protocol](https://github.com/apple/swift-evolution/blob/master/proposals/0228-fix-expressiblebystringinterpolation.md).

  ```swift
  func osLog(_ packedMsg: PackedOSLogMessage) { ... }

  struct PackedLogMsg : ExpressibleByStringInterpolation { ... }
  ```

### Constructing Format String and Argument Buffer with Custom String Interpolation

The protocol `ExpressibleByStringInterpolation` requires defining an associate type
`StringInterpolation`  that has an initializer, a method
`appendLiteral(_:String)`  and one or more overloaded methods with the name: `appendInteroplation`.
On encountering a string interpolation whose type is required to be `PackedLogMsg`,
the compiler automatically creates an instance of
`PackedLogMsg.StringInterpolation` and invokes the  `appendLiteral`  and
`appendInteroplation` functions on the literal and interpolated terms respectively.
The compiler ensures that an interpolated term with a signature `(label1: Type1, ... ,labeln: Typen)`
is passed to an `appendInterpolation` overload with a compatible signature.
(The absence of a suitable overload will result in a compile-time error.)
It subsequently creates an instance of `PackedLogMsg` passing it the
`PackedLogMsg.StringInterpolation` instance constructed as described above.
For example, the sequence of calls generated by the compiler for the call
`osLog("Login time: \(private: currentTime, .time) ms")` is similar to the code shown below:

  ```swift
  var stringInterpol = PackedLogMsg.StringInterpolation(literalCapacity: 15, interpolationCount: 1)
  stringInterpol.appendLiteral("Login time: ")
  stringInterpol.appendInterpolation(private: currentTime, .time)
  stringInterpol.appendLiteral(" ms")
  var packedMsg = PackedLogMsg(stringInterpolation: stringInterpol)
  osLog(packedMsg)
  ```
We propose to implement the construction of the format string and the byte buffer
in the `appendLiteral` and `appendInterpolation`  functions of
`PackedLogMsg.StringInterpolation`.
Furthermore, the implementation shown below ensures that it is compile-time interpretable,
which is an important restriction that ensures correctness and also deep optimizations.

For example, a simplified implementation of the type `PackedLogMsg` that allows only
64-bit `Int`s to be interpolated is shown below.

<summary>

```swift
public struct PackedLogMsg : ExpressibleByStringInterpolation {

  let stringInterpol: StringInterpolation

  public struct StringInterpolation : StringInterpolationProtocol {
    var preamble: UInt8 // The first byte of the header.
    var argCount: UInt8 // No. of arguments in the buffer.
    var formatString: String
    var encodeOps: EncodeOperations

    public init(literalCapacity: Int, interpolationCount: Int) {
      formatString = ""
      preamble = 0
      argCount = 0
      encodeOps = EncodeOperations()
    }

    public mutating func appendLiteral(_ literal: String) {
      formatString += literal  // Escaping of black-slashes is elided for brevity.
    }

    public mutating func appendInterpolation(@autoclosure private number: () -> Int64, _ format: IntLogFormat) {
      preamble |= 1 // set the private argument bit in the preamble
      argCount += 1
      formatString += "%ld"
      switch format {
      case .time:
        formatString += ",time_t"
      case .darwinErrno:
        formatString += ",darwin.errno"
      default:
        break
      }
      formatString += "}ld"
      encodeOps.append(UInt8(1)) // Append a preamble for the argument.
      encodeOps.append(UInt8(8)) // Append the byte size of the argument.
      encodeOps.append(number)   // Append the argument itself.
    }
  }

  public init(stringInterpolation: StringInterpolation) {
    stringInterpol = stringInterpolation
  }
}
```
</summary>

The complete implementation of the the `PackedLogMsg` can be found here: [PackedLogMsg prototype](https://github.com/ravikandhadai/swift/blob/logging-writeup/Logging/OSLogPrototype.swift).
Note that the above implementation separately tracks the header bytes: `preamble` and
`argCount`, and constructs the format string on the fly as the interpolated terms
are seen.

However, the construction of the byte buffer uses a different strategy.
The type `EncodeOperations`  abstracts the construction of the byte buffer.
The main benefit of the abstraction is
that it enables deferring the construction of the byte buffer to a point after the
(auto-generated) calls to  `appendLiteral` and `appendInterpolation` functions.
In our current implementation, the  buffer construction happens within the `osLog`
function (described shortly) and happens only if logging is enabled.
This deferring of buffer construction is crucial for making the string interpolation functions
compile-time interpretable, which ensures correctness as well as performance of the logging APIs.
This aspect is described in more detail in the next section.

### Implementation of osLog

Using the custom string interpolation type  `PackedLogMsg`, the `osLog` function is
defined as sketched below. It accepts a parameter `packedMsg` which can be
instantiated by string interpolation, and performs the following tasks:

  1. Allocates a byte buffer of the required size. The byte size of the buffer is tracked by `packedMsg.encodeOps`
     and is exposed by the property `packedMsg.totalArgByteCount`.
  2. It copies the arguments tracked by `packedMsg.encodeOps` into the byte buffer.
     This is performed by the function `packedMsg.encode` whose implementation is elided for brevity.
  3. It invokes the C ABIs with the format string tracked by `packedMsg` and the byte buffer.

<summary>

```swift
public func osLog(_ packedMsg: PackedLogMsg) {
  // Check if logging is enabled.
  guard logger.isEnabled(...) else { return }

  // Allocate a byte buffer and a byte encoder.
  let byteCount = packedMsg.totalArgByteCount
  var bufferPtr = UnsafeMutablePointer<UInt8>.allocate(capacity: byteCount)
  var byteEnc = ByteEncoder(bufferPtr)

  // Dispatch the encode operations tracked by packedMsg.
  packedMsg.encode(&byteEnc)

  // Invoke the C logging ABIs
  _os_log_impl_wrapper(packedMsg.formatString, bufferPtr, byteCount, ...)

  // clean up all state
  bufferPtr.deallocate()
  byteEnc.destroy()
}
```
</summary>

<!---
It may be a little surprising that the `ByteEncoder` type accepts the byte buffer
as an argument from the caller. This is done just to ensure that  the allocation of the byte buffer
happens in a stack frame that is alive during the call to the C ABI. This enables
stack allocating the byte buffer.
In the above code,  the method `allocate` allocates raw bytes on the heap.
This function is used here in place of stack allocation as Swift does not currently
have a programmatic way of allocating raw bytes of variable size on the stack.
In principle, the memory allocated in this function can be on the stack as it is freed
at the end of the function.
It is possible to either introduce a programmatic way of specifying stack
allocation or design a compile-time optimization that performs heap-to-stack promotion.

The function `encode`  essentially dispatches all the closures that were created
within the string interpolation functions, using the instance of `ByteEncoder` provided as an argument. This is the place where the buffer is actually populated. Note that it is guarded by a check that the logging is enabled. The following section describes how to optimize the `osLog` function by folding in the values of the compile-time constants: `formatString`, `totalArgByteCount` and `encodeOps`. We plan to support unrolling of selected collection APIs like `forEach` and `map`  that operate on constant collections.

```swift
public struct PackedLogMsg : ExpressibleByStringInterpolation {
  public func encode(_ enc: inout ByteEncoder) {
    enc.encode(stringInterpol.preamble)
    enc.encode(stringInterpol.argCount)
    encodeOps.ops.forEach { $0(&enc) }
  }
}
```
-->

### Enabling User-defined Types to be Logged

The  `PackedLogMsg.StringInterpolation` struct can be extended in order to log user-defined types
by Swift users, especially developers of Swift libraries, outside of the standard library.
To log a user-defined type it suffices to define an overload of `appendInterpolation`
on `PackedLogMsg.StringInterpolation`  that accepts the user-defined type and
interpolates the properties of the type as desired.
The following code snippet illustrates this on a struct `Developer` that uses an enum
`Language`.

```swift
struct Developer {
  let Id: Int
  var Name: String
  var preferredLanguage: Language
}

enum Language {
  case swift
  case objectiveC
  ...
}

extension PackedLogMsg.StringInterpolation {
  mutating func appendInterpolation(_ dev: Developer) {
    let msg: PackLogMsg = "Id: \(private: dev.id) Name: \(private: dev.name) Preferred Language: \(dev.preferredLanguage)"
    appendInterpolation(msg) // Combines self with msg.stringInterpol.
  }

  mutating func appendInterpolation(_ lang: Language) {
    switch lang {
    case .swift:
      appendInterpolation("swift")
    case .objectiveC:
      appendInterpolation("objectiveC")
    ...
    }
  }
}
```
With this implementation, one can directly log instances of `Developer` e.g.,
`osLog("Developer \(dev) made a commit")`.
The extensions reuse the string interpolation methods on types already
supported by `PackedLogMsg`.
We predefine an  `appendInterpolation` overload on
`PackLogMsg.StringInterpolation`  that takes an instance of `PackedMsg`
and concatenates the internal state of `self`
(which is of type `PackedMsg.StringInterpolation`) with the provided instance.
This enables passing an instance of `PackedMsg` constructed via string interpolation
to `appendInterpolation`  as is done by the call `appendInterpolation(msg)`.
(It also enables interpolating an instance of `packedMsg` into another string interpolation.)
The implementation of this overload is straightforward and is available [here](https://github.com/ravikandhadai/swift/blob/logging-writeup/Logging/OSLogPrototype.swift).

*Remark:*
In fact, the logging APIs are extensible enough to support any new types
that are natively added to the logging system.
Say the logging system introduces a new format specifier and an encoding format for a new aggregate type.
To extend the APIs to this new type, it suffices to overload `appendInterpolation` to the new type.
The overload will use the format specifier for the new type and will append the
argument to the `encodeOps`.
The types `EncodeOperations` (described shortly) and `ByteEncoder` would have to
be extended with the logic for marshalling the custom type.
For brevity, we elide further details of creating such extensions,
and refer interested readers to an example available [here](https://github.com/ravikandhadai/swift/blob/logging-writeup/Logging/OSLogSpecifierExt.swift).

## Optimizations

The overall optimization workflow is summarized below.
We propose to create a dedicated optimization pass that drives the following steps.
The goal of the optimization is to match the performance of the code generated by Clang for the `os_log` calls in C.

1. The [compile-time interpreter](https://forums.swift.org/t/compile-time-constant-expressions-for-swift/12879)   is run on the string interpolation calls to infer the compile-time value of the struct `packedMsg` and its properties.

2. The implementation of `osLog` is optimized by folding in the uses of the properties of
   `packedMsg` by their constant values. This involves replacing accesses to
  `formatString` property with the inferred string literal, replacing `totalArgsByteCount`
  with the inferred buffer size, and replacing all indirect invocation of the closures tracked by
  `packedMsg.encodeOps` with their inferred targets.

3. All invocations of autoclosures that were created from the arguments to the string
  interpolation methods are converted to direct calls based on the targets inferred by the
  compile-time interpreter.

4. Existing function inlining pass is used to inline all direct function calls to `ByteEncoder`
  methods and closure targets.

5. Existing Dead-code and Dead-object elimination passes are used to eliminate the `packedMsg` instance,
which is now unused as its uses have been folded, and also to eliminate the string interpolation calls.

6. The heap-allocation of the byte buffer in the `osLog` function is promoted to stack,
   based on the fact that it is deallocated at the end of the function.

If any of the above steps is not possible, we propose to present an error or warning to the user depending on whether
it results in a violation of the contract of the logging system or a performance degradation. E.g. if the format string cannot be inferred
as a compile-time constant, it's a contract violation and is therefore an error.
However, if step 6 is not possible, it is a loss of performance but is not a hard error.

### Features Needed from Compile-Time Interpreter

The following are a brief list of language features that the interpreter would have to support in order
for it to be applicable to the implementation sketched above.
  * Basic language constructs such as: integers, floats, structures, function calls and generics.
  * Array literal construction and array operations such as array indexing, array append.
  * String literal construction and string operations such as `+`, `+=` etc.
  * Closure creation, closure assignment and invocation.

The interpreter in the current state has representations for `String` and `Array`, and supports
a limited set of operations on them. Extending them to cover a few more operations,such as `+=`, array indexing and concatenation operations such as `append`, is quite straightforward.

We propose to create a new representation within the interpreter for closures,
which consists of the target of the closure (which is a SIL function) and variables
that are captured.
The closures would be treated as another kind of  _symbolic_ constant within the interpreter.
The closures that are not invoked during interpretation are allowed to use arbitrary Swift code. 
The targets of the closures that are invoked during interpretation must be interpretable
and all the captures of the closure must be constants tracked by the interpreter.

**Benefits of Supporting Closures in the Interpreter**

We believe adding support for closures generalizes the interpreter to a larger code fragment
and enables other applications to benefit from compile-time interpretation.
It provides a way to run the interpreter on code fragments that have interpretable
and non-interpretable parts interleaved in a way that is not easily separable.
(This was the case with the string interpolation methods, where the format string
was a compile-time constant whereas the interpolated expressions were not.)
The non-interpretable parts could be hidden beneath a closure that captures the necessary
state, and could be invoked after the interpretable parts. This would enable the interpreter to
run on the interpretable parts thereby completely capturing their behavior at compile time.
The interpreter will also track the targets and captured values of the closures that wrap the
non-interpretable parts, thus enabling the invocations of the closures to be replaced by
direct calls.

### Using Inlining to Enable Optimizations and Fix Out-of-Scope References

We propose to annotate functions like `osLog`, `encode`  etc. as  `@inline(__always)`.
This enables (a) optimizations such as dead code elimination to be more effective, and (b)
helps ensure that the generated code correctly refers to variables that are in scope.

Out of scope variable references could be generated,
if the closures whose invocations are replaced with direct calls are created in a scope that is different from the scope where the invocation happens.
We plan to ensure through inlining annotations that the creation and the invocations happen in the same scope (e.g. in the caller of `osLog`).
Inlining is also required for the correct functioning of certain C logging ABIs that capture the return address of the top most function on the stack.

### Making Optimizations Reusable

We propose to use the code pattern that corresponds to the custom string interpolation of `PackedLogMsg`
as the key to kick start the optimizations specific to the logging APIs.
This enables optimizations to be applicable even outside of the unified logging APIs.
In principle, any other function (or logging API) that accepts an instance of `PackedLogMsg` and
uses its properties can benefit from the optimizations.

## Designing String Interpolation Methods to be Compile-time Interpretable

This section discusses the important restriction that the implementers of the logging APIs must adhere to:
the string interpolation methods are required to be [compile-time interpretable](https://forums.swift.org/t/compile-time-constant-expressions-for-swift/12879).
Any extensions to the string interpolation methods such as the one described above are also required to be compile-time interpretable.
In this section, we clarify what this means and explain how this is achieved in the implementations shown above.

An important requirement of the logging system is that the format string passed to the C ABIs
should be a static string i.e, a literal in the compiled binary. (This is primarily a privacy requirement,
as static strings cannot be constructed out of potential sensitive, dynamic values.
However, this also improves efficiency as static strings can be extracted from the binary and need not be copied/stored.)
Another requirement is that the size of the byte buffer should be within a predefined limit.

These restrictions imply that the format string and the buffer size must be inferred at compile time.
The passes that currently exist in the Swift compiler alone aren't sufficient to infer these from the implementation shown above.
In fact, the level of analysis required to infer these constants is comparable to _running_
the code in the string interpolation methods.
We therefore propose to use the
[compile-time interpretation](https://forums.swift.org/t/compile-time-constant-expressions-for-swift/12879) feature to
infer these compile-time constants.

As such, computing the format string, despite appearing complicated, requires only compile-time
knowledge, namely (a) the literal segments of the string interpolation, and
(b) the formatting options which are required to be enum literals.
However, the main challenge is that the construction of the format string is interleaved with pieces of code that are
not compile-time interpretable, especially the `appendInterpolation` methods that manipulate interpolated expressions.

To enable using the interpreter, the implementation of the string interpolation adheres to certain _design principles_,
which are described below.
These principles could serve as guidelines for extensions to the interpolation functions as well.

### Hiding Buffer Construction logic from String Interpolation Methods using Closures

<!-- The byte buffer cannot be computed at compile-time as the interpolated
values need not be compile-time constants. However, the interpolated expressions are passed
to `appendInterpolation` functions which also constructs parts of the format string.
 Moreover, making it as much optimal as possible, using unsafe operations and raw byte allocations,
is worth the effort as it needs to happen at runtime.
The implementation uses the following design strategy to enable interpretation of the string interpolation
functions despite this fact. -->

Recall that the buffer construction in the string interpolation methods are
abstracted by the type `EncodeOperations`.
`EncodeOperations` is a struct that essentially tracks an array of closures.
The closures capture the argument to be logged,
and can later be invoked with an instance of `ByteEncoder`
which actually copies the captured argument into a buffer maintained by the
given `ByteEncoder` instance.
The struct `EncodeOperations` also tracks the total number
of bytes that would be needed to encode the arguments.
This is exposed through the property `PackedLogMsg.totalArgByteCount` and is
used to allocate a buffer of the right size by the `osLog` implemenation shown earlier.

The definition of `EncodeOperations` is shown below for `Int` types.
(It is almost identical for other types.)
The complete implementations of `EncodeOperations` and `ByteEncoder` are available
[here](https://github.com/ravikandhadai/swift/blob/logging-writeup/Logging/ByteEncoder.swift).

<summary>

```swift
public struct EncodeOperations {
  // An array of operations that encode an argument into a byte buffer.
  public var ops: [(inout ByteEncoder) -> ()]
  public var totalByteCount: Int

  public init() {
    ops = []
    totalByteCount = 0
  }

  public mutating func append(_ x: Int) {
    let encodeOp = { (enc: inout ByteEncoder) in enc.encode(x) }
    ops.append(encodeOp)
    totalByteCount += 8 // # of bytes for encoding `Int`
  }
}
```
</summary>

In essence, `EncodeOperations` does not perform the copy but only creates closures
that will perform the copy later.
The string interpolation methods are not polluted with the
details of the buffer construction which keeps them simple and
compile-time interpretable.
Besides enabling the code to be compile-time
interpretable, this also enables constructing the buffer optimally after the string interpolation methods.
The construction can use the inferred buffer size, and can also stack allocate the bytes.

### Making Parameters of `appendInterpolation` Autoclosures

The interpolated values (such as `delay`) are not necessarily a compile-time constant.
However, their actual values are not looked up by the string interpolation functions.
We therefore make the parameters to the `appendInterpolation` functions
`@autoclosure`s so that values like `delay` are actually passed in as a closure.

The autoclosures also have an advantage that if the interpolated expression have some
computation in them (e.g. like `osLog("Result of \(expensiveFun())"`)  then the
computation happens only when the autoclosures are invoked, which are guarded
by a check that passes only if logging is enabled.

## ABI Stability and Backward Compatibility

### Compatibility with Existing Applications

This proposal does not deprecate or remove any existing ABI. Therefore, it does not affect compatibility
with source code or binaries written using existing `os_log/os_signpost` functions.
However, when the existing applications are compiled with a newer Swift compiler (and newer standard library),
certain optimizations described here could be reused. This is outside the scope of this proposal.

### Interoperability with Older ABI Stable Swift

To enable backward deploying binaries that are compiled with the new logging APIs to an older, ABI-stable Swift, the ABI of the code compiled with the proposed logging APIs should not rely on any function or type that is not supported in Swift 5. This means that in the above implementation the types `PackedLogMsg`, `EncodeOperations` and `ByteEncoder` (and their properties and methods) should not be a part of the compiled code. The optimizations proposed in the document eliminate these types by a combination of targeted compile-time interpretation, constant folding and inlining, which is performed by a dedicated optimization pass.

In order to guarantee this property at compile time, we propose to annotate these types as `@_compileTime` which informs the compiler to generate an error if such annotated types are present in the compiled code. This provides a way to achieve backward deployment even when these structures are used by users in their custom extensions. Furthermore, programmers can also opt out of this restriction by opting out of backward deployment.

<!--- ### Resilience: Evolving Logging APIs without breaking Backward Compatibility

The proposal (the APIs together with the optimizations) is aimed at ensuring that
the struct `PackedLogMsg` is not a part of the ABI of the compiled program.
They are meant to be compile-time entities that are removed after optimization.
(This could be achieved by generating a compile-time error if the `packedLogMsg` remains in the optimized code.)

The implementation of `osLog` and `osSignpost` can also be eliminated from the ABI by
mandatorily inlining them in the optimization phases. This means that no implementation would

This means that the layout of the structure and the implementations of the methods in the structure
can evolve.
We propose to incorporate the new logging APIs and the optimizations described in this document
together in the same version of Swift.
-->
