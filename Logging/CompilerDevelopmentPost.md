#  Improving OS Log using Custom String Interpolation and Compile-time Interpretation

Hi all,

I, along with @Ben_Cohen, @moiseev and @Devin_Coughlin, have been working on improving
Swift APIs for Apple's [unified logging system](https://developer.apple.com/videos/play/wwdc2016/721/),
based on some recent exciting developments to the Swift language and compiler.
This post presents an overview of our plan for creating a customizable interface for Apple's logging system.
Our proposal uses [custom string interpolation](https://github.com/apple/swift-evolution/blob/master/proposals/0228-fix-expressiblebystringinterpolation.md),
which is a recently-improved language feature,
and [compile-time interpretation](https://forums.swift.org/t/compile-time-constant-expressions-for-swift/12879),
which is a compiler technology that is in the process of getting upstreamed.
We are very excited to discuss our proposal and welcome your feedback on this.
A more detailed version of this proposal is available [here](https://github.com/ravikandhadai/swift/blob/logging-writeup/Logging/ImprovingOSLogForSwift.md).

While this proposal focuses only on Apple's logging APIs, the ideas presented here could be applicable more generally.
In particular, we think the following aspects of the proposal are generalizable:

  * We show how the new string interpolation features can be used to expose all
functionalities provided by printf-style format string and varargs.
  * We present a way to write Swift code to deconstruct a string interpolation into a format string and an argument sequence
using custom string interpolation.
  * We present a compiler optimization based on compile-time interpretation that can recover compile-time
constants from complex Swift code.

We therefore see this proposal as being relevant to the following _potential future directions_:

* Development of a general-purpose, platform-agnostic logging API for Swift.
* Development of custom logging libraries for interfacing with other logging systems.
* Migrating other C-style Swift APIs such as localization APIs to string interpolation.
* Development of the compile-time interpretation technology and applying it to optimizations.

## Highlights of this Proposal

* **Changes `os_log` API to accept string interpolations instead of the printf-style format string and varargs combination that it currently accepts.**
  The new API (written in camelCase) will expose all the formatting options that are supported by the logging system  such as tagging data as _private_, specifying the precision of floating-point numbers etc.

* **Presents a standard library implementation of the osLog API based on [custom string interpolation](https://github.com/apple/swift-evolution/blob/master/proposals/0228-fix-expressiblebystringinterpolation.md).**
    The implementation will interface with the native C ABIs provided by the logging system,
    which expects a  `printf`-style format string and a contiguous sequence of bytes
    containing the varargs.
    The implementation constructs the required inputs to the C ABIs from the    string interpolation that is passed in, thereby relieving the users of `osLog` from these low-level considerations.    All of this construction logic will be implemented in Swift in the standard library.

* **Enables Swift users to extend the osLog API to log their custom types.**
     Swift users can define extensions to the new logging API. The extensions may accept
     values of user-defined types and may define how to render the type
     and define which fields are private etc.
     Instances of a user-defined type can be interpolated in the log calls like any other value: `osLog("User-defined type: \(instance)")`.

* **Makes osLog invocations _fast_ and aims to match the performance of os_log calls in C
    programs compiled with Clang.**
    This is achieved by an optimization pass that uses [compile-time interpretation](https://forums.swift.org/t/compile-time-constant-expressions-for-swift/12879).
    Using such a general-purpose framework reduces the need for hardcoded knowledge
    within in the optimization passes.
    This makes it easier for the implementation to evolve more freely,
    and allows the extensions to the logging API to benefit from the optimizations.
    However, the use of the interpreter means that implementing the logging API requires
    more sophistication. It requires separating out the implementation into
    a compile-time interpretable part and a runtime part.
    Nonetheless, this cognitive overhead is hidden from the users and is a concern only for the implementers/extenders of the logging API.

# Proposal

## The New User Interface for Logging

The following table shows some examples of `osLog` calls
with formatting options in the proposed syntax.
An equivalent call in the existing `os_log` syntax is also shown for comparison.
The new [custom string interpolation](https://github.com/apple/swift-evolution/blob/master/proposals/0228-fix-expressiblebystringinterpolation.md)
feature allows interpolated terms to be sequences of labeled expressions (similar to parameters of functions).
We propose to use these features to specify formatting options with the
interpolated expressions.

|  Proposed Syntax | Existing syntax  |
|---|---|
|  `osLog("Network delay: \(delay) ms")`  |  `os_log("Network delay: %lf ms", delay)` |
|  `osLog("Network delay: \(delay, .decimal(2)) ms")`  |  `os_log("Network delay: %.2lf ms", delay)` |
|  `osLog("Network delay: \(private: delay, .decimal(2)) ms")`  |  `os_log("Network delay: %{private}.2lf ms", delay)` |
|  `osLog("Login time: \(private: currentTime, .time) ms")`  |  `os_log("Login time: %{private, time_t}d ms", currentTime)` |
|  `osLog("My user-defined type: \(myinstance, .myoption(myparam))")`  |  - |

The formatting options are cases of `enum`s. For standard types the enums could be defined in the standard library.
(For user-defined types, it can be defined by the user creating the logging extension for the type.)
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

The formatting options that can be paired with a type is fixed by the
definition of the interpolation functions for the type.
This provides more type safety than the existing APIs, where formatting options such as `time_t` are specified as strings.
The interpolated values can also be user-defined types and can also accept user-defined
options much like the natively defined types, provided the logging API has been
appropriately extended to handle the user-defined type.

**Formatting options are required to be static i.e, compile-time known values**

An important restriction on the API is that the formatting options are required to be compile-time constants.
If the enum cases accept parameters (e.g. like `.decimal(2)`), those parameters should also be
compile-time constants. This is necessary as the logging system requires the format string to be a static string
in the compiled binary.
(This is essential for performance as well as for enforcing a privacy policy.)
We propose to generate a compiler error if this restriction is violated.

## Logging API Implementation Model

We propose to implement `osLog` in the standard library but make it extensible by Swift users.
The details described here would be relevant for users interested in creating such extensions.

As a running example, consider a call: `osLog("Login time: \(private: currentTime, .time)")`.
The implementation described below will construct a format string of the form: `"Login time: %{private,time_t}d"`
and a buffer consisting of the bytes of the interpolated expression: `currentTime`, and will invoke the C logging ABIs with these values.

We define `osLog` as a function that accepts a custom string interpolation type:
`PackedLogMsg`, which conforms to the string interpolation protocol: [`ExpressibleByStringInterpolation` protocol](https://github.com/apple/swift-evolution/blob/master/proposals/0228-fix-expressiblebystringinterpolation.md).
When a string interpolation is passed to the `osLog` function,
the compiler would generate code to create an instance of `PackedLogMsg`
and invoke a sequence of methods on it passing the literal and interpolated
parts of the string interpolation, in the same order as they appear in the source.
For example, the sequence of calls generated by the compiler for the call
`osLog("Login time: \(private: currentTime, .time)")` is similar to the code shown below:

  ```swift
  var stringInterpol = PackedLogMsg.StringInterpolation(literalCapacity: 12, interpolationCount: 1)
  stringInterpol.appendLiteral("Login time: ")
  stringInterpol.appendInterpolation(private: currentTime, .time)
  var packedMsg = PackedLogMsg(stringInterpolation: stringInterpol)
  osLog(packedMsg)
  ```
The construction of the format string and byte buffer happens in the string interpolation protocol
methods of `PackedLogMsg`. The following code snippet sketches the implementation of these functions.
(Note that the code snippet has some missing pieces which are elided for brevity.
A complete implementation of `PackedLogMsg` and `osLog` can be found here:
[PackedLogMsg prototype](https://github.com/ravikandhadai/swift/blob/logging-writeup/Logging/OSLogPrototype.swift.)
<!--- The prototype implementation is also [compile-time interpretable](https://forums.swift.org/t/compile-time-constant-expressions-for-swift/12879),
which enables interpreting the code at compile-time to infer the compile-time constants such as format string.) -->

<summary>

```swift

public func osLog(_ packedMsg: PackedLogMsg) {
  // Check if logging is enabled.
  guard logger.isEnabled(...) else { return }

  // Allocate a raw byte buffer and a byte encoder.
  let byteCount = packedMsg.totalArgByteCount
  var bufferPtr = UnsafeMutablePointer<UInt8>.allocate(capacity: byteCount)
  var byteEnc = ByteEncoder(bufferPtr)

  // Move the data tracked by 'packedMsg' into the byte buffer.
  packedMsg.encode(&byteEnc)

  // Invoke the C logging ABI.
  _os_log_impl_wrapper(packedMsg.formatString, bufferPtr, byteCount, ...)

  // Clean up all state.
  bufferPtr.deallocate()
}

public struct PackedLogMsg : ExpressibleByStringInterpolation {

  let stringInterpol: StringInterpolation

  public struct StringInterpolation : StringInterpolationProtocol {
    var preamble: UInt8 // The first byte of the header.
    var argCount: UInt8 // Number of arguments in the buffer.
    var formatString: String
    var encodeOps: EncodeOperations

    public mutating func appendLiteral(_ literal: String) {
      formatString += literal  // Escaping of black-slashes is elided for brevity.
    }

    public mutating func appendInterpolation(@autoclosure private number: () -> Int64, _ format: IntLogFormat) {
      preamble |= 1 // Set the private argument bit in the preamble.
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

### Enabling User-defined Types to be Logged

The  `PackedLogMsg.StringInterpolation` struct can be extended in order to log user-defined types,
outside the standard library.
To log a user-defined type it suffices to define an overload of `appendInterpolation`
on `PackedLogMsg.StringInterpolation`  that accepts the user-defined type and
interpolates the properties of the type as desired.
The following code snippet illustrates this on a struct `Developer` that uses an enum
`Language`.
With this implementation, one can directly log instances of `Developer` as illustrated
by `osLog("Developer \(dev) made a commit")`.
Note that the extensions reuse the string interpolation methods on types already
supported by `PackedLogMsg`.


```swift
struct Developer {
  let Id: Int
  var Name: String
  var preferredLanguage: String
}

enum Language {
  case swift
  case objectiveC
  case cpp
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
## Optimization

The overall optimization workflow is summarized below.
We propose to create a dedicated optimization pass that drives the following steps.
The goal of this optimization is to match the performance of the code generated by Clang for the `os_log` calls in C.

1. The [compile-time interpreter](https://forums.swift.org/t/compile-time-constant-expressions-for-swift/12879)   is run on the string interpolation calls to infer the compile-time value of the struct `packedMsg` and its properties.

2. The implementation of `osLog` is optimized by folding in the uses of the properties of
   `packedMsg` by their constant values. This involves replacing accesses to
  `formatString` property with the inferred string literal, replacing `totalArgsByteCount`
  with the inferred constant, and replacing all indirect invocation of the closures tracked by
  `packedMsg.encodeOps` with their inferred targets.

3. All invocations of autoclosures that were created from the arguments to the string
  interpolation methods are converted to direct calls based on the targets inferred by the
  compile-time interpreter.

4. Existing function inlining pass is used to inline all direct function calls to `ByteEncoder`
  methods and closure targets.

5. Existing dead-code and dead-object elimination passes are used to eliminate the `packedMsg` instance,
which is now unused as its uses have been folded, and also to eliminate the string interpolation calls.

6. The heap-allocation of the byte buffer in the `osLog` function is promoted to stack,
   based on the fact that it is deallocated at the end of the function.

If any of the above steps is not possible, we propose to present an error or warning to the user depending on whether
it results in a violation of the contract of the logging system or a performance degradation. E.g. if the format string cannot be inferred
as a compile-time constant, it's a contract violation and is therefore an error.
However, if step 6 is not possible, it is a loss of performance but is not an error.

### Compile-Time Interpreter - Upstreaming Plans and Extensions

The implementation of the interpreter is available in the Swift tensorflow branch,
and is planned to be upstreamed as described here: https://github.com/apple/swift/pull/19579.
(The upstreaming process is already underway.)
The following is a list of language features that the interpreter would have to support in order
for it to be applicable to the implementation sketched above.

  * Basic language constructs such as: integers, floats, structures, function calls and generics.
  * Array literal construction and array operations such as array indexing, array append.
  * String literal construction and string operations such as `+`, `+=` etc.
  * Closure creation, closure assignment and invocation.

The interpreter already supports most of the features listed above, except for closures.
We propose to create a new representation within the interpreter for closures,
which consists of the target of the closure (which is a SIL function) and variables
that are captured.
The closures would be treated as another kind of  _symbolic_ constant within the interpreter.
The closures that are not invoked during interpretation are allowed to use arbitrary Swift code. 
The targets of the closures that are invoked during interpretation must be interpretable
and all the captures of the closure must be constants tracked by the interpreter.
