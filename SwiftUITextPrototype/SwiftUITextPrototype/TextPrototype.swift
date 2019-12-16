// Copyright © 2019 Apple Inc. All rights reserved.

import Foundation

public struct EnvironmentValues {
}

@_semantics("constant_evaluator_debug_print")
@usableFromInline
@inline(never)
internal func debugPrint<T>(_ x: T) {
}

/// A view that displays one or more lines of read-only text.
@frozen
public struct Text : Equatable {
    /// Creates an instance that displays `content` verbatim.
    @inlinable
    public init(verbatim content: String) {
        print(content)
    }

    /// Creates an instance that displays `content` verbatim.
    @_disfavoredOverload
    public init<S : StringProtocol>(_ content: S) {
        print(content)
    }

    /// Creates text that displays localized content identified by a key.
    ///
    /// - Parameters:
    ///     - key: The key for a string in the table identified by `tableName`.
    ///     - tableName: The name of the string table to search. If `nil`, uses
    ///       the table in `Localizable.strings`.
    ///     - bundle: The bundle containing the strings file. If `nil`, uses the
    ///       main `Bundle`.
    ///     - comment: Contextual information about this key-value pair.
    public init(
        _ key: LocalizedStringKey,
        tableName: String? = nil,
        bundle: Bundle? = nil,
        comment: StaticString? = nil
    ) {
      print(key.key)
    }
}

// MARK: - LocalizedStringKey

/// The key used to looked up in a .string or .stringdict file.
@frozen
public struct LocalizedStringKey : ExpressibleByStringInterpolation {
    @usableFromInline
    var key: String
    @usableFromInline
    internal var hasFormatting: Bool = false
    @usableFromInline
    internal var arguments: [FormatArgument]

    @_semantics("constant_evaluable")
    @_optimize(none)
    @inlinable
    public init(_ value: String) {
        self.key = value
        self.arguments = []
    }

    @_semantics("oslog.message.init_literal")
    @_semantics("constant_evaluable")
    @_optimize(none)
    @inlinable
    public init(stringLiteral value: String) {
      self.init(value)
    }

    @_semantics("oslog.message.init_interpolation")
    @_semantics("constant_evaluable")
    @_optimize(none)
    @inlinable
    public init(stringInterpolation: StringInterpolation) {
      self.key = stringInterpolation.key
      self.arguments = stringInterpolation.arguments
      self.hasFormatting = true
      debugPrint(self.key)
    }

    @frozen
    @usableFromInline
    struct FormatArgument {
        @usableFromInline
        let value: CVarArg
        @usableFromInline
        let formatter: Formatter?

        @_semantics("constant_evaluable")
        @inlinable
        @_optimize(none)
        init(_ value: CVarArg) {
            self.value = value
            self.formatter = nil
        }

        @_semantics("constant_evaluable")
        @inlinable
        @_optimize(none)
        init(value: CVarArg, formatter: Formatter? = nil) {
            self.value = value
            self.formatter = formatter
              // Move the copy to later.
              //.map { $0.copy() as! Formatter }
        }

        func resolve(in environment: EnvironmentValues) -> CVarArg {
            guard let formatter = formatter else {
                return value
            }
            guard let formattedArg = formatter.string(for: value)
            else {
                return ""
            }
            return formattedArg
        }
    }

    @frozen
    public struct StringInterpolation : StringInterpolationProtocol {
        @usableFromInline
        internal var key: String //= ""
        @usableFromInline
        internal var arguments: [FormatArgument] //= []
      
        @_semantics("constant_evaluable")
        @_optimize(none)
        @inlinable
        public init(literalCapacity: Int, interpolationCount: Int) {
          self.key = ""
          self.arguments = []
            //self.key.reserveCapacity(literalCapacity + interpolationCount * 2)
            //self.arguments.reserveCapacity(interpolationCount)
        }

        @_semantics("constant_evaluable")
        @_optimize(none)
        @inlinable
        public mutating func appendLiteral(_ literal: String) {
          key += literal.percentEscapedString
                 // literal.replacingOccurrences(of: "%", with: "%%")
        }

        @_semantics("constant_evaluable")
        @_semantics("consteval_opaque_argument_0")
        @_optimize(none)
        @inlinable
        public mutating func appendInterpolation(_ string: String) {
          key += "%@"
          arguments.append(FormatArgument(string))
        }

        @_semantics("constant_evaluable")
        @_semantics("consteval_opaque_argument_0")
        @_optimize(none)
        @inlinable
        public mutating func appendInterpolation<T : _FormatSpecifiable & CVarArg> (
            _ value: T
        ) {
            appendInterpolation(value, specifier: value._specifier)
        }

        @_semantics("constant_evaluable")
        @_semantics("consteval_opaque_argument_0")
        @_optimize(none)
        @inlinable
        public mutating func appendInterpolation<T : _FormatSpecifiable & CVarArg> (
            _ value: T,
            // genstring depends on this specific label to extract the
            // specifier to generate localized strings.
            specifier: String
        ) {
            key += specifier
            arguments.append(FormatArgument(value)) //._arg))
        }

        @_semantics("constant_evaluable")
        @_semantics("consteval_opaque_argument_0")
        @_semantics("consteval_opaque_argument_1")
        @_optimize(none)
        @inlinable
        public mutating func appendInterpolation<Subject : ReferenceConvertible>(
            _ subject: Subject,
            formatter: Formatter? = nil
        ) {
            key += "%@"
            arguments.append(
                FormatArgument(value: subject as! NSObject, formatter: formatter)
            )
        }

        @_semantics("constant_evaluable")
        @_semantics("consteval_opaque_argument_0")
        @_semantics("consteval_opaque_argument_1")
        @_optimize(none)
        @inlinable
        public mutating func appendInterpolation<Subject : NSObject>(
            _ subject: Subject,
            formatter: Formatter? = nil
        ) {
            // If we get an NSObject subclass we don't transform it into a
            // `String` so that when is passed to Foundation through
            // `String(format:locale:args:)` it'll use `-[NSObject description:]`
            // and do the respond to selector check `descriptionWithLocale:`.
            // This is so, for example, `NSNumber` will be formatted correctly
            // accordingly to the locale (even without using an `NSFormatter`).
            key += "%@"
            arguments.append(
                FormatArgument(value: subject, formatter: formatter)
            )
        }
    }
}

// MARK: - FormatSpecifiable
@available(iOS 13.0, macOS 10.15, tvOS 13.0, watchOS 6.0, *)
public protocol _FormatSpecifiable : Equatable {
    associatedtype _Arg : CVarArg
    var _arg: _Arg { get }
    var _specifier: String { get }
}

@available(iOS 13.0, macOS 10.15, tvOS 13.0, watchOS 6.0, *)
extension Int : _FormatSpecifiable {
    public var _arg: Int64 { return Int64(self) }
  
//    public var _specifier: String { return "%lld" }

    @_semantics("constant_evaluable")
    @_optimize(none)
    @inlinable
    public var _specifier: String {
      return Int.bitWidth == CLongLong.bitWidth ? "%lld" : "%d"
    }
}

@available(iOS 13.0, macOS 10.15, tvOS 13.0, watchOS 6.0, *)
extension Int8 : _FormatSpecifiable {
    public var _arg: Int32 { return Int32(self) }

    @_semantics("constant_evaluable")
    @_optimize(none)
    @inlinable
    public var _specifier: String { return "%hhd" }
}

@available(iOS 13.0, macOS 10.15, tvOS 13.0, watchOS 6.0, *)
extension Int16 : _FormatSpecifiable {
    public var _arg: Int32 { return Int32(self) }

    @_semantics("constant_evaluable")
    @_optimize(none)
    @inlinable
    public var _specifier: String { return "%hd" }
}

@available(iOS 13.0, macOS 10.15, tvOS 13.0, watchOS 6.0, *)
extension Int32 : _FormatSpecifiable {
    public var _arg: Int32 { return self }

    @_semantics("constant_evaluable")
    @_optimize(none)
    @inlinable
    public var _specifier: String { return "%d" }
}

@available(iOS 13.0, macOS 10.15, tvOS 13.0, watchOS 6.0, *)
extension Int64 : _FormatSpecifiable {
    public var _arg: Int64 { return self }

    @_semantics("constant_evaluable")
    @_optimize(none)
    @inlinable
    public var _specifier: String { return "%lld" }
}

@available(iOS 13.0, macOS 10.15, tvOS 13.0, watchOS 6.0, *)
extension UInt : _FormatSpecifiable {
    public var _arg: UInt64 { return UInt64(self) }

    @_semantics("constant_evaluable")
    @_optimize(none)
    @inlinable
    public var _specifier: String {
      return UInt.bitWidth == CLongLong.bitWidth ? "%llu" : "%u"
    }
}

@available(iOS 13.0, macOS 10.15, tvOS 13.0, watchOS 6.0, *)
extension UInt8 : _FormatSpecifiable {
    public var _arg: UInt32 { return UInt32(self) }

    @_semantics("constant_evaluable")
    @_optimize(none)
    @inlinable
    public var _specifier: String { return "%hhu" }
}

@available(iOS 13.0, macOS 10.15, tvOS 13.0, watchOS 6.0, *)
extension UInt16 : _FormatSpecifiable {
    public var _arg: UInt32 { return UInt32(self) }

    @_semantics("constant_evaluable")
    @_optimize(none)
    @inlinable
    public var _specifier: String { return "%hu" }
}

@available(iOS 13.0, macOS 10.15, tvOS 13.0, watchOS 6.0, *)
extension UInt32 : _FormatSpecifiable {
    public var _arg: UInt32 { return UInt32(self) }

    @_semantics("constant_evaluable")
    @_optimize(none)
    @inlinable
    public var _specifier: String { return "%u" }
}

@available(iOS 13.0, macOS 10.15, tvOS 13.0, watchOS 6.0, *)
extension UInt64 : _FormatSpecifiable {
    public var _arg: UInt64 { return self }

    @_semantics("constant_evaluable")
    @_optimize(none)
    @inlinable
    public var _specifier: String { return "%llu" }
}

@available(iOS 13.0, macOS 10.15, tvOS 13.0, watchOS 6.0, *)
extension Float : _FormatSpecifiable {
    public var _arg: Float { return self }

    @_semantics("constant_evaluable")
    @_optimize(none)
    @inlinable
    public var _specifier: String { return "%f" }
}

#if os(macOS)
@available(iOS 13.0, macOS 10.15, tvOS 13.0, watchOS 6.0, *)
extension Float80 : _FormatSpecifiable {
    public var _arg: Float80 { return self }

    @_semantics("constant_evaluable")
    @_optimize(none)
    @inlinable
    public var _specifier: String { return "%lf" }
}
#endif

@available(iOS 13.0, macOS 10.15, tvOS 13.0, watchOS 6.0, *)
extension Double : _FormatSpecifiable {
    public var _arg: Double { return self }

    @_semantics("constant_evaluable")
    @_optimize(none)
    @inlinable
    public var _specifier: String { return "%lf" }
}

@available(iOS 13.0, macOS 10.15, tvOS 13.0, watchOS 6.0, *)
extension CGFloat : _FormatSpecifiable {
    public var _arg: CGFloat { return self }

    @_semantics("constant_evaluable")
    @_optimize(none)
    @inlinable
    public var _specifier: String { return "%lf" }
}

// MARK: - System Localized Strings

//extension Text {
//    enum System {
//        #if os(iOS) || os(watchOS) || os(tvOS)
//        static let edit = Text.kitLocalized("Edit")
//        static let done = Text.kitLocalized("Done")
//        static let cancel = Text.kitLocalized("Cancel")
//        static let ok = Text.kitLocalized("OK")
//        static let on = Text.kitLocalized("On")
//        static let off = Text.kitLocalized("Off")
//        static let back = Text.kitLocalized("Back")
//        static let paste = Text.kitLocalized("Paste")
//        #else
//        static let edit = Text.kitLocalized("Edit", tableName: "InputManager")
//        static let done = Text.kitLocalized("Done", tableName: "Toolbar")
//        static let cancel = Text.kitLocalized("Cancel")
//        static let ok = Text.kitLocalized("OK")
//        static let on = Text.kitLocalized("on", tableName: "Accessibility")
//        static let off = Text.kitLocalized("off", tableName: "Accessibility")
//        static let back = Text.kitLocalized("Back")
//        static let paste = Text.kitLocalized("Paste", tableName: "MenuCommands")
//        #endif
//    }
//}

//#if os(iOS) || os(tvOS) || os(watchOS)
//import UIKit
//
//extension Bundle {
//    static var kit: Bundle {
//        Bundle(for: UIApplication.self)
//    }
//}
//
//extension Text {
//    // <rdar://problem/51235985> UIKit should expose SPI to retrieve localized strings from its bundle
//    static func kitLocalized (
//        _ key: LocalizedStringKey,
//        tableName: String = "Localizable",
//        comment: StaticString? = nil
//    ) -> Text {
//        Text(key, tableName: tableName, bundle: Bundle.kit)
//    }
//}
//
//#else
//import AppKit
//
//extension Bundle {
//    static var kit: Bundle {
//        Bundle(for: NSApplication.self)
//    }
//}
//
//extension Text {
//    // <rdar://problem/51235916> AppKit should expose SPI to retrieve localized strings from its bundle
//    static func kitLocalized (
//        _ key: LocalizedStringKey,
//        tableName: String = "Common",
//        comment: StaticString? = nil
//    ) -> Text {
//        Text(key, tableName: tableName, bundle: Bundle.kit)
//    }
//}
//
//#endif

extension String {
  /// Replace all percents "%" in the string by "%%" so that the string can be
  /// interpreted as a C format string. This function is constant evaluable
  /// and its semantics is modeled within the evaluator.
  public var percentEscapedString: String {
    @_semantics("string.escapePercent.get")
    @_effects(readonly)
    @_optimize(none)
    get {
      return self
        .split(separator: "%", omittingEmptySubsequences: false)
        .joined(separator: "%%")
    }
  }
}
