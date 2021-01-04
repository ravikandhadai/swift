//===----------------- SwiftUITextStub.swift ----------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Foundation

@usableFromInline
internal struct LocalizationInformation {
  let localizedString: LocalizedStringKey
  let tableName: String?
  let comment: StaticString?
  
  // The values passed to this initializer will extracted by the localization
  // extraction pass in the compiler.
  @_semantics("swiftui.localization_info_init")
  public init(
      _ key: LocalizedStringKey,
      _ tableName: String?,
      _ comment: StaticString?
  ) {
    self.localizedString = key
    self.tableName = tableName
    self.comment = comment
  }
}

public struct Text {
    let key: LocalizedStringKey
    let tableName: String?
    let comment: StaticString?
  
    /// Creates a text view that displays localized content identified by a key.
    ///
    /// Use this initializer to look for the `key` parameter in a localization
    /// table and display the associated string value in the initialized text
    /// view. If the initializer can't find the key in the table, or if no table
    /// exists, the text view displays the string representation of the key
    /// instead.
    ///
    ///     Text("pencil") // Localizes the key if possible, or displays "pencil" if not.
    ///
    /// When you initialize a text view with a string literal, the view triggers
    /// this initializer because it assumes you want the string localized, even
    /// when you don't explicitly specify a table, as in the above example. If
    /// you haven't provided localization for a particular string, you still get
    /// reasonable behavior, because the initializer displays the key, which
    /// typically contains the unlocalized string.
    ///
    /// If you initialize a text view with a string variable rather than a
    /// string literal, the view triggers the ``Text/init(_:)-9d1g4``
    /// initializer instead, because it assumes that you don't want localization
    /// in that case. If you do want to localize the value stored in a string
    /// variable, you can choose to call the `init(_:tableName:bundle:comment:)`
    /// initializer by first creating a ``LocalizedStringKey`` instance from the
    /// string variable:
    ///
    ///     Text(LocalizedStringKey(someString)) // Localizes the contents of `someString`.
    ///
    /// If you have a string literal that you don't want to localize, use the
    /// ``Text/init(verbatim:)`` initializer instead.
    ///
    /// - Parameters:
    ///   - key: The key for a string in the table identified by `tableName`.
    ///   - tableName: The name of the string table to search. If `nil`, use the
    ///     table in the `Localizable.strings` file.
    ///   - bundle: The bundle containing the strings file. If `nil`, use the
    ///     main bundle.
    ///   - comment: Contextual information about this key-value pair.
    @_transparent
    @_optimize(none)
    public init(
        _ key: LocalizedStringKey,
        tableName: String? = nil,
        bundle: Bundle? = nil,
        comment: StaticString? = nil
    ) {
      self.init(LocalizationInformation(key, tableName, comment))
    }
  
    @usableFromInline
    internal init(_ info: LocalizationInformation) {
      self.key = info.localizedString
      self.tableName = info.tableName
      self.comment = info.comment
    }
}

/// The key used to look up a string in a strings file or strings dictionary
/// file.
@frozen
public struct LocalizedStringKey: Equatable, ExpressibleByStringInterpolation {
    var key: String
    /// Whether it contains arguments that need to be formatted
    var hasFormatting: Bool = false
    private var arguments: [FormatArgument]

    public init(_ value: String) {
        self.key = value
        self.arguments = []
    }

    @_semantics("swiftui.localized_string_key.init_literal")
    @inlinable
    @_semantics("constant_evaluable")
    public init(stringLiteral value: String) {
        self.init(value)
    }

    @_semantics("swiftui.localized_string_key.init_interpolation")
    @inlinable
    @_semantics("constant_evaluable")
    public init(stringInterpolation: StringInterpolation) {
        self.key = stringInterpolation.key
        self.arguments = stringInterpolation.arguments
        self.hasFormatting = true
    }

    @usableFromInline
    struct FormatArgument: Equatable {

        @usableFromInline
        init(_ value: CVarArg) {
        }

        @usableFromInline
        init(value: CVarArg, formatter: Formatter? = nil) {
        }

        @usableFromInline
        init(text: Text, seed: Int) {
        }

        @usableFromInline
        static func == (
            lhs: FormatArgument,
            rhs: FormatArgument
        ) -> Bool {
            return true
        }
    }

    @frozen
    public struct StringInterpolation: StringInterpolationProtocol {
        @usableFromInline
        internal var key: String = ""
        @usableFromInline
        internal var arguments: [FormatArgument] = []
        //private var seed = UniqueSeedGenerator()

        @_transparent
        @_optimize(none)
        public init(literalCapacity: Int, interpolationCount: Int) {
//            self.key.reserveCapacity(literalCapacity + interpolationCount * 2)
//            self.arguments.reserveCapacity(interpolationCount)
        }

        @_transparent
        @_optimize(none)
        public mutating func appendLiteral(_ literal: String) {
            key += literal //literal.replacingOccurrences(of: "%", with: "%%")
        }

        @_transparent
        @_optimize(none)
        public mutating func appendInterpolation(_ string: String) {
            key += "%@"
            arguments.append(FormatArgument(string))
        }

        @_transparent
        @_optimize(none)
        public mutating func appendInterpolation<Subject: ReferenceConvertible>(
            _ subject: Subject,
            formatter: Formatter? = nil
        ) {
            key += "%@"
            arguments.append(
                FormatArgument(value: subject as! NSObject, formatter: formatter)
            )
        }

        @_transparent
        @_optimize(none)
        public mutating func appendInterpolation<Subject: NSObject>(
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

        @_transparent
        @_optimize(none)
        public mutating func appendInterpolation<T: _FormatSpecifiable> (
            _ value: T
        ) {
            appendInterpolation(value, specifier: value._specifier)
        }

        @_transparent
        @_optimize(none)
        public mutating func appendInterpolation<T: _FormatSpecifiable> (
            _ value: T,
            // genstring depends on this specific label to extract the
            // specifier to generate localized strings.
            specifier: String
        ) {
            key += specifier
            arguments.append(FormatArgument(value._arg))
        }

//        @_transparent
//        @_optimize(none)
//        public mutating func appendInterpolation(_ text: Text) {
//            key += "%@"
//            arguments.append(
//                FormatArgument(text: text, seed: seed.generate()))
//        }
    }
}


// MARK: - _FormatSpecifiable

public protocol _FormatSpecifiable: Equatable {
    associatedtype _Arg: CVarArg
    var _arg: _Arg { get }
    var _specifier: String { get }
}

extension Int: _FormatSpecifiable {
    public var _arg: Int64 { return Int64(self) }
    public var _specifier: String { return "%lld" }
}

extension Int8: _FormatSpecifiable {
    public var _arg: Int32 { return Int32(self) }
    public var _specifier: String { return "%d" }
}

extension Int16: _FormatSpecifiable {
    public var _arg: Int32 { return Int32(self) }
    public var _specifier: String { return "%d" }
}

extension Int32: _FormatSpecifiable {
    public var _arg: Int32 { return self }
    public var _specifier: String { return "%d" }
}

extension Int64: _FormatSpecifiable {
    public var _arg: Int64 { return self }
    public var _specifier: String { return "%lld" }
}

extension UInt: _FormatSpecifiable {
    public var _arg: UInt64 { return UInt64(self) }
    public var _specifier: String { return "%llu" }
}

extension UInt8: _FormatSpecifiable {
    public var _arg: UInt32 { return UInt32(self) }
    public var _specifier: String { return "%u" }
}

extension UInt16: _FormatSpecifiable {
    public var _arg: UInt32 { return UInt32(self) }
    public var _specifier: String { return "%u" }
}

extension UInt32: _FormatSpecifiable {
    public var _arg: UInt32 { return UInt32(self) }
    public var _specifier: String { return "%u" }
}

extension UInt64: _FormatSpecifiable {
    public var _arg: UInt64 { return self }
    public var _specifier: String { return "%llu" }
}

extension Float: _FormatSpecifiable {
    public var _arg: Float { return self }
    public var _specifier: String { return "%f" }
}

#if os(macOS) && arch(x86_64)
extension Float80: _FormatSpecifiable {
    public var _arg: Float80 { return self }
    public var _specifier: String { return "%lf" }
}
#endif

extension Double: _FormatSpecifiable {
    public var _arg: Double { return self }
    public var _specifier: String { return "%lf" }
}

extension CGFloat: _FormatSpecifiable {
    public var _arg: CGFloat { return self }
    public var _specifier: String { return "%lf" }
}
