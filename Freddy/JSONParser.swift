//
//  JSONParser.swift
//  Freddy
//
//  Created by John Gallagher on 4/18/15.
//  Copyright © 2015 Big Nerd Ranch. Licensed under MIT.
//

import Foundation

// MARK: Literals

private enum Keyword: String {
    case False = "false"
    case Null  = "null"
    case True  = "true"
}

private struct Literal {
    static let BACKSLASH     = UInt8(ascii: "\\")
    static let COLON         = UInt8(ascii: ":")
    static let COMMA         = UInt8(ascii: ",")
    static let DOUBLE_QUOTE  = UInt8(ascii: "\"")
    static let LEFT_BRACE    = UInt8(ascii: "{")
    static let LEFT_BRACKET  = UInt8(ascii: "[")
    static let MINUS         = UInt8(ascii: "-")
    static let NEWLINE       = UInt8(ascii: "\n")
    static let PERIOD        = UInt8(ascii: ".")
    static let PLUS          = UInt8(ascii: "+")
    static let RETURN        = UInt8(ascii: "\r")
    static let RIGHT_BRACE   = UInt8(ascii: "}")
    static let RIGHT_BRACKET = UInt8(ascii: "]")
    static let SPACE         = UInt8(ascii: " ")
    static let TAB           = UInt8(ascii: "\t")

    static let e = UInt8(ascii: "e")
    static let f = UInt8(ascii: "f")
    static let n = UInt8(ascii: "n")
    static let t = UInt8(ascii: "t")
    static let u = UInt8(ascii: "u")
    static let E = UInt8(ascii: "E")

    static var Digits: ClosedInterval<UInt8> { return UInt8(ascii: "0")...UInt8(ascii: "9") }
}

private let ParserMaximumDepth = 512

/**
A pure Swift JSON parser. This parser is much faster than the
`NSJSONSerialization`-based parser (due to the overhead of having to
dynamically cast the Objective-C objects to determine their type); however,
it is much newer and has restrictions that the `NSJSONSerialization` parser
does not. Two restrictions in particular are that it requires UTF-8 data as
input and it does not allow trailing commas in arrays or dictionaries.
**/
public struct JSONParser {

    private enum Sign: Int {
        case Positive = 1
        case Negative = -1
    }

    private let input: UnsafeBufferPointer<UInt8>
    private let owner: Any?
    private var loc = 0
    private var depth = 0

    private init<T>(buffer: UnsafeBufferPointer<UInt8>, owner: T) {
        self.input = buffer
        self.owner = owner
    }

    public mutating func parse() throws -> JSON {
        let value = try parseValue()
        skipWhitespace()
        guard loc == input.count else {
            throw Error.EndOfStreamGarbage(offset: loc)
        }
        return value
    }

    private mutating func parseValue() throws -> JSON {
        guard depth <= ParserMaximumDepth else {
            throw Error.ExceededNestingLimit(offset: loc)
        }

        advancing: while loc < input.count {
            switch input[loc] {
            case Literal.LEFT_BRACKET:
                depth += 1
                defer { depth -= 1 }
                return try decodeArray()

            case Literal.LEFT_BRACE:
                depth += 1
                defer { depth -= 1 }
                return try decodeObject()

            case Literal.DOUBLE_QUOTE:
                return try decodeString()

            case Literal.f:
                return try decodeKeyword(.False, with: .Bool(false))

            case Literal.n:
                return try decodeKeyword(.Null, with: .Null)

            case Literal.t:
                return try decodeKeyword(.True, with: .Bool(true))

            case Literal.MINUS:
                return try decodeNumberNegative(loc)

            case Literal.Digits.start:
                return try decodeNumberLeadingZero(loc)

            case Literal.Digits:
                return try decodeNumberPreDecimalDigits(loc)

            case Literal.SPACE, Literal.TAB, Literal.RETURN, Literal.NEWLINE:
                loc = loc.successor()

            default:
                break advancing
            }
        }
        
        throw Error.ValueInvalid(offset: loc, character: UnicodeScalar(input[loc]))
    }

    private mutating func skipWhitespace() {
        while loc < input.count {
            switch input[loc] {
            case Literal.SPACE, Literal.TAB, Literal.RETURN, Literal.NEWLINE:
                loc = loc.successor()

            default:
                return
            }
        }
    }

    private mutating func decodeKeyword(keyword: Keyword, @autoclosure with json: () -> JSON) throws -> JSON {
        let start = loc
        let end = start.advancedBy(keyword.rawValue.utf8.count - 1, limit: input.endIndex)

        guard end != input.endIndex else {
            throw Error.EndOfStreamUnexpected
        }
        
        guard input[start ... end].elementsEqual(keyword.rawValue.utf8) else {
            throw Error.KeywordMisspelled(offset: start, text: keyword.rawValue)
        }

        loc = end.successor()
        return json()
    }

    private mutating func scanUntilEndOfString() throws -> (Range<Int>, needsEscapes: (unicode: Bool, count: Int)?) {
        var inEscape = false
        var offset = 0
        var needsUnicode = false
        var range = loc..<loc

        loop: while loc != input.endIndex {
            switch input[loc] {
            case Literal.DOUBLE_QUOTE where !inEscape:
                range.endIndex = loc
                break loop
            case Literal.BACKSLASH:
                inEscape = true
            case Literal.u where inEscape:
                loc = loc.advancedBy(4, limit: input.endIndex)
                inEscape = false
                needsUnicode = true
                offset += 5
            case _ where inEscape:
                inEscape = false
                offset += 1
            default: break
            }

            loc = loc.successor()
        }

        // input[loc] should at the closing " right now
        guard loc != input.endIndex else {
            throw Error.EndOfStreamUnexpected
        }

        loc = loc.successor()

        return (range, offset == 0 ? nil : (needsUnicode, offset))
    }

    private mutating func decodeString() throws -> JSON {
        // skip past the opening "
        let start = loc
        loc = loc.successor()

        // scan until we find the closing "
        let (range, needsEscapes) = try scanUntilEndOfString()

        // Degenerate case; no escapes.
        guard let (needsUnicode, escapeOffset) = needsEscapes else {
            // Ideally we'd use String.fromCString(_:) but that requires a NUL terminator.
            // And we could use String.init(bytes:encoding:), but that copies to an Array first.
            let ptr = input.baseAddress.advancedBy(range.startIndex)
            let string = NSString(bytes: ptr, length: range.count, encoding: NSUTF8StringEncoding) as! String
            return .String(string)
        }

        // "If `repairIllFormedSequences` is `true`, the function always succeeds."
        let (utf16Count, isAscii) = UTF16.measure(UTF8.self, input: input[range].generate(), repairIllFormedSequences: true)!
        let codeUnitCount = utf16Count - escapeOffset

        if isAscii && !needsUnicode {
            var string = ""
            var escapeParser = EscapeParser(start: start)
            string.reserveCapacity(codeUnitCount)

            try input[range].transcodeFrom(UTF8.self, to: UTF32.self, transform: {
                try escapeParser.parse($0)
            }, output: {
                string.append(UnicodeScalar($0))
            })

            return .String(string)
        } else {
            let buf = UnsafeMutablePointer<UTF16.CodeUnit>.alloc(codeUnitCount)
            var ptr = buf
            var escapeParser = EscapeParser(start: start)

            try input[range].transcodeFrom(UTF8.self, to: UTF16.self, transform: {
                try escapeParser.parse($0)
            }, output: {
                ptr.memory = $0
                ptr += 1
            })
            
            let string = String(utf16CodeUnitsNoCopy: buf, count: codeUnitCount, freeWhenDone: true)
            return .String(string)
        }
    }

    private mutating func decodeArray() throws -> JSON {
        let start = loc
        loc = loc.successor()
        var items = [JSON]()

        while loc < input.count {
            skipWhitespace()

            if loc < input.count && input[loc] == Literal.RIGHT_BRACKET {
                loc = loc.successor()
                return .Array(items)
            }

            if !items.isEmpty {
                guard loc < input.count && input[loc] == Literal.COMMA else {
                    throw Error.CollectionMissingSeparator(offset: start)
                }
                loc = loc.successor()
            }

            items.append(try parseValue())
        }

        throw Error.EndOfStreamUnexpected
    }

    // Decoding objects can be recursive, so we have to keep more than one
    // buffer around for building up key/value pairs (to reduce allocations
    // when parsing large JSON documents).
    //
    // Rough estimate of the difference between this and using a fresh
    // [(String,JSON)] for the `pairs` variable in decodeObject() below is
    // about 12% on an iPhone 5.
    private struct DecodeObjectBuffers {
        var buffers = [[(String,JSON)]]()

        mutating func getBuffer() -> [(String,JSON)] {
            if !buffers.isEmpty {
                var buffer = buffers.removeLast()
                buffer.removeAll(keepCapacity: true)
                return buffer
            }
            return [(String,JSON)]()
        }

        mutating func putBuffer(buffer: [(String,JSON)]) {
            buffers.append(buffer)
        }
    }

    private var decodeObjectBuffers = DecodeObjectBuffers()

    private mutating func decodeObject() throws -> JSON {
        let start = loc
        loc = loc.successor()
        var pairs = decodeObjectBuffers.getBuffer()

        while loc < input.count {
            skipWhitespace()

            if loc < input.count && input[loc] == Literal.RIGHT_BRACE {
                loc = loc.successor()
                var obj = [String:JSON](minimumCapacity: pairs.count)
                for (k, v) in pairs {
                    obj[k] = v
                }
                decodeObjectBuffers.putBuffer(pairs)
                return .Dictionary(obj)
            }

            if !pairs.isEmpty {
                guard loc < input.count && input[loc] == Literal.COMMA else {
                    throw Error.CollectionMissingSeparator(offset: start)
                }
                loc = loc.successor()

                skipWhitespace()
            }

            guard loc < input.count && input[loc] == Literal.DOUBLE_QUOTE else {
                throw Error.DictionaryMissingKey(offset: start)
            }

            let key = try decodeString().string()
            skipWhitespace()

            guard loc < input.count && input[loc] == Literal.COLON else {
                throw Error.CollectionMissingSeparator(offset: start)
            }
            loc = loc.successor()

            pairs.append((key, try parseValue()))
        }

        throw Error.EndOfStreamUnexpected
    }

    private mutating func decodeNumberNegative(start: Int) throws -> JSON {
        loc = loc.successor()
        guard loc < input.count else {
            throw Error.EndOfStreamUnexpected
        }

        switch input[loc] {
        case Literal.Digits.start:
            return try decodeNumberLeadingZero(start, sign: .Negative)

        case Literal.Digits:
            return try decodeNumberPreDecimalDigits(start, sign: .Negative)

        default:
            throw Error.NumberSymbolMissingDigits(offset: start)
        }
    }

    private mutating func decodeNumberLeadingZero(start: Int, sign: Sign = .Positive) throws -> JSON {
        loc = loc.successor()
        guard loc < input.count else {
            return .Int(0)
        }

        switch (input[loc], sign) {
        case (Literal.PERIOD, _):
            return try decodeNumberDecimal(start, sign: sign, value: 0)

        case (_, .Negative):
            return .Double(-0.0)

        default:
            return .Int(0)
        }
    }

    private mutating func decodeNumberPreDecimalDigits(start: Int, sign: Sign = .Positive) throws -> JSON {
        var value = 0

        advancing: while loc < input.count {
            let c = input[loc]
            switch c {
            case Literal.Digits:
                value = 10 * value + Int(c - Literal.Digits.start)
                loc = loc.successor()

            case Literal.PERIOD:
                return try decodeNumberDecimal(start, sign: sign, value: Double(value))

            case Literal.e, Literal.E:
                return try decodeNumberExponent(start, sign: sign, value: Double(value))

            default:
                break advancing
            }
        }

        return .Int(sign.rawValue * value)
    }

    private mutating func decodeNumberDecimal(start: Int, sign: Sign, value: Double) throws -> JSON {
        loc = loc.successor()
        guard loc < input.count else {
            throw Error.EndOfStreamUnexpected
        }

        switch input[loc] {
        case Literal.Digits:
            return try decodeNumberPostDecimalDigits(start, sign: sign, value: value)

        default:
            throw Error.NumberMissingFractionalDigits(offset: start)
        }
    }

    private mutating func decodeNumberPostDecimalDigits(start: Int, sign: Sign, value inValue: Double) throws -> JSON {
        var value = inValue
        var position = 0.1

        advancing: while loc < input.count {
            let c = input[loc]
            switch c {
            case Literal.Digits:
                value += position * Double(c - Literal.Digits.start)
                position /= 10
                loc = loc.successor()

            case Literal.e, Literal.E:
                return try decodeNumberExponent(start, sign: sign, value: value)

            default:
                break advancing
            }
        }

        return .Double(Double(sign.rawValue) * value)
    }

    private mutating func decodeNumberExponent(start: Int, sign: Sign, value: Double) throws -> JSON {
        loc = loc.successor()
        guard loc < input.count else {
            throw Error.EndOfStreamUnexpected
        }

        switch input[loc] {
        case Literal.Digits:
            return try decodeNumberExponentDigits(start, sign: sign, value: value, expSign: .Positive)

        case Literal.PLUS:
            return try decodeNumberExponentSign(start, sign: sign, value: value, expSign: .Positive)

        case Literal.MINUS:
            return try decodeNumberExponentSign(start, sign: sign, value: value, expSign: .Negative)

        default:
            throw Error.NumberSymbolMissingDigits(offset: start)
        }
    }

    private mutating func decodeNumberExponentSign(start: Int, sign: Sign, value: Double, expSign: Sign) throws -> JSON {
        loc = loc.successor()
        guard loc < input.count else {
            throw Error.EndOfStreamUnexpected
        }

        switch input[loc] {
        case Literal.Digits:
            return try decodeNumberExponentDigits(start, sign: sign, value: value, expSign: expSign)

        default:
            throw Error.NumberSymbolMissingDigits(offset: start)
        }
    }

    private mutating func decodeNumberExponentDigits(start: Int, sign: Sign, value: Double, expSign: Sign) throws -> JSON {
        var exponent: Double = 0

        advancing: while loc < input.count {
            let c = input[loc]
            switch c {
            case Literal.Digits:
                exponent = exponent * 10 + Double(c - Literal.Digits.start)
                loc = loc.successor()

            default:
                break advancing
            }
        }

        return .Double(Double(sign.rawValue) * value * pow(10, Double(expSign.rawValue) * exponent))
    }
}

public extension JSONParser {

    init(utf8Data inData: NSData) {
        let data = inData.copy() as! NSData
        let buffer = UnsafeBufferPointer(start: UnsafePointer<UInt8>(data.bytes), count: data.length)
        self.init(buffer: buffer, owner: data)
    }

    init(string: String) {
        let codePoints = string.nulTerminatedUTF8
        let buffer = codePoints.dropLast().withUnsafeBufferPointer { $0 }
        self.init(buffer: buffer, owner: codePoints)
    }

}

extension JSONParser: JSONParserType {

    public static func createJSONFromData(data: NSData) throws -> JSON {
        var parser = JSONParser(utf8Data: data)
        return try parser.parse()
    }

}

// MARK: - String parsing

private extension UnicodeScalar {
    init(surrogatePair leading: UInt16, _ trailing: UInt16) {
        self.init()
        let codepoints = [ leading, trailing ]
        transcode(UTF16.self, UTF32.self, codepoints.generate(), {
            self = UnicodeScalar($0)
            }, stopOnError: false)
    }

    init?(controlCharacter control: UnicodeScalar) {
        switch control {
        case "\\", "/", "\"":
            self = control
        case "b": // backspace
            self = "\u{8}"
        case "t": // tab
            self = "\u{9}"
        case "n": // new line
            self = "\u{a}"
        case "f": // form feed
            self = "\u{c}"
        case "r": // carriage return
            self = "\u{d}"
        default:
            return nil
        }
    }
}

private extension CollectionType where Generator.Element: UnsignedIntegerType {
    // Partial reimplementation of transcode(_:_:_,_:stopOnError:).
    // https://github.com/apple/swift/blob/master/stdlib/public/core/Unicode.swift
    func transcodeFrom<Input: UnicodeCodecType, Output: UnicodeCodecType where Input.CodeUnit == Generator.Element>(_: Input.Type, to _: Output.Type, @noescape transform: UnicodeScalar throws -> UnicodeScalar?, output: Output.CodeUnit -> ()) rethrows {
        var input = generate()
        var decoder = Input()
        repeat {
            let scalar: UnicodeScalar
            switch decoder.decode(&input) {
            case let .Result(us):
                scalar = us
            case .EmptyInput:
                return
            case .Error:
                scalar = "\u{fffd}"
            }

            if let transformed = try transform(scalar) {
                Output.encode(transformed, output: output)
            }
        } while true
    }
}

extension JSONParser {
    private struct EscapeParser {
        enum State {
            case None
            // consumed a "\", now looking for a control character
            case BeginControl
            // parsing a Unicode escape
            case UnicodeEscape(UInt16, remaining: Int)
            // got a Unicode character, but UTF-16 says we need another
            case NeedSurrogatePair
        }

        let start: Int
        var state = State.None
        var priorCodepoint: UInt16?

        init(start: Int) {
            self.start = start
        }

        func hexFromScalar(scalar: UnicodeScalar) throws -> UInt16 {
            let digits: ClosedInterval<UnicodeScalar> = "0"..."9"
            let lower:  ClosedInterval<UnicodeScalar> = "a"..."f"
            let upper:  ClosedInterval<UnicodeScalar> = "A"..."F"

            switch scalar {
            case digits: return UInt16(scalar.value - digits.start.value)
            case lower:  return UInt16(scalar.value - lower.start.value) + 10
            case upper:  return UInt16(scalar.value - upper.start.value) + 10
            default:     throw Error.UnicodeEscapeInvalid(offset: start)
            }
        }

        mutating func parse(scalar: UnicodeScalar) throws -> UnicodeScalar? {
            switch (state, scalar) {
            case (.None, "\\"), (.NeedSurrogatePair, "\\"):
                state = .BeginControl
            case (.None, let next):
                return next


            case (.BeginControl, "u"):
                state = .UnicodeEscape(0, remaining: 4)
                return nil
            case (.BeginControl, _) where priorCodepoint != nil:
                throw Error.UnicodeEscapeInvalid(offset: start)
            case (.BeginControl, let next):
                guard let control = UnicodeScalar(controlCharacter: next) else {
                    throw Error.ControlCharacterUnrecognized(offset: start)
                }
                state = .None
                return control


            case let (.UnicodeEscape(current, remaining), scalar):
                let codepoint = try (current << 4) | hexFromScalar(scalar)
                switch (remaining - 1, priorCodepoint) {
                case let (0, prior?):
                    state = .None
                    priorCodepoint = nil
                    return UnicodeScalar(surrogatePair: prior, codepoint)
                case (0, nil) where UTF16.isLeadSurrogate(codepoint):
                    priorCodepoint = codepoint
                    state = .NeedSurrogatePair
                case (0, nil):
                    state = .None
                    priorCodepoint = nil
                    return UnicodeScalar(codepoint)
                case let (nextRemaining, _):
                    state = .UnicodeEscape(codepoint, remaining: nextRemaining)
                }


            case (.NeedSurrogatePair, _):
                throw Error.UnicodeEscapeInvalid(offset: start)
            }

            return nil
        }
    }
}

// MARK: - Errors

extension JSONParser {

    /// Enumeration describing possible errors that occur while parsing a JSON
    /// document. Most errors include an associated `offset`, representing the
    /// offset into the UTF-8 characters making up the document where the error
    /// occurred.
    public enum Error: ErrorType {
        /// The parser ran out of data prematurely. This usually means a value
        /// was not escaped, such as a string literal not ending with a double
        /// quote.
        case EndOfStreamUnexpected
        
        /// Unexpected non-whitespace data was left around `offset` after
        /// parsing all valid JSON.
        case EndOfStreamGarbage(offset: Int)
        
        /// Too many nested objects or arrays occured at the literal started
        /// around `offset`.
        case ExceededNestingLimit(offset: Int)
        
        /// A `character` was not a valid start of a value around `offset`.
        case ValueInvalid(offset: Int, character: UnicodeScalar)

        /// Badly-formed Unicode sequence at `offset`. A string must be valid
        /// UTF-8 with escapes.
        case StringInvalid(offset: Int)

        /// Badly-formed Unicode escape sequence at `offset`. A Unicode escape
        /// uses the text "\u" followed by 4 hex digits, such as "\uF09F\uA684"
        /// to represent U+1F984, "UNICORN FACE".
        case UnicodeEscapeInvalid(offset: Int)
        
        /// Badly-formed control character around `offset`. JSON supports
        /// backslash-escaped double quotes, slashes, whitespace control codes,
        /// and Unicode escape sequences.
        case ControlCharacterUnrecognized(offset: Int)
        
        /// Invalid token, expected `text` around `offset`
        case KeywordMisspelled(offset: Int, text: String)
        
        /// Badly-formed collection at given `offset`, expected `,` or `:`
        case CollectionMissingSeparator(offset: Int)
        
        /// While parsing an object literal, a value was found without a key
        /// around `offset`. The start of a string literal was expected.
        case DictionaryMissingKey(offset: Int)
        
        /// Badly-formed number with no digits around `offset`. After a decimal
        /// point, a number must include some number of digits.
        case NumberMissingFractionalDigits(offset: Int)
        
        /// Badly-formed number with symbols ("-" or "e") but no following
        /// digits around `offset`.
        case NumberSymbolMissingDigits(offset: Int)
    }

}
