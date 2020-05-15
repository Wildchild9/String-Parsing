//
//  main.swift
//  String Parsing
//
//  Created by Noah Wilder on 2020-05-14.
//  Copyright © 2020 Noah Wilder. All rights reserved.
//

import Foundation

print("Hello world")

enum TokenizationError {
    case ambiguousToken
}
protocol Tokenizable {
    func consumeNextToken<T>(from input: T) throws -> Substring? where T: StringProtocol, T.SubSequence == Substring
}
extension Tokenizable {
    func matches<T>(_ input: T) -> Bool where T: StringProtocol, T.SubSequence == Substring {
        guard let _ = try? consumeNextToken(from: input) else {
            return false
        }
        return true
    }
}

struct BaseToken {
    var definition: String
    var id: String = ""
    
    init(_ definition: String) {
        self.definition = definition
    }
}
extension BaseToken: Tokenizable {
    func consumeNextToken<T>(from input: T) throws -> Substring? where T: StringProtocol, T.SubSequence == Substring {
        guard definition.count <= input.count else {
            return nil
        }
        var idx = input.startIndex
        for char in definition {
            guard char == input[idx] else {
                return nil
            }
            idx = input.index(after: idx)
        }
        return input[idx...]
    }
}

extension String: Tokenizable {
    func consumeNextToken<T>(from input: T) throws -> Substring? where T: StringProtocol, T.SubSequence == Substring {
        let token = BaseToken(self)
        return try token.consumeNextToken(from: input)
    }
}
struct LogicalOrToken<A, B> where A: Tokenizable, B: Tokenizable {
    var a: A
    var b: B
}

extension LogicalOrToken: Tokenizable {
    // Currently, the left-most token that's matched will be used
    // Solutions:
    //   - Add precedence
    //   - Check length
    //   - Proceed with tokenization but mark short-circuit so if there is an error down the line you can return (don't know if that is possible with this model)
    //    func consumeNextToken<T>(from input: T) throws -> (tokens: [Tokenizable], output: Substring? where T: StringProtocol, T.SubSequence == Substring {
    
    func consumeNextToken<T>(from input: T) throws -> Substring? where T: StringProtocol, T.SubSequence == Substring {
        if let tokenizedOutput = try a.consumeNextToken(from: input) {
            return tokenizedOutput
        } else if let tokenizedOutput = try b.consumeNextToken(from: input) {
            return tokenizedOutput
        }
        return nil
    }
}

func || <T, U>(lhs: T, rhs: U) -> LogicalOrToken<T, U> where T: Tokenizable, U: Tokenizable {
    return LogicalOrToken(a: lhs, b: rhs)
}

struct OptionalToken<T> where T: Tokenizable {
    var token: T
    init(_ token: T) {
        self.token = token
    }
}
extension OptionalToken: Tokenizable {
    func consumeNextToken<T>(from input: T) throws -> Substring? where T: StringProtocol, T.SubSequence == Substring {
        if let output = try token.consumeNextToken(from: input) {
            return output
        }
        return input[...]
    }
}

extension Tokenizable {
    func opt() -> OptionalToken<Self> {
        return OptionalToken(self)
    }
}

struct TokenChain<A, B>: Tokenizable where A: Tokenizable, B: Tokenizable {
    var a: A
    var b: B
    
    func consumeNextToken<T>(from input: T) throws -> Substring? where T: StringProtocol, T.SubSequence == Substring {
        guard let partiallyConsumedStr = try a.consumeNextToken(from: input) else {
            return nil
        }
        return try b.consumeNextToken(from: partiallyConsumedStr)
    }
}
func +<T, U>(lhs: T, rhs: U) -> TokenChain<T, U> where T: Tokenizable, U: Tokenizable {
    return TokenChain(a: lhs, b: rhs)
}

class AnyToken: Tokenizable {
    var token: Tokenizable!
    
    init() { }
    init(_ token: Tokenizable) {
        self.token = token
    }
    
    func consumeNextToken<T>(from input: T) throws -> Substring? where T: StringProtocol, T.SubSequence == Substring {
        return try token.consumeNextToken(from: input)
    }
}

class RecursiveToken: Tokenizable {
    var token: Tokenizable!
    init(_ token: (RecursiveToken) -> Tokenizable) {
        self.token = token(self)
    }
    func consumeNextToken<T>(from input: T) throws -> Substring? where T: StringProtocol, T.SubSequence == Substring {
        return try token.consumeNextToken(from: input)
    }
}

//var digit = "0" || "1" || "2" || "3" || "4" || "5" || "6" || "7" || "8" || "9"
//
////var digits = AnyToken()
////var _digits = digits
////digits.token = digit + _digits???
//var digits = RecursiveToken { digit + $0.opt() }
////var (digits, _digits) = AnyToken.shared
////digits.token = digit + _digits*
//var str = "1234abc"
//print(try digit.consumeNextToken(from: str)!)
//print(try digits.consumeNextToken(from: str)!)
////var digit = BaseToken("0") | "1"// | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
//print(type(of: digit))
//print(type(of: digits))
//
//print("hello")
//
//
//var thing = 0_______6
//var decimalLiteralCharacter = digit || "_"
//var decimalLiteralCharacters = RecursiveToken { decimalLiteralCharacter + $0.opt() }
//var decimalLiteral = digit + decimalLiteralCharacters.opt()
//print(try decimalLiteral.consumeNextToken(from: "1___2_3435_452abc"))
//struct RecursiveToken: Tokenizable {
//    init() { }
//}
//
//precedencegroup GrammarAssignmentPrecedence {
//    higherThan: AssignmentPrecedence
//    lowerThan: FunctionArrowPrecedence
//}
//
//prefix operator <=//: GrammarAssignmentPrecedence
//prefix func <=<T>(_ token: () -> T) -> T where T: Tokenizable {
//    return token()
//}
//prefix func <=<T>(_ token: @autoclosure () -> T) -> T where T: Tokenizable {
//    return token()
//}
//prefix func <= (_ token: (RecursiveToken) -> Tokenizable) -> RecursiveToken {
//    return RecursiveToken(token)
//}
//
//func token<T>(_ token: () -> T) -> T where T: Tokenizable {
//    return token()
//}
//func token<T>(_ token: @autoclosure () -> T) -> T where T: Tokenizable {
//    return token()
//}
//func token (_ token: (RecursiveToken) -> Tokenizable) -> RecursiveToken {
//    return RecursiveToken(token)
//}

let prefix_operator = "+" || "-"

let digit = "0" || "1" || "2" || "3" || "4" || "5" || "6" || "7" || "8" || "9"
let digits = RecursiveToken { digit + $0.opt() }

let number = digits + ("." + digits).opt()

let spacing = RecursiveToken { " " + $0.opt() }

let expression = AnyToken()
let expression_list = RecursiveToken { expression || (spacing.opt() + "," + spacing.opt() + $0) }

let binary_operator = "+" || "-" || "*" || "/" || "^"
let spaced_binary_operator = RecursiveToken { binary_operator || (" " + $0 + " ") }

let lowercase_latin_character = "a" || "b" || "c" || "d" || "e" || "f" || "g" || "h" || "i" || "j" || "k" || "l" || "m" || "n" || "o" || "p" || "q" || "r" || "s" || "t" || "u" || "v" || "w" || "x" || "y" || "z"
let uppercase_latin_character = "A" || "B" || "C" || "D" || "E" || "F" || "G" || "H" || "I" || "J" || "K" || "L" || "M" || "N" || "O" || "P" || "Q" || "R" || "S" || "T" || "U" || "V" || "W" || "X" || "Y" || "Z"
let latin_character = lowercase_latin_character || uppercase_latin_character

let identifier = RecursiveToken { latin_character + $0.opt() }

let parenthesized_expression = "(" + spacing.opt() + expression + spacing.opt() + ")"

let unary_function_expression =  identifier + parenthesized_expression
let temp1 = identifier + "(" + spacing.opt()
let temp2 = temp1 + expression + spacing.opt() + ","
let binary_function_expression = temp2 + spacing.opt() + expression + spacing.opt() + ")"

let function_expression = unary_function_expression || binary_function_expression

let eulers_number = "e"
let pi = "π" || "pi"

let constant = prefix_operator.opt() + (eulers_number || pi)

let primary_expression = function_expression || number || constant || parenthesized_expression

let prefix_expression = prefix_operator.opt() + primary_expression

let binary_expression = spaced_binary_operator + prefix_expression
let binary_expressions = RecursiveToken { binary_expression + $0.opt() }

expression.token = prefix_expression + binary_expressions.opt()

var exp = "((4) + 10 - 7) - (-64.29434) - 7 ^ 4 + cos(180 - 493 + sin(20))"
print(exp)

//print(try expression.consumeNextToken(from: exp))
//var output = ""
//dump(expression, to: &output)
//
//output = output.replacingOccurrences(of: "__lldb_expr_\\d+.", with: "", options: .regularExpression)
//print(output)
//
//print(try eulers_number.consumeNextToken(from: "eat"))
//print(try eulers_number.consumeNextToken(from: "at"))


//protocol Token {
//    static func consume<T>(_ str: T) -> (token: Self, consumedString: Substring)? where T: StringProtocol, T.SubSequence == Substring
//
//    func decomposed() -> [Token]
//}
//
//extension Token {
//
//    func decomposed() -> [Token] {
//        return [self]
//    }
//}
//
//
////class Tokenizer {
////    var topLevelToken: Token
////    var tokenTypes: [Token.Type]
////    func tokenize(_ str: String) -> [Token] {
////
////    }
////}
//// Decide how to handle whitespace (maybe make a separate token for whitespace and newline)
//
//
//typealias Pattern = Tokenizable
////struct TokenMatch {
////    var pattern: Tokenizable
////    var token: Token
////}
//
//protocol EnumToken: Token, CaseIterable {
//    var pattern: Tokenizable { get }
//}
//
//extension EnumToken {
//    static func consume<T>(_ str: T) -> (token: Self, consumedString: Substring)? where T: StringProtocol, T.SubSequence == Substring {
//        for token in allCases {
//            let pattern = token.pattern
//            if let consumedStr = try? pattern.consumeNextToken(from: str) {
//                return (token: token, consumedString: consumedStr)
//            }
//        }
//        return nil
//    }
//}
//
//enum ConstantToken: EnumToken {
//    case e
//    case pi
//
//    var pattern: Pattern {
//        switch self {
//        case .e: return "e"
//        case .pi: return "π" || "pi"
//        }
//    }
//}
//
//struct NumberToken: Token {
//    var value: Double
//
//    private enum Patterns {
//        static let digit = "0" || "1" || "2" || "3" || "4" || "5" || "6" || "7" || "8" || "9"
//        static let digits = RecursiveToken { digit + $0.opt() }
//        static let number = digits + ("." + digits).opt()
//    }
//
//    static func consume<T>(_ str: T) -> (token: Self, consumedString: Substring)? where T: StringProtocol, T.SubSequence == Substring {
//        guard let consumedStr = try? Patterns.number.consumeNextToken(from: str) else {
//            return nil
//        }
//        let matchStr = str[str.startIndex..<consumedStr.startIndex]
//        guard let value = Double(matchStr) else {
//            assertionFailure("A number was successfully parsed from the input as \"\(matchStr)\", but could not be converted to a Double.")
//            return nil
//        }
//        return (token: NumberToken(value: value), consumedString: consumedStr)
//    }
//}
//
//enum PrefixOperatorToken: EnumToken {
//    case positive
//    case negative
//
//    var pattern: Pattern {
//        switch self {
//        case .positive: return "+"
//        case .negative: return "-"
//        }
//    }
//}
//
//enum BinaryOperatorToken: EnumToken {
//    case addition
//    case subtraction
//    case multiplication
//    case division
//    case exponentiation
//
//    var pattern: Pattern {
//        switch self {
//        case .addition: return "+"
//        case .subtraction: return "-"
//        case .multiplication: return "*"
//        case .division: return "/"
//        case .exponentiation: return "^"
//        }
//    }
//}
//
//struct IdentifierToken: Token {
//    var identifier: String
//
//    init(_ identifier: String) {
//        self.identifier = identifier
//    }
//    static func consume<T>(_ str: T) -> (token: Self, consumedString: Substring)? where T: StringProtocol, T.SubSequence == Substring {
//        guard !str.isEmpty else { return nil }
//        var idx = str.startIndex
//        let firstCharacter = str[idx]
//        // Ensure the first character is a letter
//        guard firstCharacter.isLetter else { return nil }
//        var identifierStr = String(firstCharacter)
//
//        idx = str.index(after: idx)
//        while idx < str.endIndex {
//            let character = str[idx]
//            // Ensure that subsequent characters are a letter, number, or underscore
//            guard character.isLetter || Set<Character>("0123456789_").contains(character) else {
//                break
//            }
//            identifierStr.append(character)
//            idx = str.index(after: idx)
//        }
//        return (token: IdentifierToken(identifierStr), consumedString: str[idx...])
//    }
//}
//
//struct FunctionToken: Token {
//    var identifier: IdentifierToken
//    var arguments: [[Token]]
//
//    var arity: Int {
//        return arguments.count
//    }
//
//    static func consume<T>(_ str: T) -> (token: Self, consumedString: Substring)? where T: StringProtocol, T.SubSequence == Substring {
//
//        guard let (identifier, consumedStr1) = IdentifierToken.consume(str) else {
//            return nil
//        }
//        var substring = consumedStr1.drop { $0.isWhitespace }
//        guard substring.hasPrefix("(") else { return nil }
//        substring = substring.dropFirst()
//
//        var arguments = [ExpressionToken]()
//
//        guard let (firstArgument, consumedStr2) = ExpressionToken.consume(substring) else {
//            return nil
//        }
//        substring = consumedStr2
//        arguments.append(firstArgument)
//
//        substring = substring.drop { $0.isWhitespace }
//
//        while substring.hasPrefix(",") {
//            substring = substring.dropFirst()
//            substring = substring.drop { $0.isWhitespace }
//            guard let (argument, consumedStr3) = ExpressionToken.consume(substring) else {
//                return nil
//            }
//            arguments.append(argument)
//            substring = consumedStr3
//            substring = substring.drop { $0.isWhitespace }
//        }
//
//        guard substring.hasPrefix(")") else { return nil }
//        substring = substring.dropFirst()
//        return (token: FunctionToken(identifier: identifier, arguments: arguments.map { $0.decomposed() }), consumedString: substring)
//    }
//}
//
//struct ParenthesizedExpressionToken: Token {
//    var expression: [Token]
//
//    init(_ expression: ExpressionToken) {
//        self.expression = expression.decomposed()
//    }
//
//    static func consume<T>(_ str: T) -> (token: Self, consumedString: Substring)? where T: StringProtocol, T.SubSequence == Substring {
//        guard str.hasPrefix("(") else { return nil }
//        var substring = str.dropFirst()
//
//        substring = substring.drop { $0.isWhitespace }
//
//        guard let (expression, consumedStr) = ExpressionToken.consume(substring) else {
//            return nil
//        }
//        substring = consumedStr
//
//        substring = substring.drop { $0.isWhitespace }
//
//        if consumedStr.hasPrefix(")") {
//            return (token: Self(expression), consumedString: substring.dropFirst())
//        }
//        return nil
//    }
//}
//
//struct PrimaryExpressionToken: Token {
//    var tokens: [Token]
//
//    static func consume<T>(_ str: T) -> (token: PrimaryExpressionToken, consumedString: Substring)? where T: StringProtocol, T.SubSequence == Substring {
//        let tokenTypes: [Token.Type] = [FunctionToken.self, NumberToken.self, ConstantToken.self, ParenthesizedExpressionToken.self]
//        for tokenType in tokenTypes {
//            if let (token, consumedStr) = tokenType.consume(str) {
//                return (token: PrimaryExpressionToken(tokens: [token]), consumedString: consumedStr)
//            }
//        }
//       return nil
//    }
//
//    func decomposed() -> [Token] {
//        return tokens.flatMap { $0.decomposed() }
//    }
//}
//
//struct PrefixExpressionToken: Token {
//    var prefixOperator: PrefixOperatorToken?
//    var primaryExpression: [Token]
//
//    init(prefixOperator: PrefixOperatorToken? = nil, primaryExpression: PrimaryExpressionToken) {
//        self.prefixOperator = prefixOperator
//        self.primaryExpression = primaryExpression.decomposed()
//    }
//    static func consume<T>(_ str: T) -> (token: PrefixExpressionToken, consumedString: Substring)? where T : StringProtocol, T.SubSequence == Substring {
//        var substring = str[...]
//        var prefixOperator: PrefixOperatorToken?
//        if let (prefixOperatorToken, consumedStr1) = PrefixOperatorToken.consume(substring) {
//            substring = consumedStr1
//            prefixOperator = prefixOperatorToken
//        }
//        guard let (primaryExpression, consumedStr2) = PrimaryExpressionToken.consume(substring) else {
//            return nil
//        }
//        return (token: PrefixExpressionToken(prefixOperator: prefixOperator, primaryExpression: primaryExpression), consumedString: consumedStr2)
//    }
//
//}
//
//struct BinaryExpressionToken: Token {
//    var binaryOperator: BinaryOperatorToken
//    var prefixExpression: PrefixExpressionToken
//
//    var tokens: [Token] {
//        return [binaryOperator, prefixExpression]
//    }
//
//    static func consume<T>(_ str: T) -> (token: BinaryExpressionToken, consumedString: Substring)? where T : StringProtocol, T.SubSequence == Substring {
//        var leftSpacing = 0
//        var idx = str.startIndex
//        while idx < str.endIndex, str[idx].isWhitespace {
//            leftSpacing += 1
//            idx = str.index(after: idx)
//        }
//        var substring = str.dropFirst(leftSpacing)
//        guard let (binaryOperator, remainingStr1) = BinaryOperatorToken.consume(substring) else {
//            return nil
//        }
//        substring = remainingStr1
//        idx = substring.startIndex
//        for _  in 0..<leftSpacing {
//            guard idx < str.endIndex, str[idx].isWhitespace else { return nil }
//            idx = str.index(after: idx)
//        }
//        guard idx < str.endIndex, !str[idx].isWhitespace else { return nil }
//        substring = substring.dropFirst(leftSpacing)
//
//        guard let (prefixExpression, remainingStr2) = PrefixExpressionToken.consume(substring) else {
//            return nil
//        }
//
//        return (token: BinaryExpressionToken(binaryOperator: binaryOperator, prefixExpression: prefixExpression), consumedString: remainingStr2)
//    }
//
//    func decomposed() -> [Token] {
//        return tokens.flatMap { $0.decomposed() }
//    }
//}
//struct ExpressionToken: Token {
//    var prefixExpression: PrefixExpressionToken
//    var binaryExpressions: [BinaryExpressionToken]
//
//    var tokens: [Token] {
//        return [prefixExpression] + binaryExpressions
//    }
//    static func consume<T>(_ str: T) -> (token: Self, consumedString: Substring)? where T: StringProtocol, T.SubSequence == Substring {
//        guard let (prefixExpression, remainingStr1) = PrefixExpressionToken.consume(str) else {
//            return nil
//        }
//        var substring = remainingStr1
//        var binaryExpressions = [BinaryExpressionToken]()
//        while let (binaryExpression, remainingStr2) = BinaryExpressionToken.consume(substring) {
//            binaryExpressions.append(binaryExpression)
//            substring = remainingStr2
//        }
//        return (token: ExpressionToken(prefixExpression: prefixExpression, binaryExpressions: binaryExpressions), consumedString: substring)
//    }
//
//    func decomposed() -> [Token] {
//        return tokens.flatMap { $0.decomposed() }
//    }
//}
//
//func tokenize(_ str: String) -> [Token] {
//    guard let (expression, remainingStr) = ExpressionToken.consume(str), remainingStr.isEmpty else {
//        fatalError("Could not parse input.")
//    }
//    return expression.decomposed()
//}
//
//print(tokenize(exp))

typealias Pattern = Tokenizable

//struct Foo {
//    let a = 3
//    let b = 4
//    let c = "hello"
//}
//
//
//let foo = Foo()
//let mirror = Mirror(reflecting: foo)
//
//print(mirror.displayStyle)
//
//for child in mirror.children {
//    print("Property name:", child.label)
//    print("Property value:", child.value)
//}

//TODO: Allow throwing functions
struct CharacterToken: Tokenizable {
    var condition: (Character) -> Bool
    init(_ condition: @escaping (Character) -> Bool) {
        self.condition = condition
    }

    func consumeNextToken<T>(from input: T) throws -> Substring? where T: StringProtocol, T.SubSequence == Substring {
        if let firstCharacter = input.first, condition(firstCharacter) {
            return input.dropFirst()
        }
        return nil
    }
}

extension KeyPath: Tokenizable where Root == Character, Value == Bool {
    func consumeNextToken<T>(from input: T) throws -> Substring? where T: StringProtocol, T.SubSequence == Substring {
        if let firstCharacter = input.first, firstCharacter[keyPath: self] {
            return input.dropFirst()
        }
        return nil
    }
}
@propertyWrapper
struct Token {
    var pattern: Pattern
    init(wrappedValue: Pattern) {
        self.pattern = wrappedValue
    }
    
    var wrappedValue: Pattern {
        get {
            return pattern
        }
        set {
            pattern = newValue
        }
    }
}
class LabelledToken: Tokenizable {
    var label: String
    var pattern: Pattern
    
    init(label: String, pattern: Pattern) {
        self.label = label
        self.pattern = pattern
    }
    
    func consumeNextToken<T>(from input: T) throws -> Substring? where T : StringProtocol, T.SubSequence == Substring {
        return try pattern.consumeNextToken(from: input)
    }
}
class MatchedToken: LabelledToken {
    var match: Substring
    
    init(label: String, pattern: Pattern, match: Substring) {
        self.match = match
        super.init(label: label, pattern: pattern)
    }
}
extension MatchedToken: CustomStringConvertible {
    var description: String {
        return label
    }
}

//TODO: Get rid of constant token in favour of identifier and derive constants in parser
protocol LexerProtocol: CaseIterable {
    var pattern: Pattern { get }
    var label: String { get }
}
extension LexerProtocol {
    var label: String {
        return String(describing: self)
    }
}
extension LexerProtocol where Self: RawRepresentable, RawValue == String {
    var label: String {
        return rawValue
    }
}
struct MatchedEnumToken<T> where T: LexerProtocol {
    var token: T
    var match: Substring
}
extension MatchedEnumToken: CustomStringConvertible {
    var description: String {
        return token.label
    }
}
extension LexerProtocol {

    static func tokenize(_ input: String) throws -> [MatchedEnumToken<Self>] {
        let tokens = self.allCases
        var substr = input[...]
        var matchedTokens = [MatchedEnumToken<Self>]()
        // Attempts to consume tokens until substr is empty, at which point the whole input has been tokenized
        searchLoop: while !substr.isEmpty {
            // Tries to consume each token from the start of substr
            for token in tokens {
                if let consumedString = try? token.pattern.consumeNextToken(from: substr) {
                    let matchedToken = MatchedEnumToken(
                        token: token,
                        match: substr[..<consumedString.startIndex]
                    )
                    substr = consumedString
                    matchedTokens.append(matchedToken)
                    continue searchLoop
                }
            }
            let failureIndex = substr.startIndex
            throw Lexer.Error.cannotMatchToken(source: input, position: failureIndex)
        }
        return matchedTokens
    }
}


enum LexerEnum: LexerProtocol {
    case left_parentheses
    case right_parentheses
    case number
    case operation
    case identifier
    case constant
    case whitespace
    
    private static let digit = "0" || "1" || "2" || "3" || "4" || "5" || "6" || "7" || "8" || "9"
    private static let digits = RecursiveToken { digit + $0.opt() }
    
    private static let letter = \Character.isLetter
    private static let identifier_head = letter || "_"
    private static let identifier_character = identifier_head || digit
    private static let identifier_characters = RecursiveToken { identifier_character + $0.opt() }
    
    private static let eulers_number = "e"
    private static let pi = "π" || "pi"
    
    var pattern: Pattern {
        switch self {
        case .left_parentheses: return "("
        case .right_parentheses: return ")"
        case .number: return LexerEnum.digits + ("." + LexerEnum.digits).opt()
        case .operation: return "+" || "-" || "*" || "/" || "^"
        case .identifier: return LexerEnum.identifier_head + LexerEnum.identifier_characters.opt()
        case .constant: return LexerEnum.eulers_number || LexerEnum.pi
        case .whitespace: return CharacterToken(\.isWhitespace)
        }
    }
}
class Lexer {
    @Token var left_parentheses = "("
    @Token var right_parentheses = ")"
    
    static let digit = "0" || "1" || "2" || "3" || "4" || "5" || "6" || "7" || "8" || "9"
    static let digits = RecursiveToken { digit + $0.opt() }
    
    @Token var number = Lexer.digits + ("." + Lexer.digits).opt()
    @Token var operation = "+" || "-" || "*" || "/" || "^"
    
    static let letter = \Character.isLetter
    static let identifier_head = letter || "_"
    static let identifier_character = identifier_head || digit
    static let identifier_characters = RecursiveToken { identifier_character + $0.opt() }
    
    @Token var identifier = Lexer.identifier_head + identifier_characters.opt()
    
    static let eulers_number = "e"
    static let pi = "π" || "pi"
    
    @Token var constant = Lexer.eulers_number || Lexer.pi
    
//    @Token var variable = "x"
    
    @Token var whitespace = CharacterToken(\.isWhitespace)
    internal init () {
        
    }
    
    enum Error: Swift.Error {
        case cannotMatchToken(source: String, position: String.Index)
        
        var localizedDescription: String {
            switch self {
            case let .cannotMatchToken(source, position):
                let positionOffset = source.distance(from: source.startIndex, to: position)
                var errorMessage = "error: tokenization failed; could not match any tokens to the string at a position."
                errorMessage += "\n\t" + source
                errorMessage += "\n\t" + String(repeating: " ", count: positionOffset) + "^"
                return errorMessage
            }
        }
    }
    
    static var availableTokens: [LabelledToken] {
        let lexer = Lexer()
        let mirror = Mirror(reflecting: lexer)
        let children = mirror.children
        var arr = [LabelledToken]()
        for child in children {
            print(child.value)
            guard let token = child.value as? Token else {
                continue
            }
            guard let label = child.label, label.hasPrefix("_") else {
                fatalError("Child either does not have a label or its underlying name isn't prefixed with an underscore.")
            }
            arr.append(LabelledToken(label: "\(label.dropFirst())", pattern: token.pattern))
        }
        return arr
    }
    
    static func tokenize(_ input: String) throws -> [MatchedToken] {
        let tokens = availableTokens
        var substr = input[...]
        var matchedTokens = [MatchedToken]()
        // Attempts to consume tokens until substr is empty, at which point the whole input has been tokenized
        searchLoop: while !substr.isEmpty {
            // Tries to consume each token from the start of substr
            for token in tokens {
                if let consumedString = try? token.pattern.consumeNextToken(from: substr) {
                    let matchedToken = MatchedToken(
                        label: token.label,
                        pattern: token.pattern,
                        match: substr[..<consumedString.startIndex]
                    )
                    substr = consumedString
                    matchedTokens.append(matchedToken)
                    continue searchLoop
                }
            }
            let failureIndex = substr.startIndex
            throw Lexer.Error.cannotMatchToken(source: input, position: failureIndex)
        }
        return matchedTokens
    }
}
//let mirror = Mirror(reflecting: Lexer())
//
//print(mirror.displayStyle)
//
//
//for child in mirror.children {
//    guard child.value is Token else { continue }
//    print("Property name:", child.label)
//    print("Property value:", child.value)
//}
//
//let children = mirror.children
//let tokens = children
//    .filter { $0.value is Token }
//    .map { (label: $0.label!.dropFirst(), pattern: ($0.value as! Token).pattern) }
//let str = "(isdoif"
//let consumingTokens = tokens.filter { token in
//    if let substr = try? token.pattern.consumeNextToken(from: str) {
//        return true
//    } else {
//        return false
//    }
//}
//
//consumingTokens.forEach {
//    print($0.label + ":", $0.pattern)
//}

Lexer.availableTokens.forEach {
    print($0.label + ":", $0.pattern)
}


do {
    let tokens = try Lexer.tokenize(exp)
    print(tokens)
} catch let error as Lexer.Error {
    print(error.localizedDescription)
}

do {
    let tokens = try LexerEnum.tokenize(exp)
    print(tokens)
} catch let error as Lexer.Error {
    print(error.localizedDescription)
}

print(Mirror(reflecting: LexerEnum.left_parentheses).children.first?.label ?? String(describing: LexerEnum.left_parentheses))


/**
 # NOTES
 
 - After parse tree is constructed by an instance of Tokenizer, provide a SemanticAnalyzer class that can be given a set of rules to use when recursively analyzing the syntax tree
 - Consider making the provided tokens' initializers private
 - Instead of throwing errors during the semantic analysis (Sema) phase, make it return a `Result<syntax-tree, SemaError>` or something.
 - Also, the rules can return a `Result<Never, SemaError>`
 - Maybe I can make users define these patterns in a class like ArgumentParser and have `@Token` property wrappers use enclosing-self to register the tokens or reflection
 */

