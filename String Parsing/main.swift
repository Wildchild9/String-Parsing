//
//  main.swift
//  String Parsing
//
//  Created by Noah Wilder on 2020-05-14.
//  Copyright © 2020 Noah Wilder. All rights reserved.
//

import Foundation


//TODO: Get rid of constant token in favour of identifier and derive constants in parser

//let prefix_operator = "+" || "-"
//
//let digit = "0" || "1" || "2" || "3" || "4" || "5" || "6" || "7" || "8" || "9"
//let digits = RecursivePattern { digit + $0.opt() }
//
//let number = digits + ("." + digits).opt()
//
//let spacing = RecursivePattern { " " + $0.opt() }
//
//let expression = SharedPattern()
//let expression_list = RecursivePattern { expression || (spacing.opt() + "," + spacing.opt() + $0) }
//
//let binary_operator = "+" || "-" || "*" || "/" || "^"
//let spaced_binary_operator = RecursivePattern { binary_operator || (" " + $0 + " ") }
//
//let lowercase_latin_character = "a" || "b" || "c" || "d" || "e" || "f" || "g" || "h" || "i" || "j" || "k" || "l" || "m" || "n" || "o" || "p" || "q" || "r" || "s" || "t" || "u" || "v" || "w" || "x" || "y" || "z"
//let uppercase_latin_character = "A" || "B" || "C" || "D" || "E" || "F" || "G" || "H" || "I" || "J" || "K" || "L" || "M" || "N" || "O" || "P" || "Q" || "R" || "S" || "T" || "U" || "V" || "W" || "X" || "Y" || "Z"
//let latin_character = lowercase_latin_character || uppercase_latin_character
//
//let identifier = RecursivePattern { latin_character + $0.opt() }
//
//let parenthesized_expression = "(" + spacing.opt() + expression + spacing.opt() + ")"
//
//let unary_function_expression =  identifier + parenthesized_expression
//let temp1 = identifier + "(" + spacing.opt()
//let temp2 = temp1 + expression + spacing.opt() + ","
//let binary_function_expression = temp2 + spacing.opt() + expression + spacing.opt() + ")"
//
//let function_expression = unary_function_expression || binary_function_expression
//
//let eulers_number = "e"
//let pi = "π" || "pi"
//
//let constant = prefix_operator.opt() + (eulers_number || pi)
//
//let primary_expression = function_expression || number || constant || parenthesized_expression
//
//let prefix_expression = prefix_operator.opt() + primary_expression
//
//let binary_expression = spaced_binary_operator + prefix_expression
//let binary_expressions = RecursivePattern { binary_expression + $0.opt() }
//
//expression.pattern = prefix_expression + binary_expressions.opt()




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



enum TokenClassification: Parsable, CaseIterable {
    typealias TokenType = Self


    case left_parenthesis
    case right_parenthesis
    case number
    case binary_operator
    case prefix_operator
    case postfix_operator
    case identifier
    case whitespace
    case comma_separator

    private static let digit = "0" || "1" || "2" || "3" || "4" || "5" || "6" || "7" || "8" || "9"
    private static let digits = RecursivePattern { digit + $0.opt() }

    private static let letter = \Character.isLetter
    private static let identifier_head = letter || "_"
    private static let identifier_character = identifier_head || digit
    private static let identifier_characters = RecursivePattern { identifier_character + $0.opt() }

    static var grammarPattern: AnyGrammarPattern<Self> {
        let whitespaces = RecursiveGrammarPattern { whitespace + $0.opt() }
        let expression = SharedGrammarPattern<Self>()
        let expression_list = RecursiveGrammarPattern { expression + (whitespaces.opt() + comma_separator + whitespaces.opt() + $0).opt() }

//        let function1 = identifier + whitespaces.opt()
//        let function2 = left_parenthesis + whitespaces.opt() + expression_list + whitespaces.opt() + right_parenthesis
//        let function = function1 + function2

        let parenthesized_expression = left_parenthesis + whitespaces.opt() + expression + whitespace.opt() + right_parenthesis
        // let primary_expression = number || function || identifier || parenthesized_expression
        let function_call = left_parenthesis + whitespaces.opt() + expression_list + whitespaces.opt() + right_parenthesis
        let primary_expression = number || (identifier + function_call.opt()) || parenthesized_expression

//        let postfix_expression = primary_expression //+ postfix_operator.opt()

        let prefix_expression = prefix_operator.opt() + primary_expression //postfix_expression

        let spaced_binary_operator = whitespaces + binary_operator + whitespaces
        let binary_expression = (postfix_operator.opt() + spaced_binary_operator + prefix_expression) || (binary_operator + primary_expression)
//        let binary_expression = ((postfix_operator.opt() + whitespaces + binary_operator + whitespaces) || binary_operator) + prefix_expression
        //(binary_operator || (postfix_operator.opt() + whitespaces + binary_operator + whitespaces)) + prefix_expression
        let binary_expressions = RecursiveGrammarPattern { binary_expression + $0.opt() }

        expression.pattern = AnyGrammarPattern(AnyGrammarPattern(prefix_expression + binary_expressions.opt()) + postfix_operator.opt())

        return AnyGrammarPattern(AnyGrammarPattern(whitespaces.opt() + expression) + whitespaces.opt())
    }

    var pattern: Pattern {
        switch self {
        case .left_parenthesis: return "("
        case .right_parenthesis: return ")"
        case .number: return Self.digits + ("." + Self.digits).opt()
        case .binary_operator: return "+" || "-" || "*" || "/" || "^"
        case .prefix_operator: return "+" || "-"
        case .postfix_operator: return "++" || "--"
        case .identifier: return Self.identifier_head + Self.identifier_characters.opt()
        case .whitespace: return \Character.isWhitespace
        case .comma_separator: return ","
        }
    }
}

struct Function: Hashable {
    var identifier: String
    var arity: UInt
    var apply: ([Double]) -> Double
    //TODO: Allow function of any arity

    init(_ identifier: String, arity: UInt, function: @escaping ([Double]) -> Double) {
        self.identifier = identifier
        self.arity = arity
        self.apply = function
    }
    func evaluate(with arguments: [Double]) -> Double {
        guard arguments.count == arity else { fatalError("The length of the arguments must be equal to the arity of the function.") }
        return apply(arguments)
    }
    static func ==(lhs: Function, rhs: Function) -> Bool {
        return lhs.arity == rhs.arity && lhs.identifier == rhs.identifier
    }
    func hash(into hasher: inout Hasher) {
        hasher.combine(arity)
        hasher.combine(identifier)
    }
}
class Constant: Hashable {
    var identifier: String
    var value: Double

    init(_ identifier: String, value: Double) {
        self.identifier = identifier
        self.value = value
    }
    static func ==(lhs: Constant, rhs: Constant) -> Bool {
        return lhs.identifier == rhs.identifier
    }
    func hash(into hasher: inout Hasher) {
        hasher.combine(identifier)
    }
}
struct PrefixOperator: Hashable {
    var identifier: String
    var apply: (Double) -> Double

    init(_ identifier: String, function: @escaping (Double) -> Double) {
        self.identifier = identifier
        self.apply = function
    }
    static func ==(lhs: Self, rhs: Self) -> Bool {
        return lhs.identifier == rhs.identifier
    }
    func hash(into hasher: inout Hasher) {
        hasher.combine(identifier)
    }
}
struct PostfixOperator: Hashable {
    var identifier: String
    var apply: (Double) -> Double

    init(_ identifier: String, function: @escaping (Double) -> Double) {
        self.identifier = identifier
        self.apply = function
    }
    static func ==(lhs: Self, rhs: Self) -> Bool {
        return lhs.identifier == rhs.identifier
    }
    func hash(into hasher: inout Hasher) {
        hasher.combine(identifier)
    }
}
struct InfixOperator: Hashable {
    var identifier: String
    var associativity: Associativity = .left
    var precedence: Double = 0
    var apply: (Double, Double) -> Double

    enum Associativity {
        case left, right
    }

    init(_ identifier: String, associativity: Associativity = .left, precedence: Double = 0, function: @escaping (Double, Double) -> Double) {
        self.identifier = identifier
        self.associativity = associativity
        self.precedence = precedence
        self.apply = function
    }
    static func ==(lhs: Self, rhs: Self) -> Bool {
        return lhs.identifier == rhs.identifier
    }
    func hash(into hasher: inout Hasher) {
        hasher.combine(identifier)
    }
}

final class LookupTable {
    private(set) static var functions = Set<Function>()
    private(set) static var constants = Set<Constant>()
    private(set) static var infixOperators = Set<InfixOperator>()
    private(set) static var prefixOperators = Set<PrefixOperator>()
    private(set) static var postfixOperators = Set<PostfixOperator>()

    static func define(function: Function) throws {
        guard functions.insert(function).inserted else {
            throw LookupTable.Error.conflictsWithPreviousDefinition
        }
    }
    static func define(constant: Constant) throws {
        guard constants.insert(constant).inserted else {
            throw LookupTable.Error.conflictsWithPreviousDefinition
        }
    }
    static func define(infixOperator: InfixOperator) throws {
        guard infixOperators.insert(infixOperator).inserted else {
            throw LookupTable.Error.conflictsWithPreviousDefinition
        }
    }
    static func define(prefixOperator: PrefixOperator) throws {
        guard prefixOperators.insert(prefixOperator).inserted else {
            throw LookupTable.Error.conflictsWithPreviousDefinition
        }
    }
    static func define(postfixOperator: PostfixOperator) throws {
        guard postfixOperators.insert(postfixOperator).inserted else {
            throw LookupTable.Error.conflictsWithPreviousDefinition
        }
    }

    static func lookupPrefixOperator(identifier: String) throws -> PrefixOperator {
        guard let op = prefixOperators.first(where: { $0.identifier == identifier }) else {
            throw Error.undefinedPrefixOperator
        }
        return op
    }
    static func lookupPostfixOperator(identifier: String) throws -> PostfixOperator {
        guard let op = postfixOperators.first(where: { $0.identifier == identifier }) else {
            throw Error.undefinedPostfixOperator
        }
        return op
    }
    static func lookupInfixOperator(identifier: String) throws -> InfixOperator {
        guard let op = infixOperators.first(where: { $0.identifier == identifier }) else {
            throw Error.undefinedInfixOperator
        }
        return op
    }
    static func lookupConstant(identifier: String) throws -> Constant {
        guard let constant = constants.first(where: { $0.identifier == identifier }) else {
            throw Error.undefinedConstant
        }
        return constant
    }
    static func lookupFunction(identifier: String, arity: UInt) throws -> Function {
        guard let function = functions.first(where: { $0.identifier == identifier && $0.arity == arity }) else {
            throw Error.undefinedFunction
        }
        return function
    }
    static func lookupFunctions(identifier: String) throws -> [Function] {
        let matchingFunctions = functions.filter { $0.identifier == identifier }
        guard !matchingFunctions.isEmpty else {
            throw Error.undefinedFunction
        }
        return matchingFunctions.sorted { $0.arity < $1.arity }
    }

    static func updateConstant(withIdentifier identifier: String, to newValue: Double) throws {
        guard let constant = constants.first(where: { $0.identifier == identifier }) else {
            throw Error.undefinedConstant
        }
        constant.value = newValue
    }

    enum Error: Swift.Error {
        case conflictsWithPreviousDefinition
        case undefinedPrefixOperator
        case undefinedInfixOperator
        case undefinedPostfixOperator
        case undefinedConstant
        case undefinedFunction
        case value
    }
}

try LookupTable.define(infixOperator: .init("+", precedence: 0) { $0 + $1 })
try LookupTable.define(infixOperator: .init("-", precedence: 0) { $0 - $1 })
try LookupTable.define(infixOperator: .init("*", precedence: 100) { $0 * $1 })
try LookupTable.define(infixOperator: .init("/", precedence: 100) { $0 / $1 })
try LookupTable.define(infixOperator: .init("^", associativity: .right, precedence: 200, function: pow))

try LookupTable.define(prefixOperator: .init("-") { -$0 })
try LookupTable.define(prefixOperator: .init("+") { $0 })

try LookupTable.define(constant: .init("pi", value: .pi))
try LookupTable.define(constant: .init("π", value: .pi))
try LookupTable.define(constant: .init("e", value: M_E))

try LookupTable.define(function: .init("cos", arity: 1) { cos($0[0]) })
try LookupTable.define(function: .init("sin", arity: 1) { sin($0[0]) })
try LookupTable.define(function: .init("tan", arity: 1) { tan($0[0]) })
try LookupTable.define(function: .init("acos", arity: 1) { acos($0[0]) })
try LookupTable.define(function: .init("asin", arity: 1) { asin($0[0]) })
try LookupTable.define(function: .init("atan", arity: 1) { atan($0[0]) })
try LookupTable.define(function: .init("sqrt", arity: 1) { sqrt($0[0]) })
try LookupTable.define(function: .init("cbrt", arity: 1) { cbrt($0[0]) })
try LookupTable.define(function: .init("log", arity: 1) { log10($0[0]) })
try LookupTable.define(function: .init("log2", arity: 1) { log2($0[0]) })
try LookupTable.define(function: .init("ln", arity: 1) { log($0[0]) })
try LookupTable.define(function: .init("exp", arity: 1) { Darwin.exp($0[0]) })


enum Expression {
    indirect case infixOperator(InfixOperator, lhs: Self, rhs: Self)
    indirect case postfixOperator(PostfixOperator, Self)
    indirect case prefixOperator(PrefixOperator, Self)
    indirect case function(Function, arguments: [Self])
    case constant(Constant)
    case number(Double)

    static let zero: Self = .number(.zero)

    func evaluated() -> Double {
        switch self {
        case let .infixOperator(op, lhs: lhs, rhs: rhs):
            return op.apply(lhs.evaluated(), rhs.evaluated())
        case let .prefixOperator(op, exp):
            return op.apply(exp.evaluated())
        case let .postfixOperator(op, exp):
            return op.apply(exp.evaluated())
        case let .function(function, arguments: args):
            return function.apply(args.map { $0.evaluated() })
        case let .constant(constant):
            return constant.value
        case let .number(n):
            return n
        }
    }
}

typealias Token = MatchedToken<TokenClassification>

enum SemaError: Error {
    case infixOperatorSpacing
    case invalidSpacing
    case invalidOperator
    case invalidSeparator
    case functionDoesNotExist(identifier: String, arity: Int? = nil)
    case missingTokens
    case functionDoesNotTerminate(identifier: String)
    case constantDoesNotExist(identifier: String)
    case invalidToken(Token, output: [Token], stack: [Token], idx: Array<Token>.Index)
    case unbalancedParentheses
}

// Puts the tokens into postfix notation
func postfixTokens(from tokens: [Token]) throws -> (tokens: [Token], constantsAndFunctions: [Either<Constant, Function>]) {
    var stack = [Token]()
    var output = [Token]()
    let tokens = tokens.filter { $0.classification != .whitespace }
    var idx = tokens.startIndex
    var constantsAndFunctions = [Either<Constant, Function>]()
//    var leftParensCount = 0
//    var rightParensCount = 0

    while idx < tokens.endIndex {
//        let lastToken = idx - 1 >= tokens.startIndex ? tokens[idx - 1] : nil
        let token = tokens[idx]
        let nextToken = idx + 1 < tokens.endIndex ? tokens[idx + 1] : nil

        mainTokenClassification:
        switch token.classification {

        case .whitespace:
            break

        case .comma_separator:
            throw SemaError.invalidSeparator

        case .number:
            output.append(token)
            if nextToken?.classification != .postfix_operator {
                while stack.last?.classification == .prefix_operator {
                    output.append(stack.removeLast())
                }
            }


        case .left_parenthesis:
            stack.append(token)

        case .prefix_operator:
            stack.append(token)

        case .binary_operator:
            guard let currentOpertor = try? LookupTable.lookupInfixOperator(identifier: String(token.match)) else {
                throw SemaError.invalidOperator
            }
            while let operatorToken = stack.last, operatorToken.classification == .binary_operator, let op = try? LookupTable.lookupInfixOperator(identifier: String(operatorToken.match)), op.precedence > currentOpertor.precedence {
                stack.removeLast()
                output.append(operatorToken)
            }
            stack.append(token)

        case .postfix_operator:
            output.append(Token(classification: .postfix_operator, match: token.match))
            if nextToken?.classification != .postfix_operator {
                while stack.last?.classification == .prefix_operator {
                    output.append(stack.removeLast())
                }
            }

        // Matches functions
        case .identifier where nextToken?.classification == .left_parenthesis:
            let identifier = String(token.match)
            guard let functions = try? LookupTable.lookupFunctions(identifier: identifier) else {
                throw SemaError.functionDoesNotExist(identifier: identifier)
            }
            var depth = 1
            // Ensure there are tokens after the left parentheses
            guard idx + 2 < tokens.endIndex else {
                throw SemaError.missingTokens
            }
            var arguments = [[Token]]()
            var currentArgument = [Token]()

            for (i, currentToken) in tokens[(idx + 2)...].enumerated() {
                switch currentToken.classification {
                case .left_parenthesis:
                    depth += 1
                    currentArgument.append(currentToken)

                case .right_parenthesis where depth == 1:
                    // Adds to the output the function in postfix notation
                    let argument = try postfixTokens(from: currentArgument)
                    constantsAndFunctions.append(contentsOf: argument.constantsAndFunctions)
                    arguments.append(argument.tokens)
                    output.append(contentsOf: arguments.flatMap { $0 })
                    output.append(token)
                    guard let function = functions.first(where: { $0.arity == arguments.count }) else {
                        throw SemaError.functionDoesNotExist(identifier: identifier, arity: arguments.count)
                    }
                    constantsAndFunctions.append(.right(function))
                    idx += 2 + i
                    if idx + 1 < tokens.endIndex, tokens[idx + 1].classification != .postfix_operator {
                        while stack.last?.classification == .prefix_operator {
                            output.append(stack.removeLast())
                        }
                    }
                    break mainTokenClassification

                case .right_parenthesis:
                    depth -= 1
                    currentArgument.append(currentToken)

                case .comma_separator where depth == 1:
                    let argument = try postfixTokens(from: currentArgument)
                    arguments.append(argument.tokens)
                    constantsAndFunctions.append(contentsOf: argument.constantsAndFunctions)
                    currentArgument.removeAll()

                default:
                    currentArgument.append(currentToken)
                }
            }

            throw SemaError.functionDoesNotTerminate(identifier: identifier)

        case .identifier:
            let identifier = String(token.match)
            guard let constant = try? LookupTable.lookupConstant(identifier: identifier) else {
                throw SemaError.constantDoesNotExist(identifier: identifier)
            }
            output.append(token)
            constantsAndFunctions.append(.left(constant))
            if nextToken?.classification != .postfix_operator {
                while stack.last?.classification == .prefix_operator {
                    output.append(stack.removeLast())
                }
            }

        case .right_parenthesis:
            for stackToken in stack.reversed() {
                if stackToken.classification == .left_parenthesis {
                    stack.removeLast()
                    if nextToken?.classification != .postfix_operator {
                        while stack.last?.classification == .prefix_operator {
                            output.append(stack.removeLast())
                        }
                    }
                    break mainTokenClassification
                }
                output.append(stackToken)
            }
            throw SemaError.unbalancedParentheses

        }

        // Increment token index
        idx += 1

    }
    while !stack.isEmpty {
        output.append(stack.removeLast())
    }
    return (tokens: output, constantsAndFunctions)
}

public enum Either<A, B> {
    case left(A)
    case right(B)
}

func createAST(from postfixData: (tokens: [Token], constantsAndFunctions: [Either<Constant, Function>])) throws -> Expression {
    let (tokens, constantsAndFunctions) = postfixData
    guard let lastToken = tokens.last else {
        fatalError()
    }
    guard tokens.count > 1 else {
        let match = String(lastToken.match)
        switch lastToken.classification {
        case .identifier:
            guard case let .left(constant) = constantsAndFunctions.first!, constant.identifier == match else {
                fatalError()
            }
            return .constant(constant)
        case .number:
            return .number(Double(match)!)
        default:
            fatalError("Invalid token for single token expression.")
        }
    }

    var identifierIterator = constantsAndFunctions.makeIterator()
    var arr = [Either<Expression, Token>]()
    arr = tokens.map { .right($0) }
    var i = 0
    while i != arr.endIndex {
        guard case let .right(element) = arr[i] else { fatalError() }
        let match = String(element.match)
        switch element.classification {
        case .left_parenthesis, .right_parenthesis, .whitespace, .comma_separator:
            fatalError("This token should")
        case .number:
            arr[i] = .left(.number(Double(match)!))
            i += 1
        case .binary_operator:
            guard i >= 2, case let .left(lhs) = arr[i - 1], case let .left(rhs) = arr[i - 2] else {
                fatalError("Invalid binary operator token position.")
            }
            let infixOperator = try LookupTable.lookupInfixOperator(identifier: match)
            arr[(i - 2)...i] = [.left(.infixOperator(infixOperator, lhs: rhs, rhs: lhs))]
            i -= 1

        case .prefix_operator:
            guard i >= 1, case let .left(expression) = arr[i - 1] else {
                fatalError("Invalid prefix operator token position.")
            }
            let prefixOperator = try LookupTable.lookupPrefixOperator(identifier: match)
            arr[(i - 1)...i] = [.left(.prefixOperator(prefixOperator, expression))]

        case .postfix_operator:
            guard i >= 1, case let .left(expression) = arr[i - 1] else {
                fatalError("Invalid postfix operator token position.")
            }
            let postfixOperator = try LookupTable.lookupPostfixOperator(identifier: match)
            arr[(i - 1)...i] = [.left(.postfixOperator(postfixOperator, expression))]

        case .identifier:
            guard let identifier = identifierIterator.next() else {
                fatalError()
            }
            // If this is true, the identifier is a constant
            switch identifier {
            case let .left(constant):
                guard constant.identifier == match else { fatalError() }
                arr[i] = .left(.constant(constant))
                i += 1
            case let .right(function):
                guard function.identifier == match else { fatalError() }
                let arguments = arr[(i - Int(function.arity))..<i].map { element -> Expression in
                    guard case let .left(expression) = element else { fatalError() }
                    return expression
                }
                arr[(i - Int(function.arity))...i] = [.left(.function(function, arguments: arguments))]
                i -= Int(function.arity)
                i += 1
            }
        }

    }
    guard arr.count == 1, case let .left(expression) = arr[0] else { fatalError() }
    return expression

    // Crash if token type is a parentheses
}

extension Expression {
    init(_ string: String) throws {
        do {
            let tokenizer = Tokenizer(using: TokenClassification.self)
            let tokens = try tokenizer.tokenize(string)
            let output = try postfixTokens(from: tokens)
            let expression = try createAST(from: output)
            self = expression
        } catch let error as ParseError {
            print(error.localizedDescription)
            fatalError("Could not parse string to `Expression`.")
        } catch {
            fatalError("Could not parse string to `Expression`.")
        }
    }
}

extension Expression: CustomStringConvertible {
    private var _description: String {
        switch self {
        case let .infixOperator(infixOperator, lhs, rhs):
            return "(\(lhs._description) \(infixOperator.identifier) \(rhs._description))"

        case let .postfixOperator(postfixOperator, expression):
            return expression._description + postfixOperator.identifier//"(" + expression._description + postfixOperator.identifier + ")"

        case let .prefixOperator(prefixOperator, expression):
            return prefixOperator.identifier + expression._description//"(" + prefixOperator.identifier + expression._description + ")"

        case let .function(function, arguments):
            return function.identifier + "(" + arguments.lazy.map(\.description).joined(separator: ", ") + ")"

        case let .constant(constant):
            return constant.identifier

        case let .number(number):
            return "\(number)"
        }
    }
    var description: String {
        return self._description.replacingOccurrences(of: "^\\((.+)\\)$", with: "$1", options: .regularExpression)
    }
}


var exp1 = try Expression("+cos(π / 4) + 4") //"+((4) + 10 - 7) - (-64.29434) - 7 ^ 4 + cos(180 - 493 + sin(20))"
print(exp1)
//let tokenizer = Tokenizer(using: TokenClassification.self)
//var tokens: [MatchedToken<TokenClassification>]
//do {
//    tokens = try tokenizer.tokenize(exp1)
//    print(tokens)
//    let output = try postfixTokens(from: tokens)
//    tokens = output.tokens
//    print(tokens)
//    print(tokens.lazy.filter { $0.classification != .whitespace }.map { String($0.match) }.joined(separator: " "))
//    let expression = try createAST(from: output)
//    print(expression)
//    print(expression.evaluated())
//} catch let error as ParseError {
//    print(error.localizedDescription)
//    exit(0)
//}
print(exp1.evaluated())

try LookupTable.define(constant: .init("x", value: 0))
var exp = try Expression("x ^ 2")
for x in 0...10 {
    try LookupTable.updateConstant(withIdentifier: "x", to: Double(x))
    print("\(x) ^ 2 = \(exp.evaluated())")
}

try LookupTable.define(postfixOperator: .init("--") { $0 - 1 })

var exp3 = try Expression("cos(0)-- + 3")
print(exp3.evaluated())
postfix operator --
postfix func --(value: Int) -> Int {
    return value - 1
}



//enum ChemToken: Parsable, CaseIterable {
//
//    typealias TokenType = ChemToken
//    case number
//    case element
//    case left_parenthesis
//    case right_parenthesis
//    case arrow
//    case whitespace
//    case plus
//
//    private static let digits = RegexPattern("\\d+")
//    private static let uppercase_letter = RegexPattern("[A-Z]")
//    private static let lowercase_letter = RegexPattern("[a-z]")
//
//    var pattern: Pattern {
//        switch self {
//        case .number:
//            return Self.digits
//        case .element:
//            return Self.uppercase_letter + Self.lowercase_letter.opt()
//        case .left_parenthesis:
//            return "("
//        case .right_parenthesis:
//            return ")"
//        case .arrow:
//            return "->" || "=>" || "→"
//        case .whitespace:
//            return \Character.isWhitespace
//        case .plus:
//            return "+"
//        }
//    }
//
//    static var grammarPattern: AnyGrammarPattern<ChemToken> {
//        let whitespaces = RecursiveGrammarPattern { whitespace + $0.opt() }
//
//        let numbered_element = element + number.opt()
//        let elements = RecursiveGrammarPattern { numbered_element + $0.opt() }
//
//        let bracketed_molecule = left_parenthesis + numbered_element + elements + right_parenthesis + number
//        let molecule_component = numbered_element || bracketed_molecule
//        let molecule = RecursiveGrammarPattern { molecule_component + $0.opt() }
//
//        let reaction_side = RecursiveGrammarPattern { molecule + whitespaces.opt() + (plus + whitespaces.opt() + $0).opt() }
//        let reaction = whitespaces.opt() + reaction_side + arrow + whitespaces.opt() + reaction_side
//
//        return AnyGrammarPattern(reaction)
//    }
//}
//
//
//var chemicalEquation = "C2(OH)10 + O2 -> CO2 + CO + H2O"
//var tokenizer = Tokenizer(using: ChemToken.self)
//var tokens = [MatchedToken<ChemToken>]()
//print(chemicalEquation)
//do {
//    tokens = try tokenizer.tokenize(chemicalEquation)
//} catch let error as ParseError {
//    print(error.localizedDescription)
//} catch {
//    print(error.localizedDescription)
//}
//
//
//var reactionSides = tokens.filter { $0.classification != .whitespace }.split { $0.classification == .arrow }
//var lhsTokens = reactionSides[0]
//var rhsTokens = reactionSides[1]
//var reactants = lhsTokens.split { $0.classification == .plus }
//var products = rhsTokens.split { $0.classification == .plus }
//print(reactants.map { $0.map { String($0.match) }.joined() })
//print(products.map { $0.map { String($0.match) }.joined() })
//
//print(tokens)


enum XToken: Parsable, CaseIterable {
    typealias TokenType = Self
//    case a
//    case ab
//    case bc
//    case bcc
//    case ccd
//    case cdd
//
//    var pattern: Pattern {
//        switch self {
//        case .a: return "a"
//        case .ab: return "ab"
//        case .bc: return "bc"
//        case .bcc: return "bcc"
//        case .ccd: return "ccd"
//        case .cdd: return "cdd"
//        }
//    }
//
//    static var grammarPattern: AnyGrammarPattern<XToken> {
//        return (a || ab) + (bc || bcc) + (ccd || cdd)
//    }
    case a
    case b
    case c
    case d

    static let pattern = "a" + ("b" || ("b" + "b")) + "c"
    var pattern: Pattern {
        switch self {
        case .a: return "a"//Self.pattern
        case .b: return "b"
        case .c: return "c"
        case .d: return "d"
        }
    }

    /// #Should match:
    ///   - `abbc`
    ///   - `abc`
    static var grammarPattern: AnyGrammarPattern<XToken> {
//        let temp1 = a + b.opt()
//        let grammar = temp1 + b + c
//        let bb = b + b
//        print(type(of: a + (b || bb) + c))
//        return AnyGrammarPattern<XToken>(a + (b || bb) + c)
        let ab = a + b
        let bc = b + c
        let bcc = bc + c
        let cc = c + c
        let ccd = cc + d
        let cd = c + d
        let cdd = cd + d
        let pattern = (a || ab) + (bc || bcc) + (ccd || cdd)
        print(str)
        return AnyGrammarPattern<XToken>(pattern)
    }
}

print("\n\n")
var str = "abbcccdd"//"abccccd"
print(str)
var tokenizer = Tokenizer(using: XToken.self)
var tokens = [MatchedToken<XToken>]()
do {
    tokens = try tokenizer.tokenize(str)
    print(tokens)
} catch let error as ParseError {
    print(error.localizedDescription)
} catch {
    print(error.localizedDescription)
}

