//
//  main.swift
//  String Parsing
//
//  Created by Noah Wilder on 2020-05-14.
//  Copyright © 2020 Noah Wilder. All rights reserved.
//

import Foundation

enum TokenClassification: Parsable, CaseIterable {
    typealias Base = Self

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
    private static let digits = RecursiveTokenPattern { digit + $0.opt() }

    private static let letter = \Character.isLetter
    private static let identifier_head = letter || "_"
    private static let identifier_character = identifier_head || RegexTokenPattern("\\d")//digit
    private static let identifier_characters = RecursiveTokenPattern { identifier_character + $0.opt() }

    var pattern: TokenPattern {
        switch self {
        case .left_parenthesis: return "("
        case .right_parenthesis: return ")"
        case .number: return RegexTokenPattern("\\d+(.\\d+)?")//Self.digits + ("." + Self.digits).opt()
        case .binary_operator: return "+" || "-" || "*" || "/" || "^"
        case .prefix_operator: return "+" || "-"
        case .postfix_operator: return "++" || "--"
        case .identifier: return Self.identifier_head + Self.identifier_characters.opt()
        case .whitespace: return \Character.isWhitespace
        case .comma_separator: return ","
        }
    }
    
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
        let primary_expression = number || ((identifier + function_call.opt()) || parenthesized_expression)

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
}

struct Function: Hashable {
    var identifier: String
    var arity: UInt
    var apply: ([Double]) -> Double

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
try LookupTable.define(constant: .init("g", value: 9.81))

try LookupTable.define(function: .init("cos", arity: 1) { cos($0[0]) })
try LookupTable.define(function: .init("sin", arity: 1) { sin($0[0]) })
try LookupTable.define(function: .init("tan", arity: 1) { tan($0[0]) })
try LookupTable.define(function: .init("sec", arity: 1) { 1 / cos($0[0]) })
try LookupTable.define(function: .init("csc", arity: 1) { 1 / sin($0[0]) })
try LookupTable.define(function: .init("cot", arity: 1) { 1 / tan($0[0]) })
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

    while idx < tokens.endIndex {
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
            while let operatorToken = stack.last, operatorToken.classification == .binary_operator, let op = try? LookupTable.lookupInfixOperator(identifier: String(operatorToken.match)), op.precedence >= currentOpertor.precedence {
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
                stack.removeLast()
                if stackToken.classification == .left_parenthesis {
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
            fatalError("This token should not be present in this stage.")
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
            fatalError("Could not parse string to `Expression`")
        } catch {
            fatalError("Could not parse string to `Expression`")
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


//var exp1 = try Expression("+cos(π / 4) + 4") //"+((4) + 10 - 7) - (-64.29434) - 7 ^ 4 + cos(180 - 493 + sin(20))"
try LookupTable.define(constant: .init("x", value: 0))

var expStr = "e ^ (5 * x) + e ^ (1 / 4 * x) + 4"//"4cos(x)+3"//"+((4) + 10 - 7) - (-64.29434) - 7 ^ 4 + cos(180 - 493 + sin(20))"
print(expStr)
var exp1 = try Expression(expStr)
print(exp1)
print(exp1.evaluated())

var exp = try Expression("x ^ 2")
for x in 0...10 {
    try LookupTable.updateConstant(withIdentifier: "x", to: Double(x))
    print("\(x) ^ 2 = \(exp.evaluated())")
}

try LookupTable.define(postfixOperator: .init("--") { $0 - 1 })



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


//enum XToken: Parsable, CaseIterable {
//    typealias Base = Self
//    case a
//    case ab
//    case bc
//    case bcc
//    case ccd
//    case cdd
//
//    var pattern: TokenPattern {
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
//        return AnyGrammarPattern(AnyGrammarPattern(a || ab) + AnyGrammarPattern(bc || bcc) + AnyGrammarPattern(ccd || cdd))
//    }
////    case a
////    case b
////    case c
////    case d
////
////    static let pattern = "a" + ("b" || ("b" + "b")) + "c"
////    var pattern: TokenPattern {
////        switch self {
////        case .a: return "a"//Self.pattern
////        case .b: return "b"
////        case .c: return "c"
////        case .d: return "d"
////        }
////    }
////
////    /// #Should match:
////    ///   - `abbc`
////    ///   - `abc`
////    static var grammarPattern: AnyGrammarPattern<XToken> {
//////        let temp1 = a + b.opt()
//////        let grammar = temp1 + b + c
//////        let bb = b + b
//////        print(type(of: a + (b || bb) + c))
//////        return AnyGrammarPattern<XToken>(a + (b || bb) + c)
////        let ab = a + b
////        let bc = b + c
////        let bcc = bc + c
////        let cc = c + c
////        let ccd = cc + d
////        let cd = c + d
////        let cdd = cd + d
////        let pattern = (a || ab) + (bc || bcc) + (ccd || cdd)
////        return AnyGrammarPattern<XToken>(pattern)
////    }
//}
//
//print("\n\n")
//var str = "abbcccdd"//"abccccd"
//print(str)
//var tokenizer = Tokenizer(using: XToken.self)
//var tokens = [MatchedToken<XToken>]()
//do {
//    tokens = try tokenizer.tokenize(str)
//    print(tokens)
//} catch let error as ParseError {
//    print(error.localizedDescription)
//} catch {
//    print(error.localizedDescription)
//}

//
////
////
////
////protocol B: A where T == Self {
////    static var thing: AnyA<Self> { get }
////}
////
////protocol A {
////    associatedtype T: B
////}
////
////struct C<L, R>: A where L: A, R: A, L.T == R.T {
////    var l: L
////    var r: R
////    typealias T = L.T
////}
////func +<T, U>(lhs: T, rhs: U) -> C<T, U> where T: A, U: A, T.T == U.T {
////    return C(l: lhs, r: rhs)
////}
//////struct D<L, R>: A where  L: A, R: A, L.T == R.T {
//////    var l: L
//////    var r: R
//////    typealias T = L.T
//////}
//////func ||<T, U>(lhs: T, rhs: U) -> D<T, U> {
//////    return D(l: lhs, r: rhs)
//////}
////fileprivate class _AnyABoxBase<T>: A where T: B {
////    typealias Base = T
////}
////fileprivate class _AnyABox<T: A>: _AnyABoxBase<T.T> {
////    let base: T
////    init(_ base: T) {
////        self.base = base
////    }
////}
////final class AnyA<U>: A where U: B {
////    typealias T = U
////    private let box: _AnyABoxBase<U>
////
////    init<P>(_ base: P) where P: A, P.T == U {
////        self.box = _AnyABox(base)
////    }
////
////}
////enum Foo: B {
////    typealias T = Self
////    case a
////    case b
////    case c
////    case d
////
////    static var thing: AnyA<Foo> {
////        let temp = a + b + c
////        return AnyA<Foo>(temp)
//////        let temp1 = a + b
//////        let temp2 = temp1 + c + d
//////        return AnyA<Foo>(temp2)
////    }
////}
//
//
//
////enum Foo: CaseIterable, Parsable {
////    typealias Base = Self
////
////    case m
////    case n
////    case o
////
////    var pattern: TokenPattern {
////        switch self {
////        case .m: return  "A" || "AB"
////        case .n: return  "BC" || "BCC"
////        case .o: return  "CCD" || "CDD"
////
////        }
////    }
////
////    static var grammarPattern: AnyGrammarPattern<Foo> {
////        let temp = m + n
////        return AnyGrammarPattern(temp + o)
////    }
////}
////
////do {
////    let str = "ABCCCCD" //"ABBC"
////    //"abbcccdd"//"abccccd"
////    let tokenizer = Tokenizer(using: Foo.self)
////    var tokens = [MatchedToken<Foo>]()
////    do {
////        tokens = try tokenizer.tokenize(str)
////        print(tokens)
////    } catch let error as ParseError {
////        print(error.localizedDescription)
////    } catch {
////        print(error.localizedDescription)
////    }
////
////}
//
////struct C<L, R> {
////    var l: L
////    var r: R
////}
////
////func +<T, U>(lhs: T, rhs: U) -> C<T, U> {
////    return C(l: lhs, r: rhs)
////}
////
////enum Foo {
////    case a
////
////    static var thing: Any {
////        let temp = a + a + a + a // Error: Cannot convert value of type 'C<Foo, Foo>' to expected argument type 'Foo'
////        return temp
////    }
////}
//
//
////protocol Grammar: CaseIterable, Parsable {
////    associatedtype Base = Self
////}

//
//enum BaseToken: Parsable, CaseIterable {
//    typealias Base = Self
//
//    case a
//    case ab
//    case bc
//    case bcc
//    case ccd
//    case cdd
//
//    var pattern: TokenPattern {
//        switch self {
//        case .a: return "A"
//        case .ab: return "AB"
//        case .bc: return "BC"
//        case .bcc: return "BCC"
//        case .ccd: return "CCD"
//        case .cdd: return "CDD"
//        }
//    }
//
//    static var grammarPattern: AnyGrammarPattern<BaseToken> {
//        let temp1 = a || ab
//        let temp2 = bc || bcc
//        let temp3 = ccd || cdd
//        let temp4 = temp1 + temp2
//        let temp5 = temp4 + temp3
//        return AnyGrammarPattern(temp5)
//    }
//}
//do {
//    //"abbcccdd"//"abccccd"
//    let input = "ABBCCCDD"
//    let tokenizer = Tokenizer(using: BaseToken.self)
//    let tokens = try tokenizer.tokenize(input)
//    print(tokens)
//}

/**
 # Parsing Steps
 
     1. Create an input string, `"ABBCCCDD"`, that will be tokenized.
 
     2. Initialize a `Tokenizer<BaseToken>` instance using `Tokenizer(using:)`, passing `BaseToken.self` as the argument.
 
     3. Call `tokenize(_:)` method on tokenizer instance passing in the input string to be parsed. (note that all of the steps following this one happen internally)
 
     4. Initialize an instance of `GrammarParsingContext<BaseToken>`.
 
     5. Call `parseInput()` method on context.
 
     6. Call `parse(from:index:)` on root `+` pattern. The root pattern is shown in the diagram below.
         ```
                ┌──── + ────┐
                │           │
            ┌── + ──┐     ┌ || ┐
            │       │     │    │
         ┌ || ┐   ┌ || ┐ CCD  CDD
         │    │   │    │
         A    AB BC   BCC
         ```
 
     7. Attempt to parse first pattern, `+`, of the current `+` pattern, descending into it.
 
     8. Attempt to parse first pattern, `||`, of the current `+` pattern, descending into it.
 
     9. Take a snapshot of the current context.

     10. Successfully parse `A` from input.
 
     11. Add recovery point for right pattern, `AB`, using snapshot.
 
     12. Propogate successful parsing result upwards to parent `+` pattern.
 
     13. Update pattern of recovery point to `AB + (BC || BCC)`.
 
     14. Attempt to parse second pattern, `||`, of the current `+` pattern, descending into it.
 
     15. Attempt to parse `BC` and fail. Try to parse the right pattern next instead.
 
     16. Attempt to parse `BCC` and fail.
 
     17. Propogate failure upwards to parent `+` pattern.
  
     18. Propogate failure upwards to the parent (root) `+` pattern.
     
     19. Update pattern of recovery point to `(AB + (BC || BCC)) + (CCD || CDD)`.
 
     20. Propogate failure upwards to initial call site, `parseInput()`.
 
     21. Check for recovery points to see if we can recover parsing.
 
     22. Pop the last recovery point and revert the context to it. Start parsing from the recovery point's pattern, shown below.
        ```
             ┌──── + ────┐
             │           │
         ┌── + ──┐     ┌ || ┐
         │       │     │    │
        AB     ┌ || ┐ CCD  CDD
               │    │
              BC   BCC
        ```
 
     23. Attempt to parse first pattern, `+`, of the current root `+` pattern, descending into it.

     24. Successfully parse left pattern, `AB`, from input.
 
     25. Attempt to parse second pattern , `||`, of the current `+` pattern, descending into it.
 
     26. Take a snapshot of the current context.
 
     27. Successfully parse left pattern, `BC` from the input.

     28. Add recovery point for right pattern, `BCC`, using snapshot.
 
     29. Propogate successful parsing result upwards to parent `+` pattern.
 
     30. Propogate successful parsing result upwards to parent (root) `+` pattern.
 
     31. Update pattern of recovery point to `BCC + (CCD || CDD)`.

     32. Attempt to parse second pattern,  `||`, of the current `+` pattern, descending into it.

     33. Attempt to parse `CCD` and fail. Try to parse the right pattern next instead.
     
     34. Attempt to parse `CDD` and fail.
 
     35. Propogate failure upwards to parent (root) `+` pattern.
 
     36. Propogate failure upwards to initial call site, `parseInput()`.

     37. Check for recovery points to see if we can recover parsing.

     38. Pop the last recovery point and revert the context to it. Start parsing from the recovery point's pattern, shown below.
         ```
          ┌── + ──┐
          │       │
         BCC    ┌ || ┐
                │    |
               CCD  CDD
         ```
 
     39. Successfully parse the left pattern of the current root `+` pattern.
 
     40. Propogate successful parsing result upwards to parent (root) `+` pattern.
 
     41. Attempt to parse second pattern, `||`, of the current `+` pattern, descending into it.
 
     42. Attempt to parse `CCD` and fail. Try to parse the right pattern next instead.
 
     43. Successfully parse `CDD` from input.
 
     44. Propogate successful parsing result upwards to parent (root) `+` pattern.
 
     45. Propogate successful parsing result upwards to initial call site, `parseInput()`.
 
     46. Return `consumedTokens` property of context from `parseInput()`.
 
     47. Propogate return value of`parseInput()` to return value of the invocation the `tokenize(_:)` method on our tokenizer instance.
 */

