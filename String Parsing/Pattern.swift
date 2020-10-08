//
//  Pattern.swift
//  String Parsing
//
//  Created by Noah Wilder on 2020-05-15.
//  Copyright © 2020 Noah Wilder. All rights reserved.
//

import Foundation

//MARK: - Pattern
protocol Pattern {
    func consumed<T>(from input: T) -> Substring? where T: StringProtocol, T.SubSequence == Substring
}

extension String: Pattern {
    func consumed<T>(from input: T) -> Substring? where T: StringProtocol, T.SubSequence == Substring {
        guard self.count <= input.count else {
            return nil
        }
        var idx = input.startIndex
        for char in self {
            guard char == input[idx] else {
                return nil
            }
            idx = input.index(after: idx)
        }
        return input[idx...]
    }
}

struct LogicalOrPattern<P1, P2> where P1: Pattern, P2: Pattern {
    var leftPattern: P1
    var rightPattern: P2
    
    init(left: P1, right: P2) {
        self.leftPattern = left
        self.rightPattern = right
    }
}
extension LogicalOrPattern: Pattern {
    // Currently, the left-most pattern that's matched will be used
    func consumed<T>(from input: T) -> Substring? where T: StringProtocol, T.SubSequence == Substring {
        if let consumedStr = leftPattern.consumed(from: input) {
            return consumedStr
        } else if let consumedStr = rightPattern.consumed(from: input) {
            return consumedStr
        }
        return nil
    }
}
func ||<T, U>(lhs: T, rhs: U) -> LogicalOrPattern<T, U> where T: Pattern, U: Pattern {
    return LogicalOrPattern(left: lhs, right: rhs)
}

struct OptionalPattern<T> where T: Pattern {
    var pattern: T
    init(_ pattern: T) {
        self.pattern = pattern
    }
}
extension OptionalPattern: Pattern {
    func consumed<T>(from input: T) -> Substring? where T: StringProtocol, T.SubSequence == Substring {
        if let consumedStr = pattern.consumed(from: input) {
            return consumedStr
        }
        return input[...]
    }
}
extension Pattern {
    func opt() -> OptionalPattern<Self> {
        return OptionalPattern(self)
    }
}

struct SequentialPattern<P1, P2>: Pattern where P1: Pattern, P2: Pattern {
    var pattern1: P1
    var pattern2: P2
    
    func consumed<T>(from input: T) -> Substring? where T: StringProtocol, T.SubSequence == Substring {
        guard let consumedStr = pattern1.consumed(from: input) else {
            return nil
        }
        return pattern2.consumed(from: consumedStr)
    }
}
func +<T, U>(lhs: T, rhs: U) -> SequentialPattern<T, U> where T: Pattern, U: Pattern {
    return SequentialPattern(pattern1: lhs, pattern2: rhs)
}

class SharedPattern: Pattern {
    var pattern: Pattern!
    
    init() { }
    init(_ pattern: Pattern) {
        self.pattern = pattern
    }
    
    func consumed<T>(from input: T) -> Substring? where T: StringProtocol, T.SubSequence == Substring {
        return pattern.consumed(from: input)
    }
}

class RecursivePattern: Pattern {
    var pattern: Pattern!
    init(_ recursivePattern: (_ selfPattern: RecursivePattern) -> Pattern) {
        self.pattern = recursivePattern(self)
    }
    func consumed<T>(from input: T) -> Substring? where T: StringProtocol, T.SubSequence == Substring {
        return pattern.consumed(from: input)
    }
}
struct RegexPattern: Pattern {
    var pattern: String
    
    init(_ pattern: String) {
        self.pattern = pattern
    }
    
    func consumed<T>(from input: T) -> Substring? where T : StringProtocol, T.SubSequence == Substring {
        guard let range = input.range(of: pattern, options: [.anchored, .regularExpression]) else {
            return nil
        }
        return input[range.upperBound...]
    }
}

extension KeyPath: Pattern where Root == Character, Value == Bool {
    func consumed<T>(from input: T) -> Substring? where T: StringProtocol, T.SubSequence == Substring {
        if let firstCharacter = input.first, firstCharacter[keyPath: self] {
            return input.dropFirst()
        }
        return nil
    }
}

//MARK: - GrammarPattern


protocol GrammarPattern {
    associatedtype TokenType: Parsable
    func parse(from context: GrammarParsingContext<TokenType>, index: GrammarPatternIndex) -> GrammarParsingResult
}

extension Parsable {
    func parse(from context: GrammarParsingContext<TokenType>, index: GrammarPatternIndex) -> GrammarParsingResult {
        guard let remaining = pattern.consumed(from: context.input[context.currentIndex...]) else {
            return .failure
        }
        let token = MatchedToken(classification: self, match: context.input[context.currentIndex..<remaining.startIndex])
        context.advance(to: remaining.startIndex, adding: token)
        return .success
    }
}

struct GrammarPatternIndex {
    var depth: Int
    var position: Double
    
    init() {
        self.depth = 0
        self.position = 0
    }
    
    static let zero = GrammarPatternIndex()

    func translated(_ verticalDirection: VerticalDirection, and horizontalDirection: HorizontalDirection) -> GrammarPatternIndex {
        var newIndex = self
        newIndex.depth += verticalDirection.rawValue
        let positionIncrement = pow(1.0 / 2.0, Double(newIndex.depth))
        switch horizontalDirection {
        case .left: newIndex.position -= positionIncrement
        case .right: newIndex.position += positionIncrement
        }
        return newIndex
    }
    
    enum VerticalDirection: Int {
        case down = 1
        case up = -1
    }
    enum HorizontalDirection {
        case left, right
    }
    enum Position {
        indirect case left(parent: Position)
        indirect case right(parent: Position)
        case leaf
    }
}

final class GrammarParsingContext<T> where T: Parsable {
    private(set) var input: String
    private(set) var basePattern: AnyGrammarPattern<T>
    private(set) var consumedTokens = [MatchedToken<T>]()
    private(set) var currentIndex: String.Index {
        didSet {
            if currentIndex > furthestIndex {
                furthestIndex = currentIndex
            }
        }
    }
    private(set) var furthestIndex: String.Index
    var recoveryPoints: [RecoveryPoint] = []
    private var parsingOutput: [MatchedToken<T>]?
    private var isParsing = true
    
    init(input: String) {
        self.input = input
        self.currentIndex = input.startIndex
        self.furthestIndex = input.startIndex
        self.basePattern = AnyGrammarPattern(T.grammarPattern)
    }

    func parseInput() -> [MatchedToken<T>]? {
        guard isParsing else {
            return parsingOutput
        }
        defer { isParsing = false }
        let initialResult = basePattern.parse(from: self, index: .zero)
        if initialResult == .success, currentIndex == input.endIndex {
            parsingOutput = consumedTokens
            return parsingOutput
        }
        while let recoveryPoint = recoveryPoints.popLast() {
            let result = revert(to: recoveryPoint)
            if result == .success, currentIndex == input.endIndex {
                parsingOutput = consumedTokens
                return parsingOutput
            }
        }
        parsingOutput = nil
        return nil
    }
    
    func advance(to newIndex: String.Index, adding consumedTokens: [MatchedToken<T>]) {
        self.currentIndex = newIndex
        self.consumedTokens.append(contentsOf: consumedTokens)
    }
    func advance(to newIndex: String.Index, adding consumedToken: MatchedToken<T>) {
        advance(to: newIndex, adding: [consumedToken])
    }
    
    var hasRecoveryPoint: Bool {
        return !recoveryPoints.isEmpty
    }
    
    func addRecoveryPoint<P>(at index: GrammarPatternIndex, pattern: P) where P: GrammarPattern, P.TokenType == T {
        let snapshot = Snapshot(tokens: consumedTokens, currentIndex: currentIndex)
        let recoveryPoint = RecoveryPoint(snapshot: snapshot, index: index, pattern: pattern)
        recoveryPoints.append(recoveryPoint)
    }
    
    func attemptRecovery() -> GrammarParsingResult {
        while let recoveryPoint = recoveryPoints.popLast() {
            let result = revert(to: recoveryPoint)
            if result == .success {
                return .success
            }
        }
        return .failure
    }
    
    private func revert(to recoveryPoint: RecoveryPoint) -> GrammarParsingResult {
        self.currentIndex = recoveryPoint.snapshot.currentIndex
        self.consumedTokens = recoveryPoint.snapshot.tokens
        return recoveryPoint.pattern.parse(from: self, index: recoveryPoint.index)
    }

    final class Snapshot {
        var tokens: [MatchedToken<T>]
        var currentIndex: String.Index

        internal init(tokens: [MatchedToken<T>], currentIndex: String.Index) {
            self.tokens = tokens
            self.currentIndex = currentIndex
        }
    }
    final class RecoveryPoint {
        var isClosed = false
        var snapshot: Snapshot
        var pattern: AnyGrammarPattern<T>
        var index: GrammarPatternIndex

        internal init<P>(snapshot: Snapshot, index: GrammarPatternIndex, pattern: P) where P: GrammarPattern, P.TokenType == T {
            self.snapshot = snapshot
            self.pattern = AnyGrammarPattern(pattern)
            self.index = index
        }
        
        func updatePattern<P>(to newPattern: (AnyGrammarPattern<T>) -> P) where P: GrammarPattern, P.TokenType == T {
            guard !isClosed else {
                return
            }
            self.pattern = AnyGrammarPattern(newPattern(self.pattern))
        }
        
        func close() {
            self.isClosed = true
        }
        func open() {
            self.isClosed = false
        }
    }
    
    @discardableResult
    func updateRecovery<P>(at index: GrammarPatternIndex, newPattern: (AnyGrammarPattern<T>) -> P, closingPoints: Bool) -> (() -> Void)? where P: GrammarPattern, P.TokenType == T {
        let shouldCloseRecoveryPoints = index.position == (pow(2, Double(index.depth)) - 1) / pow(2, Double(index.depth))
        var closedRecoveryPoints = [RecoveryPoint]()
        for recoveryPoint in recoveryPoints where !recoveryPoint.isClosed && recoveryPoint.index.position < index.position && recoveryPoint.index.depth > index.depth {
            recoveryPoint.updatePattern(to: newPattern)
            if closingPoints {
                recoveryPoint.close()
                if !shouldCloseRecoveryPoints {
                    closedRecoveryPoints.append(recoveryPoint)
                }
            }
        }
        if closingPoints {
            return { closedRecoveryPoints.forEach { $0.close() } }
        }
        return nil
    }
}

enum GrammarParsingResult {
    case success
    case failure
}

struct LogicalOrGrammarPattern<P1, P2>: GrammarPattern where P1: GrammarPattern, P2: GrammarPattern, P1.TokenType == P2.TokenType {
    var leftPattern: P1
    var rightPattern: P2
    typealias TokenType = P1.TokenType

    init(left: P1, right: P2) {
        self.leftPattern = left
        self.rightPattern = right
    }
    
    func parse(from context: GrammarParsingContext<TokenType>, index: GrammarPatternIndex) -> GrammarParsingResult {
        
        context.addRecoveryPoint(at: index, pattern: rightPattern)
        let leftResult = leftPattern.parse(from: context, index: index.translated(.down, and: .left))
        
        guard case .success = leftResult else {
            return rightPattern.parse(from: context, index: index)
        }
        
        context.updateRecovery(at: index, newPattern: { $0 || rightPattern }, closingPoints: false)
        return .success
    }
}

func ||<T, U>(lhs: T, rhs: U) -> LogicalOrGrammarPattern<T, U> {
    return LogicalOrGrammarPattern(left: lhs, right: rhs)
}

struct SequentialGrammarPattern<P1, P2>: GrammarPattern where P1: GrammarPattern, P2: GrammarPattern, P1.TokenType == P2.TokenType {
   
    
    typealias TokenType = P1.TokenType

    var pattern1: P1
    var pattern2: P2

    func parse(from context: GrammarParsingContext<P1.TokenType>, index: GrammarPatternIndex) -> GrammarParsingResult {
        let result1 = pattern1.parse(from: context, index: index.translated(.down, and: .left))

        guard case .success = result1 else {
            return .failure
        }
        
        let openPoints = context.updateRecovery(at: index, newPattern: { $0 + pattern2 }, closingPoints: true)
        
        let result2 = pattern2.parse(from: context, index: index.translated(.down, and: .right))
        openPoints?()
        return result2
    }
}
func +<T, U>(lhs: T, rhs: U) -> SequentialGrammarPattern<T, U> {
    return SequentialGrammarPattern(pattern1: lhs, pattern2: rhs)
}

fileprivate class _AnyGrammarPatternBoxBase<T>: GrammarPattern where T: Parsable {
    typealias TokenType = T

    func parse(from context: GrammarParsingContext<TokenType>, index: GrammarPatternIndex) -> GrammarParsingResult {
        fatalError()
    }
}
fileprivate class _AnyGrammarPatternBox<T: GrammarPattern>: _AnyGrammarPatternBoxBase<T.TokenType> {
    let base: T
    init(_ base: T) {
        self.base = base
    }
    override func parse(from context: GrammarParsingContext<TokenType>, index: GrammarPatternIndex) -> GrammarParsingResult {
        return base.parse(from: context, index: index)
    }
}
final class AnyGrammarPattern<U>: GrammarPattern where U: Parsable {
    typealias TokenType = U
    private let box: _AnyGrammarPatternBoxBase<U>

    init<P>(_ base: P) where P: GrammarPattern, P.TokenType == TokenType {
        self.box = _AnyGrammarPatternBox(base)
    }
    func parse(from context: GrammarParsingContext<TokenType>, index: GrammarPatternIndex) -> GrammarParsingResult {
        return box.parse(from: context, index: index)
    }
}
struct EmptyGrammarPattern<U>: GrammarPattern where U: Parsable {
    typealias TokenType = U
    func parse(from context: GrammarParsingContext<U>, index: GrammarPatternIndex) -> GrammarParsingResult {
        return .success
    }
}
struct OptionalGrammarPattern<T> where T: GrammarPattern {
    typealias TokenType = T.TokenType

    var pattern: T
    init(_ pattern: T) {
        self.pattern = pattern
    }
}
extension OptionalGrammarPattern: GrammarPattern {
    func parse(from context: GrammarParsingContext<TokenType>, index: GrammarPatternIndex) -> GrammarParsingResult {
        let newPattern = pattern || EmptyGrammarPattern()
        return newPattern.parse(from: context, index: index)
    }
}
extension GrammarPattern {
    func opt() -> OptionalGrammarPattern<Self> {
        return OptionalGrammarPattern(self)
    }
}

class RecursiveGrammarPattern<U>: GrammarPattern where U: Parsable {
    typealias TokenType = U

    var pattern: AnyGrammarPattern<U>!
    init<P>(_ recursivePattern: (_ selfPattern: RecursiveGrammarPattern<U>) -> P) where P: GrammarPattern, P.TokenType == U {
        self.pattern = AnyGrammarPattern(recursivePattern(self))
    }
    func parse(from context: GrammarParsingContext<U>, index: GrammarPatternIndex) -> GrammarParsingResult {
        pattern.parse(from: context, index: index)
    }
}

class SharedGrammarPattern<U>: GrammarPattern where U: Parsable {
    typealias TokenType = U

    var pattern: AnyGrammarPattern<U>

    init() {
        pattern = AnyGrammarPattern(NeverGrammarPattern())
    }
    init<P>(_ pattern: P) where P: GrammarPattern, P.TokenType == U {
        self.pattern = AnyGrammarPattern(pattern)
    }

    func parse(from context: GrammarParsingContext<U>, index: GrammarPatternIndex) -> GrammarParsingResult {
        pattern.parse(from: context, index: index)
    }
}

struct NeverGrammarPattern<U>: GrammarPattern where U: Parsable {
    typealias TokenType = U
    init() { }

    func parse(from context: GrammarParsingContext<U>, index: GrammarPatternIndex) -> GrammarParsingResult {
        return .failure
    }
}
