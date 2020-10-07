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

//protocol GrammarPattern {
//    associatedtype TokenType: Parsable
//    func parse(from context: GrammarParsingContext<TokenType>) -> GrammarParsingResult<TokenType>
////    func consuming<T>(context: GrammarParsingContext<TokenType>) where T : StringProtocol, T.SubSequence == Substring
//}
//extension Parsable {
//    func parse(from context: GrammarParsingContext<TokenType>) -> GrammarParsingResult<TokenType>  {
//        guard let remaining = pattern.consumed(from: context.input[context.currentIndex...]) else {
//            return .failure(in: context)
//        }
//        return .success(in: context, token: self, currentIndex: remaining.startIndex)
//    }
//}
//
//struct GrammarParsingResult<T> where T: Parsable {
//
////    case success(tokens: [MatchedToken<T>], currentIndex: String.Index)
////    case success(token: T, currentIndex: String.Index)
////    case failure
////    private let result: _Result
//
//    var context: GrammarParsingContext<T>
//    private var result: _Result
//
//    private init(context: GrammarParsingContext<T>, result: _Result) {
//        self.context = context
//        self.result = result
//    }
//    static func success<T>(in context: GrammarParsingContext<T>) -> GrammarParsingResult<T> where T: Parsable {
//        return GrammarParsingResult<T>(context: context, result: .success)
//    }
//
//    static func success<T>(in context: GrammarParsingContext<T>, tokens: [MatchedToken<T>], currentIndex: String.Index) -> GrammarParsingResult<T> where T: Parsable {
//        context.tokens.append(contentsOf: tokens)
//        context.currentIndex = currentIndex
//        return GrammarParsingResult<T>(context: context, result: .success)
//    }
//    static func success<T>(in context: GrammarParsingContext<T>, token: T, currentIndex: String.Index) -> GrammarParsingResult<T> where T: Parsable {
//        return success(in: context, tokens: [MatchedToken(classification: token, match: context.input[context.currentIndex..<currentIndex])], currentIndex: currentIndex)
//    }
//    static func failure<T>(in context: GrammarParsingContext<T>) -> GrammarParsingResult<T> where T: Parsable {
//        // do something on failure
//        return GrammarParsingResult<T>(context: context, result: .failure)
//    }
//    var wasSuccessful: Bool {
//        switch result {
//        case .success: return true
//        case .failure: return false
//        }
//    }
////    var success: (currentIndex: String.Index, additionalTokens: [MatchedToken<T>])?
////
////    private init(context: GrammarParsingContext<T>, success: (currentIndex: String.Index, additionalTokens: [MatchedToken<T>])?) {
////        self.context = context
////        self.success = success
////    }
////    static func success<T>(in context: GrammarParsingContext<T>, tokens: [MatchedToken<T>], currentIndex: String.Index) -> GrammarParsingResult<T> where T: Parsable {
////        return GrammarParsingResult<T>(
////            context: context,
////            success: (currentIndex: currentIndex, additionalTokens: tokens)
////        )
////    }
////    static func success<T>(in context: GrammarParsingContext<T>, token: T, currentIndex: String.Index) -> GrammarParsingResult<T> where T: Parsable {
////        return GrammarParsingResult<T>(
////            context: context,
////            success: (
////                currentIndex: currentIndex,
////                additionalTokens: [MatchedToken(classification: token, match: context.input[context.currentIndex..<currentIndex])]
////            )
////        )
////    }
//
////    static func failure(in context: GrammarParsingContext<T>) -> Self {
////        retrun
////    }
////    static func failure<T>(in: GrammarParsingContext<T>) -> Self where T: Parsable {
////        return Self(.failure)
////    }
////    var wasSuccessful: Bool {
////        return success != nil
////    }
////
//    private enum _Result {
//        case success
//        case failure
//    }
//}
//
//class GrammarParsingContext<T> where T: Parsable {
//    var input: String
////    let basePattern: AnyGrammarPattern<T>
////    var currentPattern: AnyGrammarPattern<T>
//    var tokens = [MatchedToken<T>]()
//    var currentIndex: String.Index {
//        didSet {
//            if currentIndex > furthestIndex {
//                furthestIndex = currentIndex
//            }
//        }
//    }
//    var furthestIndex: String.Index
//    private(set) var isParsing = true
//
//    var recoveryPoint: RecoveryPoint?
//
//    init(input: String) {//, basePattern: P) where P: GrammarPattern, P.TokenType == T {
//        self.input = input
//        self.currentIndex = input.startIndex
//        self.furthestIndex = input.startIndex
////        self.basePattern = AnyGrammarPattern(basePattern)
////        self.currentPattern = AnyGrammarPattern(basePattern)
//    }
//
////    func consume(_ result: GrammarParsingResult<T>) {
////        guard let (idx, additionalTokens) = result.success else {
////            return
////        }
////        self.currentIndex = idx
////        self.tokens.append(contentsOf: additionalTokens)
////    }
//
//    class Snapshot {
//        var tokens: ArraySlice<MatchedToken<T>>
//        var currentIndex: String.Index
////        weak var context: GrammarParsingContext?
//
//        internal init(tokens: ArraySlice<MatchedToken<T>>, currentIndex: String.Index) {
//            self.tokens = tokens
////            self.pattern = pattern
//            self.currentIndex = currentIndex
//        }
//    }
//
//    class RecoveryPoint {
//        var snapshot: Snapshot
//        var pattern: AnyGrammarPattern<T>
//
//        internal init<P>(snapshot: Snapshot, pattern: P) where P: GrammarPattern, P.TokenType == T {
//            self.snapshot = snapshot
//            self.pattern = AnyGrammarPattern(pattern)
//        }
//    }
//
//    func addRecoveryOption<P>(initialRecoveryPoint: RecoveryPoint, updatedPattern: (AnyGrammarPattern<T>) -> P) where P: GrammarPattern, P.TokenType == T {
//        if let recoveryPattern = recoveryPoint?.pattern {
//            recoveryPoint?.pattern = AnyGrammarPattern(updatedPattern(recoveryPattern))
//        } else {
//            recoveryPoint = initialRecoveryPoint
//        }
//    }
//    func updateRecoveryOption<P>(updatedPattern: (AnyGrammarPattern<T>) -> P) where P: GrammarPattern, P.TokenType == T {
//        if let recoveryPattern = recoveryPoint?.pattern {
//            recoveryPoint?.pattern = AnyGrammarPattern(updatedPattern(recoveryPattern))
//        }
//    }
//    func attemptRecovery() -> GrammarParsingResult<T> {
//        guard let recoveryPoint = recoveryPoint else {
//            isParsing = false
//            return .failure(in: self)
//        }
//        revert(to: recoveryPoint.snapshot)
//        let pattern = recoveryPoint.pattern
//        self.recoveryPoint = nil
//        return pattern.parse(from: self)
//    }
//
//    private func revert(to snapshot: Snapshot) {
//        tokens = Array(snapshot.tokens)
//        currentIndex = snapshot.currentIndex
//    }
//
//    func snapshot() -> Snapshot {
//        return Snapshot(
//            tokens: tokens[...],
//            currentIndex: currentIndex
//        )
//    }
//}
//
//struct LogicalOrGrammarPattern<P1, P2>: GrammarPattern where P1: GrammarPattern, P2: GrammarPattern, P1.TokenType == P2.TokenType {
//    var leftPattern: P1
//    var rightPattern: P2
//    typealias TokenType = P1.TokenType
//
//    init(left: P1, right: P2) {
//        self.leftPattern = left
//        self.rightPattern = right
//    }
//
//    func parse(from context: GrammarParsingContext<TokenType>) -> GrammarParsingResult<TokenType> {
//        let snapshot = context.snapshot()
//        let leftResult = leftPattern.parse(from: context)
//        if leftResult.wasSuccessful {
//            let recoveryOption = GrammarParsingContext<TokenType>.RecoveryPoint(snapshot: snapshot, pattern: rightPattern)
//            context.addRecoveryOption(initialRecoveryPoint: recoveryOption, updatedPattern: {
//                $0 || rightPattern
//            })
////            context.consume(leftResult)
//            return .success(in: context)
//        }
//        let rightResult = rightPattern.parse(from: context)
//        if rightResult.wasSuccessful {
//            return .success(in: context)
//        }
//        return context.attemptRecovery()
//    }
//}
////    func consuming<T>(input: T) -> (tokens: [MatchedToken<TokenType>], remaining: Substring, returnPoint: GrammarReturnPoint<TokenType>?)? where T: StringProtocol, T.SubSequence == Substring {
////        if let leftOutput = leftPattern.consuming(input: input) {
////            leftOutput.returnPoint?.updatePattern { $0 || rightPattern }
////            let returnPoint = leftOutput.returnPoint ?? .init(rightPattern, input: input, consumedTokens: [])
////            return (tokens: leftOutput.tokens, remaining: leftOutput.remaining, returnPoint: returnPoint)
////        } else if let rightOutput = rightPattern.consuming(input: input) {
////            return (tokens: rightOutput.tokens, remaining: rightOutput.remaining, returnPoint: rightOutput.returnPoint)
////        }
////        return nil
////    }
//func ||<T, U>(lhs: T, rhs: U) -> LogicalOrGrammarPattern<T, U> where T: GrammarPattern, U: GrammarPattern, T.TokenType == U.TokenType {
//    return LogicalOrGrammarPattern(left: lhs, right: rhs)
//}
//
//struct SequentialGrammarPattern<P1, P2>: GrammarPattern where P1: GrammarPattern, P2: GrammarPattern, P1.TokenType == P2.TokenType {
//    typealias TokenType = P1.TokenType
//
//    var pattern1: P1
//    var pattern2: P2
//
//    func parse(from context: GrammarParsingContext<TokenType>) -> GrammarParsingResult<TokenType> {
//        let result1 = pattern1.parse(from: context)
//        guard result1.wasSuccessful else {
//            return context.attemptRecovery()
//        }
//        context.updateRecoveryOption { $0 + pattern2 }
//        let result2 = pattern2.parse(from: context)
//        guard result2.wasSuccessful else {
//            return context.attemptRecovery()
//        }
//        return .success(in: context)
//    }
////    func consuming<T>(input: T) -> (tokens: [MatchedToken<P1.TokenType>], remaining: Substring, returnPoint: GrammarReturnPoint<TokenType>?)? where T : StringProtocol, T.SubSequence == Substring {
////        guard let output = pattern1.consuming(input: input) else {
////            return nil
////        }
////        output.returnPoint?.updatePattern { $0 + pattern2 }
////        if var nextOutput = pattern2.consuming(input: output.remaining) {
////            if let returnPoint = output.returnPoint {
////                nextOutput.returnPoint = returnPoint
////            } else {
////                nextOutput.returnPoint?.consumedTokens += output.tokens
////            }
////            nextOutput.tokens = output.tokens + nextOutput.tokens
////            return nextOutput
////        } else if let returnPoint = output.returnPoint {
////            return returnPoint.attemptConsume()
////        }
////        return nil
////    }
//}
//func +<T, U>(lhs: T, rhs: U) -> SequentialGrammarPattern<T, U> where T: GrammarPattern, U: GrammarPattern, T.TokenType == U.TokenType {
//    return SequentialGrammarPattern(pattern1: lhs, pattern2: rhs)
//}
//fileprivate class _AnyGrammarPatternBoxBase<T>: GrammarPattern where T: Parsable {
//    typealias TokenType = T
//
//    func parse(from context: GrammarParsingContext<TokenType>) -> GrammarParsingResult<TokenType> {
//        fatalError()
//    }
//}
//fileprivate class _AnyGrammarPatternBox<T: GrammarPattern>: _AnyGrammarPatternBoxBase<T.TokenType> {
//    let base: T
//    init(_ base: T) {
//        self.base = base
//    }
//    override func parse(from context: GrammarParsingContext<TokenType>) -> GrammarParsingResult<TokenType> {
//        return base.parse(from: context)
//    }
//}
//class AnyGrammarPattern<U>: GrammarPattern where U: Parsable {
//    typealias TokenType = U
//    private let box: _AnyGrammarPatternBoxBase<U>
//
//    init<P>(_ base: P) where P: GrammarPattern, P.TokenType == TokenType {
//        self.box = _AnyGrammarPatternBox(base)
//    }
//    func parse(from context: GrammarParsingContext<TokenType>) -> GrammarParsingResult<TokenType> {
//        return box.parse(from: context)
//    }
//}
//struct EmptyGrammarPattern<U>: GrammarPattern where U: Parsable {
//    typealias TokenType = U
//    func parse(from context: GrammarParsingContext<U>) -> GrammarParsingResult<U> {
//        return .success(in: context)
//    }
//}
//struct OptionalGrammarPattern<T> where T: GrammarPattern {
//    typealias TokenType = T.TokenType
//
//    var pattern: T
//    init(_ pattern: T) {
//        self.pattern = pattern
//    }
//}
//extension OptionalGrammarPattern: GrammarPattern {
//    func parse(from context: GrammarParsingContext<T.TokenType>) -> GrammarParsingResult<T.TokenType> {
//        let snapshot = context.snapshot()
//        let result = pattern.parse(from: context)
//        guard result.wasSuccessful else {
//            return .success(in: context)
//        }
//        context.addRecoveryOption(initialRecoveryPoint: .init(snapshot: snapshot, pattern: EmptyGrammarPattern())) {
//            $0.opt()
//        }
//        return .success(in: context)
//    }
////    func consuming<T>(input: T) -> (tokens: [MatchedToken<TokenType>], remaining: Substring, returnPoint: GrammarReturnPoint<TokenType>?)? where T : StringProtocol, T.SubSequence == Substring {
////        pattern.
////        guard let output = pattern.consuming(input: input) else {
////            return (tokens: [], remaining: input[...], returnPoint: nil)
////        }
////        output.returnPoint?.updatePattern { $0.opt() }
////        let returnPoint = output.returnPoint ?? .init(EmptyGrammarPattern(), input: input, consumedTokens: [])
////        return (tokens: output.tokens, remaining: output.remaining, returnPoint: returnPoint)
////    }
//}
//extension GrammarPattern {
//    func opt() -> OptionalGrammarPattern<Self> {
//        return OptionalGrammarPattern(self)
//    }
//}
//
//class RecursiveGrammarPattern<U>: GrammarPattern where U: Parsable {
//    typealias TokenType = U
//
//    var pattern: AnyGrammarPattern<U>!
//    init<P>(_ recursivePattern: (_ selfPattern: RecursiveGrammarPattern<U>) -> P) where P: GrammarPattern, P.TokenType == U {
//        self.pattern = AnyGrammarPattern(recursivePattern(self))
//    }
//    func parse(from context: GrammarParsingContext<U>) -> GrammarParsingResult<U> {
//        pattern.parse(from: context)
//    }
//}
//
//class SharedGrammarPattern<U>: GrammarPattern where U: Parsable {
//    typealias TokenType = U
//
//    var pattern: AnyGrammarPattern<U>
//
//    init() {
//        pattern = AnyGrammarPattern(NeverGrammarPattern())
//    }
//    init<P>(_ pattern: P) where P: GrammarPattern, P.TokenType == U {
//        self.pattern = AnyGrammarPattern(pattern)
//    }
//
//    func parse(from context: GrammarParsingContext<U>) -> GrammarParsingResult<U> {
//        pattern.parse(from: context)
//    }
//}
//
//struct NeverGrammarPattern<U>: GrammarPattern where U: Parsable {
//    typealias TokenType = U
//    init() { }
//
//    func parse(from context: GrammarParsingContext<U>) -> GrammarParsingResult<U> {
//        return .failure(in: context)
//    }
////    func consuming<T>(input: T) -> (tokens: [MatchedToken<U>], remaining: Substring, returnPoint: GrammarReturnPoint<U>?)? where T : StringProtocol, T.SubSequence == Substring {
////        return nil
////    }
//}


//class GrammarReturnPoint<U>: GrammarPattern where U: Parsable {
//    typealias TokenType = U
//    private(set) var pattern: AnyGrammarPattern<U>
//    var consumedTokens: [MatchedToken<TokenType>]
//    var input: Substring
//
//    init<P, Q>(_ pattern: P, input: Q, consumedTokens: [MatchedToken<TokenType>]) where P: GrammarPattern, P.TokenType == TokenType, Q: StringProtocol, Q.SubSequence == Substring {
//        self.pattern = AnyGrammarPattern(pattern)
//        self.input = input[...]
//        self.consumedTokens = consumedTokens
//    }
//    func attemptConsume() -> (tokens: [MatchedToken<TokenType>], remaining: Substring, returnPoint: GrammarReturnPoint<TokenType>?)? {
//        guard var output = pattern.consuming(input: input) else {
//            return nil
//        }
//        output.tokens = consumedTokens + output.tokens
//        return output
//    }
//    func consuming<T>(input: T) -> (tokens: [MatchedToken<TokenType>], remaining: Substring, returnPoint: GrammarReturnPoint<TokenType>?)? where T : StringProtocol, T.SubSequence == Substring {
//        guard var output = pattern.consuming(input: input) else {
//            return nil
//        }
//        output.tokens = consumedTokens + output.tokens
//        return output
//    }
//    func updatePattern<P>(to newPattern: (AnyGrammarPattern<U>) -> P) where P: GrammarPattern, P.TokenType == TokenType {
//        self.pattern = AnyGrammarPattern(newPattern(pattern))
//    }
//
//
//}
//
//struct EmptyGrammarPattern<U>: GrammarPattern where U: Parsable {
//    typealias TokenType = U
//    func consuming<T>(input: T) -> (tokens: [MatchedToken<U>], remaining: Substring, returnPoint: GrammarReturnPoint<U>?)? where T : StringProtocol, T.SubSequence == Substring {
//        return (tokens: [], remaining: input[...], returnPoint: nil)
//    }
//}
//struct LogicalOrGrammarPattern<P1, P2>: GrammarPattern where P1: GrammarPattern, P2: GrammarPattern, P1.TokenType == P2.TokenType {
//    var leftPattern: P1
//    var rightPattern: P2
//    typealias TokenType = P1.TokenType
//
//    init(left: P1, right: P2) {
//        self.leftPattern = left
//        self.rightPattern = right
//    }
//
//    func consuming<T>(input: T) -> (tokens: [MatchedToken<TokenType>], remaining: Substring, returnPoint: GrammarReturnPoint<TokenType>?)? where T: StringProtocol, T.SubSequence == Substring {
//        if let leftOutput = leftPattern.consuming(input: input) {
//            leftOutput.returnPoint?.updatePattern { $0 || rightPattern }
//            let returnPoint = leftOutput.returnPoint ?? .init(rightPattern, input: input, consumedTokens: [])
//            return (tokens: leftOutput.tokens, remaining: leftOutput.remaining, returnPoint: returnPoint)
//        } else if let rightOutput = rightPattern.consuming(input: input) {
//            return (tokens: rightOutput.tokens, remaining: rightOutput.remaining, returnPoint: rightOutput.returnPoint)
//        }
//        return nil
//    }
//}
//func ||<T, U>(lhs: T, rhs: U) -> LogicalOrGrammarPattern<T, U> where T: GrammarPattern, U: GrammarPattern, T.TokenType == U.TokenType {
//    return LogicalOrGrammarPattern(left: lhs, right: rhs)
//}
//struct OptionalGrammarPattern<T> where T: GrammarPattern {
//    typealias TokenType = T.TokenType
//
//    var pattern: T
//    init(_ pattern: T) {
//        self.pattern = pattern
//    }
//}
//extension OptionalGrammarPattern: GrammarPattern {
//    func consuming<T>(input: T) -> (tokens: [MatchedToken<TokenType>], remaining: Substring, returnPoint: GrammarReturnPoint<TokenType>?)? where T : StringProtocol, T.SubSequence == Substring {
//        guard let output = pattern.consuming(input: input) else {
//            return (tokens: [], remaining: input[...], returnPoint: nil)
//        }
//        output.returnPoint?.updatePattern { $0.opt() }
//        let returnPoint = output.returnPoint ?? .init(EmptyGrammarPattern(), input: input, consumedTokens: [])
//        return (tokens: output.tokens, remaining: output.remaining, returnPoint: returnPoint)
//    }
//}
//extension GrammarPattern {
//    func opt() -> OptionalGrammarPattern<Self> {
//        return OptionalGrammarPattern(self)
//    }
//}
//
//struct SequentialGrammarPattern<P1, P2>: GrammarPattern where P1: GrammarPattern, P2: GrammarPattern, P1.TokenType == P2.TokenType {
//    typealias TokenType = P1.TokenType
//
//    var pattern1: P1
//    var pattern2: P2
//
//    func consuming<T>(input: T) -> (tokens: [MatchedToken<P1.TokenType>], remaining: Substring, returnPoint: GrammarReturnPoint<TokenType>?)? where T : StringProtocol, T.SubSequence == Substring {
//        guard let output = pattern1.consuming(input: input) else {
//            return nil
//        }
//        output.returnPoint?.updatePattern { $0 + pattern2 }
//        if var nextOutput = pattern2.consuming(input: output.remaining) {
//            if let returnPoint = output.returnPoint {
//                nextOutput.returnPoint = returnPoint
//            } else {
//                nextOutput.returnPoint?.consumedTokens += output.tokens
//            }
//            nextOutput.tokens = output.tokens + nextOutput.tokens
//            return nextOutput
//        } else if let returnPoint = output.returnPoint {
//            return returnPoint.attemptConsume()
//        }
//        return nil
//    }
//}
//func +<T, U>(lhs: T, rhs: U) -> SequentialGrammarPattern<T, U> where T: GrammarPattern, U: GrammarPattern, T.TokenType == U.TokenType {
//    return SequentialGrammarPattern(pattern1: lhs, pattern2: rhs)
//}
////
////struct RepeatingGrammarPattern<T>: GrammarPattern where T: GrammarPattern {
////    typealias TokenType = T.TokenType
////    var pattern: T
////    var allowedNumberOfRepeats: Range<UInt>
////
////    init(pattern: T, count: PartialRangeFrom<UInt>) {
////        self.pattern = pattern
////        self.allowedNumberOfRepeats = count.lowerBound..<UInt.max
////    }
////    init(pattern: T, count: ClosedRange<UInt>) {
////        self.pattern = pattern
////        self.allowedNumberOfRepeats = count.lowerBound..<(count.upperBound == UInt.max ? count.upperBound : count.upperBound + 1)
////    }
////    init(pattern: T, count: Range<UInt>) {
////        self.pattern = pattern
////        self.allowedNumberOfRepeats = count
////    }
////    init(pattern: T, count: UInt) {
////        self.pattern = pattern
////        self.allowedNumberOfRepeats = count..<(count == UInt.max ? count : count + 1)
////    }
////
////    func consuming<T>(input: T) -> [(tokens: [MatchedToken<TokenType>], remaining: Substring)] where T : StringProtocol, T.SubSequence == Substring {
////        //        var finalOutputs = [(tokens: [MatchedToken<TokenType>], remaining: Substring)]()
////        var partialOutputs: [(tokens: [MatchedToken<TokenType>], remaining: Substring)]
////        partialOutputs = [(tokens: [MatchedToken<TokenType>](), remaining: input[...])]
////        //        var tokens = [MatchedToken<TokenType>]()
////        //        var remaining = input[...]
////        for _ in 0..<allowedNumberOfRepeats.lowerBound {
////            let previousPartialOutputs = partialOutputs
////            partialOutputs.removeAll()
////            for partialOutput in previousPartialOutputs {
////                // String completed before all was matched
////                let nextOutputs = pattern.consuming(input: partialOutput.remaining)
////                guard !nextOutputs.isEmpty else {
////                    continue
////                }
////                let combinedPartialOutputs = nextOutputs.map { (tokens: partialOutput.tokens + $0.tokens, remaining: $0.remaining) }
////                partialOutputs.append(contentsOf: combinedPartialOutputs)
////            }
////            guard !partialOutputs.isEmpty else {
////                return []
////            }
////        }
////
////        var finalOutputs = partialOutputs//[(tokens: [MatchedToken<TokenType>], remaining: Substring)]()
////        var count = allowedNumberOfRepeats.lowerBound
////
////        while count < allowedNumberOfRepeats.upperBound {
////            //            var combinedOutputs = [(tokens: [MatchedToken<TokenType>], remaining: Substring)]()
////            let previousPartialOutputs = partialOutputs
////            partialOutputs.removeAll()
////            for partialOutput in previousPartialOutputs {
////                let nextOutputs = pattern.consuming(input: partialOutput.remaining)
////                guard !nextOutputs.isEmpty else {
////                    continue
////                }
////                let combinedPartialOutputs = nextOutputs.map { (tokens: partialOutput.tokens + $0.tokens, remaining: $0.remaining) }
////                finalOutputs.append(contentsOf: combinedPartialOutputs)
////                partialOutputs.append(contentsOf: combinedPartialOutputs)
////            }
////            count += 1
////        }
////        return finalOutputs
////    }
////}
////extension GrammarPattern {
////    func repeating(count: PartialRangeFrom<UInt>) -> RepeatingGrammarPattern<Self> {
////        return RepeatingGrammarPattern(pattern: self, count: count)
////    }
////    func repeating(count: ClosedRange<UInt>) -> RepeatingGrammarPattern<Self> {
////        return RepeatingGrammarPattern(pattern: self, count: count)
////    }
////    func repeating(count: Range<UInt>) -> RepeatingGrammarPattern<Self> {
////        return RepeatingGrammarPattern(pattern: self, count: count)
////    }
////    func repeating(count: UInt) -> RepeatingGrammarPattern<Self> {
////        return RepeatingGrammarPattern(pattern: self, count: count)
////    }
////}
//
//fileprivate class _AnyGrammarPatternBoxBase<T>: GrammarPattern where T: Parsable {
//    typealias TokenType = T
//
//    func consuming<T>(input: T) -> (tokens: [MatchedToken<TokenType>], remaining: Substring, returnPoint: GrammarReturnPoint<TokenType>?)? where T : StringProtocol, T.SubSequence == Substring {
//        fatalError()
//    }
//}
//fileprivate class _AnyGrammarPatternBox<T: GrammarPattern>: _AnyGrammarPatternBoxBase<T.TokenType> {
//    let base: T
//    init(_ base: T) {
//        self.base = base
//    }
//    override func consuming<T>(input: T) -> (tokens: [MatchedToken<TokenType>], remaining: Substring, returnPoint: GrammarReturnPoint<TokenType>?)? where T : StringProtocol, T.SubSequence == Substring {
//        return base.consuming(input: input)
//    }
//}
//class AnyGrammarPattern<U>: GrammarPattern where U: Parsable {
//    typealias TokenType = U
//    private let box: _AnyGrammarPatternBoxBase<U>
//
//    init<P>(_ base: P) where P: GrammarPattern, P.TokenType == TokenType {
//        self.box = _AnyGrammarPatternBox(base)
//    }
//    func consuming<T>(input: T) -> (tokens: [MatchedToken<U>], remaining: Substring, returnPoint: GrammarReturnPoint<U>?)? where T : StringProtocol, T.SubSequence == Substring {
//        return box.consuming(input: input)
//    }
//}
//
//class RecursiveGrammarPattern<U>: GrammarPattern where U: Parsable {
//    typealias TokenType = U
//
//    var pattern: AnyGrammarPattern<U>!
//    init<P>(_ recursivePattern: (_ selfPattern: RecursiveGrammarPattern<U>) -> P) where P: GrammarPattern, P.TokenType == U {
//        self.pattern = AnyGrammarPattern(recursivePattern(self))
//    }
//    func consuming<T>(input: T) -> (tokens: [MatchedToken<U>], remaining: Substring, returnPoint: GrammarReturnPoint<U>?)? where T : StringProtocol, T.SubSequence == Substring {
//        return pattern.consuming(input: input)
//    }
//}
//
//class SharedGrammarPattern<U>: GrammarPattern where U: Parsable {
//    typealias TokenType = U
//
//    var pattern: AnyGrammarPattern<U>
//
//    init() {
//        pattern = AnyGrammarPattern(NeverGrammarPattern())
//    }
//    init<P>(_ pattern: P) where P: GrammarPattern, P.TokenType == U {
//        self.pattern = AnyGrammarPattern(pattern)
//    }
//
//    func consuming<T>(input: T) -> (tokens: [MatchedToken<U>], remaining: Substring, returnPoint: GrammarReturnPoint<U>?)? where T : StringProtocol, T.SubSequence == Substring {
//        return pattern.consuming(input: input)
//    }
//}
//
//struct NeverGrammarPattern<U>: GrammarPattern where U: Parsable {
//    init() { }
//    func consuming<T>(input: T) -> (tokens: [MatchedToken<U>], remaining: Substring, returnPoint: GrammarReturnPoint<U>?)? where T : StringProtocol, T.SubSequence == Substring {
//        return nil
//    }
//}



protocol GrammarPattern {
    associatedtype TokenType: Parsable
    func consuming<T>(input: T) -> (tokens: [MatchedToken<TokenType>], remaining: Substring, returnPoint: GrammarReturnPoint<TokenType>?)? where T : StringProtocol, T.SubSequence == Substring
}
class GrammarReturnPoint<U>: GrammarPattern where U: Parsable {
    typealias TokenType = U
    private(set) var pattern: AnyGrammarPattern<U>
    var consumedTokens: [MatchedToken<TokenType>]
    var input: Substring

    init<P, Q>(_ pattern: P, input: Q, consumedTokens: [MatchedToken<TokenType>]) where P: GrammarPattern, P.TokenType == TokenType, Q: StringProtocol, Q.SubSequence == Substring {
        self.pattern = AnyGrammarPattern(pattern)
        self.input = input[...]
        self.consumedTokens = consumedTokens
    }
    func attemptConsume() -> (tokens: [MatchedToken<TokenType>], remaining: Substring, returnPoint: GrammarReturnPoint<TokenType>?)? {
        guard var output = pattern.consuming(input: input) else {
            return nil
        }
        output.tokens = consumedTokens + output.tokens
        return output
    }
    func consuming<T>(input: T) -> (tokens: [MatchedToken<TokenType>], remaining: Substring, returnPoint: GrammarReturnPoint<TokenType>?)? where T : StringProtocol, T.SubSequence == Substring {
        guard var output = pattern.consuming(input: input) else {
            return nil
        }
        output.tokens = consumedTokens + output.tokens
        return output
    }
    func updatePattern<P>(to newPattern: (AnyGrammarPattern<U>) -> P) where P: GrammarPattern, P.TokenType == TokenType {
        self.pattern = AnyGrammarPattern(newPattern(pattern))
    }


}

struct EmptyGrammarPattern<U>: GrammarPattern where U: Parsable {
    typealias TokenType = U
    func consuming<T>(input: T) -> (tokens: [MatchedToken<U>], remaining: Substring, returnPoint: GrammarReturnPoint<U>?)? where T : StringProtocol, T.SubSequence == Substring {
        return (tokens: [], remaining: input[...], returnPoint: nil)
    }
}
struct LogicalOrGrammarPattern<P1, P2>: GrammarPattern where P1: GrammarPattern, P2: GrammarPattern, P1.TokenType == P2.TokenType {
    var leftPattern: P1
    var rightPattern: P2
    typealias TokenType = P1.TokenType

    init(left: P1, right: P2) {
        self.leftPattern = left
        self.rightPattern = right
    }

    func consuming<T>(input: T) -> (tokens: [MatchedToken<TokenType>], remaining: Substring, returnPoint: GrammarReturnPoint<TokenType>?)? where T: StringProtocol, T.SubSequence == Substring {
        if let leftOutput = leftPattern.consuming(input: input) {
            leftOutput.returnPoint?.updatePattern { $0 || rightPattern }
            let returnPoint = leftOutput.returnPoint ?? .init(rightPattern, input: input, consumedTokens: [])
            return (tokens: leftOutput.tokens, remaining: leftOutput.remaining, returnPoint: returnPoint)
        } else if let rightOutput = rightPattern.consuming(input: input) {
            return (tokens: rightOutput.tokens, remaining: rightOutput.remaining, returnPoint: rightOutput.returnPoint)
        }
        return nil
    }
}
func ||<T, U>(lhs: T, rhs: U) -> LogicalOrGrammarPattern<T, U> where T: GrammarPattern, U: GrammarPattern, T.TokenType == U.TokenType {
    return LogicalOrGrammarPattern(left: lhs, right: rhs)
}
struct OptionalGrammarPattern<T> where T: GrammarPattern {
    typealias TokenType = T.TokenType

    var pattern: T
    init(_ pattern: T) {
        self.pattern = pattern
    }
}
extension OptionalGrammarPattern: GrammarPattern {
    func consuming<T>(input: T) -> (tokens: [MatchedToken<TokenType>], remaining: Substring, returnPoint: GrammarReturnPoint<TokenType>?)? where T : StringProtocol, T.SubSequence == Substring {
        guard let output = pattern.consuming(input: input) else {
            return (tokens: [], remaining: input[...], returnPoint: nil)
        }
        output.returnPoint?.updatePattern { $0.opt() }
        let returnPoint = output.returnPoint ?? .init(EmptyGrammarPattern(), input: input, consumedTokens: [])
        return (tokens: output.tokens, remaining: output.remaining, returnPoint: returnPoint)
    }
}
extension GrammarPattern {
    func opt() -> OptionalGrammarPattern<Self> {
        return OptionalGrammarPattern(self)
    }
}

struct SequentialGrammarPattern<P1, P2>: GrammarPattern where P1: GrammarPattern, P2: GrammarPattern, P1.TokenType == P2.TokenType {
    typealias TokenType = P1.TokenType

    var pattern1: P1
    var pattern2: P2

    func consuming<T>(input: T) -> (tokens: [MatchedToken<P1.TokenType>], remaining: Substring, returnPoint: GrammarReturnPoint<TokenType>?)? where T : StringProtocol, T.SubSequence == Substring {
        guard let output = pattern1.consuming(input: input) else {
            return nil
        }
        output.returnPoint?.updatePattern { $0 + pattern2 }
        if var nextOutput = pattern2.consuming(input: output.remaining) {
            if let returnPoint = output.returnPoint {
                nextOutput.returnPoint = returnPoint
            } else {
                nextOutput.returnPoint?.consumedTokens += output.tokens
            }
            nextOutput.tokens = output.tokens + nextOutput.tokens
            return nextOutput
        } else if let returnPoint = output.returnPoint {
            return returnPoint.attemptConsume()
        }
        return nil
    }
}
func +<T, U>(lhs: T, rhs: U) -> SequentialGrammarPattern<T, U> where T: GrammarPattern, U: GrammarPattern, T.TokenType == U.TokenType {
    return SequentialGrammarPattern(pattern1: lhs, pattern2: rhs)
}
//
//struct RepeatingGrammarPattern<T>: GrammarPattern where T: GrammarPattern {
//    typealias TokenType = T.TokenType
//    var pattern: T
//    var allowedNumberOfRepeats: Range<UInt>
//
//    init(pattern: T, count: PartialRangeFrom<UInt>) {
//        self.pattern = pattern
//        self.allowedNumberOfRepeats = count.lowerBound..<UInt.max
//    }
//    init(pattern: T, count: ClosedRange<UInt>) {
//        self.pattern = pattern
//        self.allowedNumberOfRepeats = count.lowerBound..<(count.upperBound == UInt.max ? count.upperBound : count.upperBound + 1)
//    }
//    init(pattern: T, count: Range<UInt>) {
//        self.pattern = pattern
//        self.allowedNumberOfRepeats = count
//    }
//    init(pattern: T, count: UInt) {
//        self.pattern = pattern
//        self.allowedNumberOfRepeats = count..<(count == UInt.max ? count : count + 1)
//    }
//
//    func consuming<T>(input: T) -> [(tokens: [MatchedToken<TokenType>], remaining: Substring)] where T : StringProtocol, T.SubSequence == Substring {
//        //        var finalOutputs = [(tokens: [MatchedToken<TokenType>], remaining: Substring)]()
//        var partialOutputs: [(tokens: [MatchedToken<TokenType>], remaining: Substring)]
//        partialOutputs = [(tokens: [MatchedToken<TokenType>](), remaining: input[...])]
//        //        var tokens = [MatchedToken<TokenType>]()
//        //        var remaining = input[...]
//        for _ in 0..<allowedNumberOfRepeats.lowerBound {
//            let previousPartialOutputs = partialOutputs
//            partialOutputs.removeAll()
//            for partialOutput in previousPartialOutputs {
//                // String completed before all was matched
//                let nextOutputs = pattern.consuming(input: partialOutput.remaining)
//                guard !nextOutputs.isEmpty else {
//                    continue
//                }
//                let combinedPartialOutputs = nextOutputs.map { (tokens: partialOutput.tokens + $0.tokens, remaining: $0.remaining) }
//                partialOutputs.append(contentsOf: combinedPartialOutputs)
//            }
//            guard !partialOutputs.isEmpty else {
//                return []
//            }
//        }
//
//        var finalOutputs = partialOutputs//[(tokens: [MatchedToken<TokenType>], remaining: Substring)]()
//        var count = allowedNumberOfRepeats.lowerBound
//
//        while count < allowedNumberOfRepeats.upperBound {
//            //            var combinedOutputs = [(tokens: [MatchedToken<TokenType>], remaining: Substring)]()
//            let previousPartialOutputs = partialOutputs
//            partialOutputs.removeAll()
//            for partialOutput in previousPartialOutputs {
//                let nextOutputs = pattern.consuming(input: partialOutput.remaining)
//                guard !nextOutputs.isEmpty else {
//                    continue
//                }
//                let combinedPartialOutputs = nextOutputs.map { (tokens: partialOutput.tokens + $0.tokens, remaining: $0.remaining) }
//                finalOutputs.append(contentsOf: combinedPartialOutputs)
//                partialOutputs.append(contentsOf: combinedPartialOutputs)
//            }
//            count += 1
//        }
//        return finalOutputs
//    }
//}
//extension GrammarPattern {
//    func repeating(count: PartialRangeFrom<UInt>) -> RepeatingGrammarPattern<Self> {
//        return RepeatingGrammarPattern(pattern: self, count: count)
//    }
//    func repeating(count: ClosedRange<UInt>) -> RepeatingGrammarPattern<Self> {
//        return RepeatingGrammarPattern(pattern: self, count: count)
//    }
//    func repeating(count: Range<UInt>) -> RepeatingGrammarPattern<Self> {
//        return RepeatingGrammarPattern(pattern: self, count: count)
//    }
//    func repeating(count: UInt) -> RepeatingGrammarPattern<Self> {
//        return RepeatingGrammarPattern(pattern: self, count: count)
//    }
//}

fileprivate class _AnyGrammarPatternBoxBase<T>: GrammarPattern where T: Parsable {
    typealias TokenType = T

    func consuming<T>(input: T) -> (tokens: [MatchedToken<TokenType>], remaining: Substring, returnPoint: GrammarReturnPoint<TokenType>?)? where T : StringProtocol, T.SubSequence == Substring {
        fatalError()
    }
}
fileprivate class _AnyGrammarPatternBox<T: GrammarPattern>: _AnyGrammarPatternBoxBase<T.TokenType> {
    let base: T
    init(_ base: T) {
        self.base = base
    }
    override func consuming<T>(input: T) -> (tokens: [MatchedToken<TokenType>], remaining: Substring, returnPoint: GrammarReturnPoint<TokenType>?)? where T : StringProtocol, T.SubSequence == Substring {
        return base.consuming(input: input)
    }
}
class AnyGrammarPattern<U>: GrammarPattern where U: Parsable {
    typealias TokenType = U
    private let box: _AnyGrammarPatternBoxBase<U>

    init<P>(_ base: P) where P: GrammarPattern, P.TokenType == TokenType {
        self.box = _AnyGrammarPatternBox(base)
    }
    func consuming<T>(input: T) -> (tokens: [MatchedToken<U>], remaining: Substring, returnPoint: GrammarReturnPoint<U>?)? where T : StringProtocol, T.SubSequence == Substring {
        return box.consuming(input: input)
    }
}

class RecursiveGrammarPattern<U>: GrammarPattern where U: Parsable {
    typealias TokenType = U

    var pattern: AnyGrammarPattern<U>!
    init<P>(_ recursivePattern: (_ selfPattern: RecursiveGrammarPattern<U>) -> P) where P: GrammarPattern, P.TokenType == U {
        self.pattern = AnyGrammarPattern(recursivePattern(self))
    }
    func consuming<T>(input: T) -> (tokens: [MatchedToken<U>], remaining: Substring, returnPoint: GrammarReturnPoint<U>?)? where T : StringProtocol, T.SubSequence == Substring {
        return pattern.consuming(input: input)
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

    func consuming<T>(input: T) -> (tokens: [MatchedToken<U>], remaining: Substring, returnPoint: GrammarReturnPoint<U>?)? where T : StringProtocol, T.SubSequence == Substring {
        return pattern.consuming(input: input)
    }
}

struct NeverGrammarPattern<U>: GrammarPattern where U: Parsable {
    init() { }
    func consuming<T>(input: T) -> (tokens: [MatchedToken<U>], remaining: Substring, returnPoint: GrammarReturnPoint<U>?)? where T : StringProtocol, T.SubSequence == Substring {
        return nil
    }
}



// Grammar Return Point Variation
//protocol GrammarPattern {
//    associatedtype TokenType: Parsable
//    func consuming<T>(input: T) -> (tokens: [MatchedToken<TokenType>], remaining: Substring, returnPoint: GrammarReturnPoint<TokenType>?)? where T : StringProtocol, T.SubSequence == Substring
//}
//
//class GrammarReturnPoint<U>: GrammarPattern where U: Parsable {
//    typealias TokenType = U
//    private(set) var pattern: AnyGrammarPattern<U>
//    var consumedTokens: [MatchedToken<TokenType>]
//    var input: Substring
//
//    init<P, Q>(_ pattern: P, input: Q, consumedTokens: [MatchedToken<TokenType>]) where P: GrammarPattern, P.TokenType == TokenType, Q: StringProtocol, Q.SubSequence == Substring {
//        self.pattern = AnyGrammarPattern(pattern)
//        self.input = input[...]
//        self.consumedTokens = consumedTokens
//    }
//    func attemptConsume() -> (tokens: [MatchedToken<TokenType>], remaining: Substring, returnPoint: GrammarReturnPoint<TokenType>?)? {
//        guard var output = pattern.consuming(input: input) else {
//            return nil
//        }
//        output.tokens = consumedTokens + output.tokens
//        return output
//    }
//    func consuming<T>(input: T) -> (tokens: [MatchedToken<TokenType>], remaining: Substring, returnPoint: GrammarReturnPoint<TokenType>?)? where T : StringProtocol, T.SubSequence == Substring {
//        guard var output = pattern.consuming(input: input) else {
//            return nil
//        }
//        output.tokens = consumedTokens + output.tokens
//        return output
//    }
//    func updatePattern<P>(to newPattern: (AnyGrammarPattern<U>) -> P) where P: GrammarPattern, P.TokenType == TokenType {
//        self.pattern = AnyGrammarPattern(newPattern(pattern))
//    }
//
//
//}
//
//struct EmptyGrammarPattern<U>: GrammarPattern where U: Parsable {
//    typealias TokenType = U
//    func consuming<T>(input: T) -> (tokens: [MatchedToken<U>], remaining: Substring, returnPoint: GrammarReturnPoint<U>?)? where T : StringProtocol, T.SubSequence == Substring {
//        return (tokens: [], remaining: input[...], returnPoint: nil)
//    }
//}
//struct LogicalOrGrammarPattern<P1, P2>: GrammarPattern where P1: GrammarPattern, P2: GrammarPattern, P1.TokenType == P2.TokenType {
//    var leftPattern: P1
//    var rightPattern: P2
//    typealias TokenType = P1.TokenType
//
//    init(left: P1, right: P2) {
//        self.leftPattern = left
//        self.rightPattern = right
//    }
//
//    func consuming<T>(input: T) -> (tokens: [MatchedToken<TokenType>], remaining: Substring, returnPoint: GrammarReturnPoint<TokenType>?)? where T: StringProtocol, T.SubSequence == Substring {
//        if let leftOutput = leftPattern.consuming(input: input) {
//            leftOutput.returnPoint?.updatePattern { $0 || rightPattern }
//            let returnPoint = leftOutput.returnPoint ?? .init(rightPattern, input: input, consumedTokens: [])
//            return (tokens: leftOutput.tokens, remaining: leftOutput.remaining, returnPoint: returnPoint)
//        } else if let rightOutput = rightPattern.consuming(input: input) {
//            return (tokens: rightOutput.tokens, remaining: rightOutput.remaining, returnPoint: rightOutput.returnPoint)
//        }
//        return nil
//    }
//}
//func ||<T, U>(lhs: T, rhs: U) -> LogicalOrGrammarPattern<T, U> where T: GrammarPattern, U: GrammarPattern, T.TokenType == U.TokenType {
//    return LogicalOrGrammarPattern(left: lhs, right: rhs)
//}
////+<||<T, T>, T>
//struct OptionalGrammarPattern<T> where T: GrammarPattern {
//    typealias TokenType = T.TokenType
//
//    var pattern: T
//    init(_ pattern: T) {
//        self.pattern = pattern
//    }
//}
//extension OptionalGrammarPattern: GrammarPattern {
//    func consuming<T>(input: T) -> (tokens: [MatchedToken<TokenType>], remaining: Substring, returnPoint: GrammarReturnPoint<TokenType>?)? where T : StringProtocol, T.SubSequence == Substring {
//        guard let output = pattern.consuming(input: input) else {
//            return (tokens: [], remaining: input[...], returnPoint: nil)
//        }
//        output.returnPoint?.updatePattern { $0.opt() }
//        let returnPoint = output.returnPoint ?? .init(EmptyGrammarPattern(), input: input, consumedTokens: [])
//        return (tokens: output.tokens, remaining: output.remaining, returnPoint: returnPoint)
//    }
//}
//extension GrammarPattern {
//    func opt() -> OptionalGrammarPattern<Self> {
//        return OptionalGrammarPattern(self)
//    }
//}
//
//struct SequentialGrammarPattern<P1, P2>: GrammarPattern where P1: GrammarPattern, P2: GrammarPattern, P1.TokenType == P2.TokenType {
//    typealias TokenType = P1.TokenType
//
//    var pattern1: P1
//    var pattern2: P2
//
//    func consuming<T>(input: T) -> (tokens: [MatchedToken<P1.TokenType>], remaining: Substring, returnPoint: GrammarReturnPoint<TokenType>?)? where T : StringProtocol, T.SubSequence == Substring {
//        guard let output = pattern1.consuming(input: input) else {
//            return nil
//        }
//        output.returnPoint?.updatePattern { $0 + pattern2 }
//        if var nextOutput = pattern2.consuming(input: output.remaining) {
//            if let returnPoint = output.returnPoint {
//                nextOutput.returnPoint = returnPoint
//            } else {
//                nextOutput.returnPoint?.consumedTokens += output.tokens
//            }
//            nextOutput.tokens = output.tokens + nextOutput.tokens
//            return nextOutput
//        } else if let returnPoint = output.returnPoint {
//            return returnPoint.attemptConsume()
//        }
//        return nil
//    }
//}
//func +<T, U>(lhs: T, rhs: U) -> SequentialGrammarPattern<T, U> where T: GrammarPattern, U: GrammarPattern, T.TokenType == U.TokenType {
//    return SequentialGrammarPattern(pattern1: lhs, pattern2: rhs)
//}
////
////struct RepeatingGrammarPattern<T>: GrammarPattern where T: GrammarPattern {
////    typealias TokenType = T.TokenType
////    var pattern: T
////    var allowedNumberOfRepeats: Range<UInt>
////
////    init(pattern: T, count: PartialRangeFrom<UInt>) {
////        self.pattern = pattern
////        self.allowedNumberOfRepeats = count.lowerBound..<UInt.max
////    }
////    init(pattern: T, count: ClosedRange<UInt>) {
////        self.pattern = pattern
////        self.allowedNumberOfRepeats = count.lowerBound..<(count.upperBound == UInt.max ? count.upperBound : count.upperBound + 1)
////    }
////    init(pattern: T, count: Range<UInt>) {
////        self.pattern = pattern
////        self.allowedNumberOfRepeats = count
////    }
////    init(pattern: T, count: UInt) {
////        self.pattern = pattern
////        self.allowedNumberOfRepeats = count..<(count == UInt.max ? count : count + 1)
////    }
////
////    func consuming<T>(input: T) -> [(tokens: [MatchedToken<TokenType>], remaining: Substring)] where T : StringProtocol, T.SubSequence == Substring {
////        //        var finalOutputs = [(tokens: [MatchedToken<TokenType>], remaining: Substring)]()
////        var partialOutputs: [(tokens: [MatchedToken<TokenType>], remaining: Substring)]
////        partialOutputs = [(tokens: [MatchedToken<TokenType>](), remaining: input[...])]
////        //        var tokens = [MatchedToken<TokenType>]()
////        //        var remaining = input[...]
////        for _ in 0..<allowedNumberOfRepeats.lowerBound {
////            let previousPartialOutputs = partialOutputs
////            partialOutputs.removeAll()
////            for partialOutput in previousPartialOutputs {
////                // String completed before all was matched
////                let nextOutputs = pattern.consuming(input: partialOutput.remaining)
////                guard !nextOutputs.isEmpty else {
////                    continue
////                }
////                let combinedPartialOutputs = nextOutputs.map { (tokens: partialOutput.tokens + $0.tokens, remaining: $0.remaining) }
////                partialOutputs.append(contentsOf: combinedPartialOutputs)
////            }
////            guard !partialOutputs.isEmpty else {
////                return []
////            }
////        }
////
////        var finalOutputs = partialOutputs//[(tokens: [MatchedToken<TokenType>], remaining: Substring)]()
////        var count = allowedNumberOfRepeats.lowerBound
////
////        while count < allowedNumberOfRepeats.upperBound {
////            //            var combinedOutputs = [(tokens: [MatchedToken<TokenType>], remaining: Substring)]()
////            let previousPartialOutputs = partialOutputs
////            partialOutputs.removeAll()
////            for partialOutput in previousPartialOutputs {
////                let nextOutputs = pattern.consuming(input: partialOutput.remaining)
////                guard !nextOutputs.isEmpty else {
////                    continue
////                }
////                let combinedPartialOutputs = nextOutputs.map { (tokens: partialOutput.tokens + $0.tokens, remaining: $0.remaining) }
////                finalOutputs.append(contentsOf: combinedPartialOutputs)
////                partialOutputs.append(contentsOf: combinedPartialOutputs)
////            }
////            count += 1
////        }
////        return finalOutputs
////    }
////}
////extension GrammarPattern {
////    func repeating(count: PartialRangeFrom<UInt>) -> RepeatingGrammarPattern<Self> {
////        return RepeatingGrammarPattern(pattern: self, count: count)
////    }
////    func repeating(count: ClosedRange<UInt>) -> RepeatingGrammarPattern<Self> {
////        return RepeatingGrammarPattern(pattern: self, count: count)
////    }
////    func repeating(count: Range<UInt>) -> RepeatingGrammarPattern<Self> {
////        return RepeatingGrammarPattern(pattern: self, count: count)
////    }
////    func repeating(count: UInt) -> RepeatingGrammarPattern<Self> {
////        return RepeatingGrammarPattern(pattern: self, count: count)
////    }
////}
//
//fileprivate class _AnyGrammarPatternBoxBase<T>: GrammarPattern where T: Parsable {
//    typealias TokenType = T
//
//    func consuming<T>(input: T) -> (tokens: [MatchedToken<TokenType>], remaining: Substring, returnPoint: GrammarReturnPoint<TokenType>?)? where T : StringProtocol, T.SubSequence == Substring {
//        fatalError()
//    }
//}
//fileprivate class _AnyGrammarPatternBox<T: GrammarPattern>: _AnyGrammarPatternBoxBase<T.TokenType> {
//    let base: T
//    init(_ base: T) {
//        self.base = base
//    }
//    override func consuming<T>(input: T) -> (tokens: [MatchedToken<TokenType>], remaining: Substring, returnPoint: GrammarReturnPoint<TokenType>?)? where T : StringProtocol, T.SubSequence == Substring {
//        return base.consuming(input: input)
//    }
//}
//class AnyGrammarPattern<U>: GrammarPattern where U: Parsable {
//    typealias TokenType = U
//    private let box: _AnyGrammarPatternBoxBase<U>
//
//    init<P>(_ base: P) where P: GrammarPattern, P.TokenType == TokenType {
//        self.box = _AnyGrammarPatternBox(base)
//    }
//    func consuming<T>(input: T) -> (tokens: [MatchedToken<U>], remaining: Substring, returnPoint: GrammarReturnPoint<U>?)? where T : StringProtocol, T.SubSequence == Substring {
//        return box.consuming(input: input)
//    }
//}
//
//class RecursiveGrammarPattern<U>: GrammarPattern where U: Parsable {
//    typealias TokenType = U
//
//    var pattern: AnyGrammarPattern<U>!
//    init<P>(_ recursivePattern: (_ selfPattern: RecursiveGrammarPattern<U>) -> P) where P: GrammarPattern, P.TokenType == U {
//        self.pattern = AnyGrammarPattern(recursivePattern(self))
//    }
//    func consuming<T>(input: T) -> (tokens: [MatchedToken<U>], remaining: Substring, returnPoint: GrammarReturnPoint<U>?)? where T : StringProtocol, T.SubSequence == Substring {
//        return pattern.consuming(input: input)
//    }
//}
//
//class SharedGrammarPattern<U>: GrammarPattern where U: Parsable {
//    typealias TokenType = U
//
//    var pattern: AnyGrammarPattern<U>
//
//    init() {
//        pattern = AnyGrammarPattern(NeverGrammarPattern())
//    }
//    init<P>(_ pattern: P) where P: GrammarPattern, P.TokenType == U {
//        self.pattern = AnyGrammarPattern(pattern)
//    }
//
//    func consuming<T>(input: T) -> (tokens: [MatchedToken<U>], remaining: Substring, returnPoint: GrammarReturnPoint<U>?)? where T : StringProtocol, T.SubSequence == Substring {
//        return pattern.consuming(input: input)
//    }
//}
//
//struct NeverGrammarPattern<U>: GrammarPattern where U: Parsable {
//    init() { }
//    func consuming<T>(input: T) -> (tokens: [MatchedToken<U>], remaining: Substring, returnPoint: GrammarReturnPoint<U>?)? where T : StringProtocol, T.SubSequence == Substring {
//        return nil
//    }
//}

// Grammar Array Variation
//protocol GrammarPattern {
//    associatedtype TokenType: Parsable
//    func consuming<T>(input: T) -> [(tokens: [MatchedToken<TokenType>], remaining: Substring)] where T : StringProtocol, T.SubSequence == Substring
//}
//
//
//struct LogicalOrGrammarPattern<P1, P2>: GrammarPattern where P1: GrammarPattern, P2: GrammarPattern, P1.TokenType == P2.TokenType {
//    var leftPattern: P1
//    var rightPattern: P2
//    typealias TokenType = P1.TokenType
//
//    init(left: P1, right: P2) {
//        self.leftPattern = left
//        self.rightPattern = right
//    }
//
//    func consuming<T>(input: T) -> [(tokens: [MatchedToken<TokenType>], remaining: Substring)] where T: StringProtocol, T.SubSequence == Substring {
//        let leftOutput = leftPattern.consuming(input: input)
//        let rightOutput = rightPattern.consuming(input: input)
//        return leftOutput + rightOutput
//    }
//}
//func ||<T, U>(lhs: T, rhs: U) -> LogicalOrGrammarPattern<T, U> where T: GrammarPattern, U: GrammarPattern, T.TokenType == U.TokenType {
//    return LogicalOrGrammarPattern(left: lhs, right: rhs)
//}
//
//struct OptionalGrammarPattern<T> where T: GrammarPattern {
//    typealias TokenType = T.TokenType
//
//    var pattern: T
//    init(_ pattern: T) {
//        self.pattern = pattern
//    }
//}
//extension OptionalGrammarPattern: GrammarPattern {
//    func consuming<T>(input: T) -> [(tokens: [MatchedToken<TokenType>], remaining: Substring)] where T : StringProtocol, T.SubSequence == Substring {
//        let output = pattern.consuming(input: input)
//        return output + [(tokens: [], remaining: input[...])]
//    }
//}
//extension GrammarPattern {
//    func opt() -> OptionalGrammarPattern<Self> {
//        return OptionalGrammarPattern(self)
//    }
//}
//
//struct SequentialGrammarPattern<P1, P2>: GrammarPattern where P1: GrammarPattern, P2: GrammarPattern, P1.TokenType == P2.TokenType {
//    typealias TokenType = P1.TokenType
//
//    var pattern1: P1
//    var pattern2: P2
//
//    func consuming<T>(input: T) -> [(tokens: [MatchedToken<P1.TokenType>], remaining: Substring)] where T : StringProtocol, T.SubSequence == Substring {
//        var finalOutputs = [(tokens: [MatchedToken<P1.TokenType>], remaining: Substring)]()
//        let partialOutputs = pattern1.consuming(input: input)
//        for partialOutput in partialOutputs {
//            let outputs = pattern2.consuming(input: partialOutput.remaining)
//            let combinedOutputs = outputs.map { (tokens: partialOutput.tokens + $0.tokens, remaining: $0.remaining) }
//            finalOutputs.append(contentsOf: combinedOutputs)
//        }
//        return finalOutputs
//    }
//}
//func +<T, U>(lhs: T, rhs: U) -> SequentialGrammarPattern<T, U> where T: GrammarPattern, U: GrammarPattern, T.TokenType == U.TokenType {
//    return SequentialGrammarPattern(pattern1: lhs, pattern2: rhs)
//}
//
//struct RepeatingGrammarPattern<T>: GrammarPattern where T: GrammarPattern {
//    typealias TokenType = T.TokenType
//    var pattern: T
//    var allowedNumberOfRepeats: Range<UInt>
//
//    init(pattern: T, count: PartialRangeFrom<UInt>) {
//        self.pattern = pattern
//        self.allowedNumberOfRepeats = count.lowerBound..<UInt.max
//    }
//    init(pattern: T, count: ClosedRange<UInt>) {
//        self.pattern = pattern
//        self.allowedNumberOfRepeats = count.lowerBound..<(count.upperBound == UInt.max ? count.upperBound : count.upperBound + 1)
//    }
//    init(pattern: T, count: Range<UInt>) {
//        self.pattern = pattern
//        self.allowedNumberOfRepeats = count
//    }
//    init(pattern: T, count: UInt) {
//        self.pattern = pattern
//        self.allowedNumberOfRepeats = count..<(count == UInt.max ? count : count + 1)
//    }
//
//    func consuming<T>(input: T) -> [(tokens: [MatchedToken<TokenType>], remaining: Substring)] where T : StringProtocol, T.SubSequence == Substring {
////        var finalOutputs = [(tokens: [MatchedToken<TokenType>], remaining: Substring)]()
//        var partialOutputs: [(tokens: [MatchedToken<TokenType>], remaining: Substring)]
//        partialOutputs = [(tokens: [MatchedToken<TokenType>](), remaining: input[...])]
////        var tokens = [MatchedToken<TokenType>]()
////        var remaining = input[...]
//        for _ in 0..<allowedNumberOfRepeats.lowerBound {
//            let previousPartialOutputs = partialOutputs
//            partialOutputs.removeAll()
//            for partialOutput in previousPartialOutputs {
//                // String completed before all was matched
//                let nextOutputs = pattern.consuming(input: partialOutput.remaining)
//                guard !nextOutputs.isEmpty else {
//                    continue
//                }
//                let combinedPartialOutputs = nextOutputs.map { (tokens: partialOutput.tokens + $0.tokens, remaining: $0.remaining) }
//                partialOutputs.append(contentsOf: combinedPartialOutputs)
//            }
//            guard !partialOutputs.isEmpty else {
//                return []
//            }
//        }
//
//        var finalOutputs = partialOutputs//[(tokens: [MatchedToken<TokenType>], remaining: Substring)]()
//        var count = allowedNumberOfRepeats.lowerBound
//
//        while count < allowedNumberOfRepeats.upperBound {
////            var combinedOutputs = [(tokens: [MatchedToken<TokenType>], remaining: Substring)]()
//            let previousPartialOutputs = partialOutputs
//            partialOutputs.removeAll()
//            for partialOutput in previousPartialOutputs {
//                let nextOutputs = pattern.consuming(input: partialOutput.remaining)
//                guard !nextOutputs.isEmpty else {
//                    continue
//                }
//                let combinedPartialOutputs = nextOutputs.map { (tokens: partialOutput.tokens + $0.tokens, remaining: $0.remaining) }
//                finalOutputs.append(contentsOf: combinedPartialOutputs)
//                partialOutputs.append(contentsOf: combinedPartialOutputs)
//            }
//            count += 1
//        }
//        return finalOutputs
//    }
//}
//extension GrammarPattern {
//    func repeating(count: PartialRangeFrom<UInt>) -> RepeatingGrammarPattern<Self> {
//        return RepeatingGrammarPattern(pattern: self, count: count)
//    }
//    func repeating(count: ClosedRange<UInt>) -> RepeatingGrammarPattern<Self> {
//        return RepeatingGrammarPattern(pattern: self, count: count)
//    }
//    func repeating(count: Range<UInt>) -> RepeatingGrammarPattern<Self> {
//        return RepeatingGrammarPattern(pattern: self, count: count)
//    }
//    func repeating(count: UInt) -> RepeatingGrammarPattern<Self> {
//        return RepeatingGrammarPattern(pattern: self, count: count)
//    }
//}
//
//fileprivate class _AnyGrammarPatternBoxBase<T>: GrammarPattern where T: Parsable {
//    typealias TokenType = T
//
//    func consuming<T>(input: T) -> [(tokens: [MatchedToken<TokenType>], remaining: Substring)] where T : StringProtocol, T.SubSequence == Substring {
//        fatalError()
//    }
//}
//fileprivate class _AnyGrammarPatternBox<T: GrammarPattern>: _AnyGrammarPatternBoxBase<T.TokenType> {
//    let base: T
//    init(_ base: T) {
//        self.base = base
//    }
//    override func consuming<T>(input: T) -> [(tokens: [MatchedToken<TokenType>], remaining: Substring)] where T : StringProtocol, T.SubSequence == Substring {
//        return base.consuming(input: input)
//    }
//}
//class AnyGrammarPattern<U>: GrammarPattern where U: Parsable {
//    typealias TokenType = U
//    private let box: _AnyGrammarPatternBoxBase<U>
//
//    init<P>(_ base: P) where P: GrammarPattern, P.TokenType == TokenType {
//        self.box = _AnyGrammarPatternBox(base)
//    }
//    func consuming<T>(input: T) -> [(tokens: [MatchedToken<TokenType>], remaining: Substring)] where T : StringProtocol, T.SubSequence == Substring {
//        return box.consuming(input: input)
//    }
//}
//
//class RecursiveGrammarPattern<U>: GrammarPattern where U: Parsable {
//    typealias TokenType = U
//
//    var pattern: AnyGrammarPattern<U>!
//    init<P>(_ recursivePattern: (_ selfPattern: RecursiveGrammarPattern<U>) -> P) where P: GrammarPattern, P.TokenType == U {
//        self.pattern = AnyGrammarPattern(recursivePattern(self))
//    }
//    func consuming<T>(input: T) -> [(tokens: [MatchedToken<U>], remaining: Substring)] where T : StringProtocol, T.SubSequence == Substring {
//        return pattern.consuming(input: input)
//    }
//}
//
//class SharedGrammarPattern<U>: GrammarPattern where U: Parsable {
//    typealias TokenType = U
//
//    var pattern: AnyGrammarPattern<U>!
//
//    init() { }
//    init<P>(_ pattern: P) where P: GrammarPattern, P.TokenType == U {
//        self.pattern = AnyGrammarPattern(pattern)
//    }
//
//    func consuming<T>(input: T) -> [(tokens: [MatchedToken<TokenType>], remaining: Substring)] where T : StringProtocol, T.SubSequence == Substring {
//        return pattern.consuming(input: input)
//    }
//}
























////MARK: - GrammarPattern
//protocol GrammarPattern {
//    associatedtype TokenType: Parsable
//    func consuming<T>(input: T) -> (tokens: [MatchedToken<TokenType>], remaining: Substring)? where T : StringProtocol, T.SubSequence == Substring
//}
////extension GrammarPattern {
////    func _consuming<T>(input: T) -> (tokens: [MatchedToken<TokenType>], parsed: Substring, remaining: Substring)? where T: StringProtocol, T.SubSequence == Substring {
////        guard var (tokens, remaining) = consuming(input: input) else {
////            return nil
////        }
////        let parsed = input[..<remaining.startIndex]
////        return (tokens: tokens, parsed: parsed, remaining: remaining)
////    }
////}
//
//struct LogicalOrGrammarPattern<P1, P2>: GrammarPattern where P1: GrammarPattern, P2: GrammarPattern, P1.TokenType == P2.TokenType {
//    var leftPattern: P1
//    var rightPattern: P2
//    typealias TokenType = P1.TokenType
//
//    init(left: P1, right: P2) {
//        self.leftPattern = left
//        self.rightPattern = right
//    }
//
//    func consuming<T>(input: T) -> (tokens: [MatchedToken<TokenType>], remaining: Substring)? where T: StringProtocol, T.SubSequence == Substring {
//        if let output = leftPattern.consuming(input: input) {
//            return output
//        } else if let output = rightPattern.consuming(input: input) {
//            return output
//        }
//        return nil
//    }
//}
//func ||<T, U>(lhs: T, rhs: U) -> LogicalOrGrammarPattern<T, U> where T: GrammarPattern, U: GrammarPattern, T.TokenType == U.TokenType {
//    return LogicalOrGrammarPattern(left: lhs, right: rhs)
//}
//
//struct OptionalGrammarPattern<T> where T: GrammarPattern {
//    typealias TokenType = T.TokenType
//
//    var pattern: T
//    init(_ pattern: T) {
//        self.pattern = pattern
//    }
//}
//extension OptionalGrammarPattern: GrammarPattern {
//    func consuming<T>(input: T) -> (tokens: [MatchedToken<TokenType>], remaining: Substring)? where T : StringProtocol, T.SubSequence == Substring {
//        if let output = pattern.consuming(input: input) {
//            return output
//        }
//        return (tokens: [], remaining: input[...])
//    }
//}
//extension GrammarPattern {
//    func opt() -> OptionalGrammarPattern<Self> {
//        return OptionalGrammarPattern(self)
//    }
//}
//
//struct SequentialGrammarPattern<P1, P2>: GrammarPattern where P1: GrammarPattern, P2: GrammarPattern, P1.TokenType == P2.TokenType {
//    typealias TokenType = P1.TokenType
//
//    var pattern1: P1
//    var pattern2: P2
//
//    func consuming<T>(input: T) -> (tokens: [MatchedToken<P1.TokenType>], remaining: Substring)? where T : StringProtocol, T.SubSequence == Substring {
//        guard let output1 = pattern1.consuming(input: input) else {
//            return nil
//        }
//        guard var output2 = pattern2.consuming(input: output1.remaining) else {
//            return nil
//        }
//        output2.tokens = output1.tokens + output2.tokens
//        return output2
//    }
//}
//func +<T, U>(lhs: T, rhs: U) -> SequentialGrammarPattern<T, U> where T: GrammarPattern, U: GrammarPattern, T.TokenType == U.TokenType {
//    return SequentialGrammarPattern(pattern1: lhs, pattern2: rhs)
//}
//
//struct RepeatingGrammarPattern<T>: GrammarPattern where T: GrammarPattern {
//    typealias TokenType = T.TokenType
//    var pattern: T
//    var allowedNumberOfRepeats: Range<UInt>
//
//    init(pattern: T, count: PartialRangeFrom<UInt>) {
//        self.pattern = pattern
//        self.allowedNumberOfRepeats = count.lowerBound..<UInt.max
//    }
//    init(pattern: T, count: ClosedRange<UInt>) {
//        self.pattern = pattern
//        self.allowedNumberOfRepeats = count.lowerBound..<(count.upperBound + 1)
//    }
//    init(pattern: T, count: Range<UInt>) {
//        self.pattern = pattern
//        self.allowedNumberOfRepeats = count
//    }
//    init(pattern: T, count: UInt) {
//        self.pattern = pattern
//        self.allowedNumberOfRepeats = count..<(count + 1)
//    }
//
//    func consuming<T>(input: T) -> (tokens: [MatchedToken<TokenType>], remaining: Substring)? where T : StringProtocol, T.SubSequence == Substring {
//        var tokens = [MatchedToken<TokenType>]()
//        var remaining = input[...]
//        for _ in 0..<allowedNumberOfRepeats.lowerBound {
//            guard let output = pattern.consuming(input: remaining) else {
//                return nil
//            }
//            tokens.append(contentsOf: output.tokens)
//            remaining = output.remaining
//        }
//        var count = allowedNumberOfRepeats.lowerBound
//        while count < allowedNumberOfRepeats.upperBound, let output = pattern.consuming(input: remaining) {
//            tokens.append(contentsOf: output.tokens)
//            remaining = output.remaining
//            count += 1
//        }
//        return (tokens: tokens, remaining: remaining)
//    }
//}
//extension GrammarPattern {
//    func repeating(count: PartialRangeFrom<UInt>) -> RepeatingGrammarPattern<Self> {
//        return RepeatingGrammarPattern(pattern: self, count: count)
//    }
//    func repeating(count: ClosedRange<UInt>) -> RepeatingGrammarPattern<Self> {
//        return RepeatingGrammarPattern(pattern: self, count: count)
//    }
//    func repeating(count: Range<UInt>) -> RepeatingGrammarPattern<Self> {
//        return RepeatingGrammarPattern(pattern: self, count: count)
//    }
//    func repeating(count: UInt) -> RepeatingGrammarPattern<Self> {
//        return RepeatingGrammarPattern(pattern: self, count: count)
//    }
//}
//
//fileprivate class _AnyGrammarPatternBoxBase<T>: GrammarPattern where T: Parsable {
//    typealias TokenType = T
//
//    func consuming<T>(input: T) -> (tokens: [MatchedToken<TokenType>], remaining: Substring)? where T : StringProtocol, T.SubSequence == Substring {
//        fatalError()
//    }
//}
//fileprivate class _AnyGrammarPatternBox<T: GrammarPattern>: _AnyGrammarPatternBoxBase<T.TokenType> {
//    let base: T
//    init(_ base: T) {
//        self.base = base
//    }
//    override func consuming<T>(input: T) -> (tokens: [MatchedToken<TokenType>], remaining: Substring)? where T : StringProtocol, T.SubSequence == Substring {
//        return base.consuming(input: input)
//    }
//}
//class AnyGrammarPattern<U>: GrammarPattern where U: Parsable {
//    typealias TokenType = U
//    private let box: _AnyGrammarPatternBoxBase<U>
//
//    init<P>(_ base: P) where P: GrammarPattern, P.TokenType == TokenType {
//        self.box = _AnyGrammarPatternBox(base)
//    }
//    func consuming<T>(input: T) -> (tokens: [MatchedToken<TokenType>], remaining: Substring)? where T : StringProtocol, T.SubSequence == Substring {
//        return box.consuming(input: input)
//    }
//}
//
//class RecursiveGrammarPattern<U>: GrammarPattern where U: Parsable {
//    typealias TokenType = U
//
//    var pattern: AnyGrammarPattern<U>!
//    init<P>(_ recursivePattern: (_ selfPattern: RecursiveGrammarPattern<U>) -> P) where P: GrammarPattern, P.TokenType == U {
//        self.pattern = AnyGrammarPattern(recursivePattern(self))
//    }
//    func consuming<T>(input: T) -> (tokens: [MatchedToken<U>], remaining: Substring)? where T : StringProtocol, T.SubSequence == Substring {
//        return pattern.consuming(input: input)
//    }
//}
//
//class SharedGrammarPattern<U>: GrammarPattern where U: Parsable {
//    typealias TokenType = U
//
//    var pattern: AnyGrammarPattern<U>!
//
//    init() { }
//    init<P>(_ pattern: P) where P: GrammarPattern, P.TokenType == U {
//        self.pattern = AnyGrammarPattern(pattern)
//    }
//
//    func consuming<T>(input: T) -> (tokens: [MatchedToken<TokenType>], remaining: Substring)? where T : StringProtocol, T.SubSequence == Substring {
//        return pattern.consuming(input: input)
//    }
//}
//
//
