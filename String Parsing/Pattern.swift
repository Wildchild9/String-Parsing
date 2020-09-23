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
    func consuming<T>(input: T) -> (tokens: [MatchedToken<TokenType>], remaining: Substring)? where T : StringProtocol, T.SubSequence == Substring
}

struct LogicalOrGrammarPattern<P1, P2>: GrammarPattern where P1: GrammarPattern, P2: GrammarPattern, P1.TokenType == P2.TokenType {
    var leftPattern: P1
    var rightPattern: P2
    typealias TokenType = P1.TokenType
    
    init(left: P1, right: P2) {
        self.leftPattern = left
        self.rightPattern = right
    }
    
    func consuming<T>(input: T) -> (tokens: [MatchedToken<TokenType>], remaining: Substring)? where T: StringProtocol, T.SubSequence == Substring {
        if let output = leftPattern.consuming(input: input) {
            return output
        } else if let output = rightPattern.consuming(input: input) {
            return output
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
    func consuming<T>(input: T) -> (tokens: [MatchedToken<TokenType>], remaining: Substring)? where T : StringProtocol, T.SubSequence == Substring {
        if let output = pattern.consuming(input: input) {
            return output
        }
        return (tokens: [], remaining: input[...])
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
    
    func consuming<T>(input: T) -> (tokens: [MatchedToken<P1.TokenType>], remaining: Substring)? where T : StringProtocol, T.SubSequence == Substring {
        guard let output1 = pattern1.consuming(input: input) else {
            return nil
        }
        guard var output2 = pattern2.consuming(input: output1.remaining) else {
            return nil
        }
        output2.tokens = output1.tokens + output2.tokens
        return output2
    }
}
func +<T, U>(lhs: T, rhs: U) -> SequentialGrammarPattern<T, U> where T: GrammarPattern, U: GrammarPattern, T.TokenType == U.TokenType {
    return SequentialGrammarPattern(pattern1: lhs, pattern2: rhs)
}

struct RepeatingGrammarPattern<T>: GrammarPattern where T: GrammarPattern {
    typealias TokenType = T.TokenType
    var pattern: T
    var allowedNumberOfRepeats: Range<UInt>

    init(pattern: T, count: PartialRangeFrom<UInt>) {
        self.pattern = pattern
        self.allowedNumberOfRepeats = count.lowerBound..<UInt.max
    }
    init(pattern: T, count: ClosedRange<UInt>) {
        self.pattern = pattern
        self.allowedNumberOfRepeats = count.lowerBound..<(count.upperBound + 1)
    }
    init(pattern: T, count: Range<UInt>) {
        self.pattern = pattern
        self.allowedNumberOfRepeats = count
    }
    init(pattern: T, count: UInt) {
        self.pattern = pattern
        self.allowedNumberOfRepeats = count..<(count + 1)
    }
    
    func consuming<T>(input: T) -> (tokens: [MatchedToken<TokenType>], remaining: Substring)? where T : StringProtocol, T.SubSequence == Substring {
        var tokens = [MatchedToken<TokenType>]()
        var remaining = input[...]
        for _ in 0...allowedNumberOfRepeats.lowerBound {
            guard let output = pattern.consuming(input: remaining) else {
                return nil
            }
            tokens.append(contentsOf: output.tokens)
            remaining = output.remaining
        }
        var count = allowedNumberOfRepeats.lowerBound
        while count + 1 < allowedNumberOfRepeats.upperBound, let output = pattern.consuming(input: remaining) {
            tokens.append(contentsOf: output.tokens)
            remaining = output.remaining
            count += 1
        }
        return (tokens: tokens, remaining: remaining)
    }
    
}
extension GrammarPattern {
    func repeating(count: PartialRangeFrom<UInt>) -> RepeatingGrammarPattern<Self> {
        return RepeatingGrammarPattern(pattern: self, count: count)
    }
    func repeating(count: ClosedRange<UInt>) -> RepeatingGrammarPattern<Self> {
        return RepeatingGrammarPattern(pattern: self, count: count)
    }
    func repeating(count: Range<UInt>) -> RepeatingGrammarPattern<Self> {
        return RepeatingGrammarPattern(pattern: self, count: count)
    }
    func repeating(count: UInt) -> RepeatingGrammarPattern<Self> {
        return RepeatingGrammarPattern(pattern: self, count: count)
    }
}

fileprivate class _AnyGrammarPatternBoxBase<T>: GrammarPattern where T: Parsable {
    typealias TokenType = T
    
    func consuming<T>(input: T) -> (tokens: [MatchedToken<TokenType>], remaining: Substring)? where T : StringProtocol, T.SubSequence == Substring {
        fatalError()
    }
}
fileprivate class _AnyGrammarPatternBox<T: GrammarPattern>: _AnyGrammarPatternBoxBase<T.TokenType> {
    let base: T
    init(_ base: T) {
        self.base = base
    }
    override func consuming<T>(input: T) -> (tokens: [MatchedToken<TokenType>], remaining: Substring)? where T : StringProtocol, T.SubSequence == Substring {
        return base.consuming(input: input)
    }
}
class AnyGrammarPattern<U>: GrammarPattern where U: Parsable {
    typealias TokenType = U
    private let box: _AnyGrammarPatternBoxBase<U>
    
    init<P>(_ base: P) where P: GrammarPattern, P.TokenType == TokenType {
        self.box = _AnyGrammarPatternBox(base)
    }
    func consuming<T>(input: T) -> (tokens: [MatchedToken<TokenType>], remaining: Substring)? where T : StringProtocol, T.SubSequence == Substring {
        return box.consuming(input: input)
    }
}

class RecursiveGrammarPattern<U>: GrammarPattern where U: Parsable {
    typealias TokenType = U
    
    var pattern: AnyGrammarPattern<U>!
    init<P>(_ recursivePattern: (_ selfPattern: RecursiveGrammarPattern<U>) -> P) where P: GrammarPattern, P.TokenType == U {
        self.pattern = AnyGrammarPattern(recursivePattern(self))
    }
    func consuming<T>(input: T) -> (tokens: [MatchedToken<U>], remaining: Substring)? where T : StringProtocol, T.SubSequence == Substring {
        return pattern.consuming(input: input)
    }
}

class SharedGrammarPattern<U>: GrammarPattern where U: Parsable {
    typealias TokenType = U
    
    var pattern: AnyGrammarPattern<U>!
    
    init() { }
    init<P>(_ pattern: P) where P: GrammarPattern, P.TokenType == U {
        self.pattern = AnyGrammarPattern(pattern)
    }
    
    func consuming<T>(input: T) -> (tokens: [MatchedToken<TokenType>], remaining: Substring)? where T : StringProtocol, T.SubSequence == Substring {
        return pattern.consuming(input: input)
    }
}


