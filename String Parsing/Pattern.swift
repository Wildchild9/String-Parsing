//
//  Pattern.swift
//  String Parsing
//
//  Created by Noah Wilder on 2020-05-15.
//  Copyright © 2020 Noah Wilder. All rights reserved.
//

import Foundation
//MARK: - TokenPattern

protocol TokenPattern: CustomStringConvertible {
    func parse(from context: TokenParsingContext, index: TokenPatternIndex) -> TokenParsingResult
}

struct TokenPatternIndex {
    private(set) var depth: Int
    private(set) var path: [Step] = []
    
    fileprivate init() {
        self.depth = 0
    }
    private init(depth: Int, path: [Step]) {
        self.depth = depth
        self.path = path
    }
    
    static let root = TokenPatternIndex()
    
    func parentIndex() -> TokenPatternIndex? {
        guard !path.isEmpty else {
            return nil
        }
        var parent = self
        parent.depth -= 1
        parent.path.removeLast()
        return parent
    }
    func isParent(of index: TokenPatternIndex) -> Bool {
        guard index.depth > depth else {
            return false
        }
        return isRoot || zip(path, index.path).allSatisfy { $0 == $0 }
    }
    
    var isRoot: Bool {
        return path.isEmpty
    }
    
    func childIndex(stepping direction: Step) -> TokenPatternIndex {
        var newIndex = self
        newIndex.depth += 1
        newIndex.path.append(direction)
        return newIndex
    }
    enum Step {
        case left, right
    }
}

final class TokenParsingContext {
    private(set) var input: String
    private(set) var currentIndex: String.Index
    
    private var recoveryPoints: [RecoveryPoint] = []
    
    private init(input: String, currentIndex: String.Index) {
        self.input = input
        self.currentIndex = currentIndex
    }
    
    fileprivate static func parse(input: String, from index: String.Index, using pattern: TokenPattern) -> (currentIndex: String.Index, recoveryPoints: [RecoveryPoint])? {
        let context = TokenParsingContext(input: input, currentIndex: index)
        let initialResult = pattern.parse(from: context, index: .root)
        if initialResult == .success {
            return (currentIndex: context.currentIndex, recoveryPoints: context.recoveryPoints)
        }
        while let recoveryPoint = context.recoveryPoints.popLast() {
            let result = context.revert(to: recoveryPoint)
            if result == .success {
                return (currentIndex: context.currentIndex, recoveryPoints: context.recoveryPoints)
            }
        }
        return nil
    }
    
    func advanceIndex(by offset: String.IndexDistance) {
        currentIndex = input.index(currentIndex, offsetBy: offset)
    }
    func advanceIndex(to index: String.Index) {
        currentIndex = index
    }
    
    func addRecoveryPoint(at index: TokenPatternIndex, pattern: TokenPattern) {
        addRecoveryPoint(at: index, from: currentIndex, pattern: pattern)
    }
    func addRecoveryPoint(at index: TokenPatternIndex, from parsingIndex: String.Index, pattern: TokenPattern) {
//        if let lastRecoveryPoint = recoveryPoints.last, index.isParent(of: lastRecoveryPoint.index) {
//            return
//        }
        let recoveryPoint = RecoveryPoint(parsingIndex: parsingIndex, index: index, pattern: pattern)
        recoveryPoints.append(recoveryPoint)
    }
    
    func attemptRecovery() -> TokenParsingResult {
        while let recoveryPoint = recoveryPoints.popLast() {
            let result = revert(to: recoveryPoint)
            if result == .success {
                return .success
            }
        }
        return .failure
    }
    
    private func revert(to recoveryPoint: RecoveryPoint) -> TokenParsingResult {
        self.currentIndex = recoveryPoint.parsingIndex
        return recoveryPoint.pattern.parse(from: self, index: recoveryPoint.index)
    }

    final class RecoveryPoint {
        var parsingIndex: String.Index
        var pattern: TokenPattern
        var index: TokenPatternIndex
        
        internal init(parsingIndex: String.Index, index: TokenPatternIndex, pattern: TokenPattern) {
            self.parsingIndex = parsingIndex
            self.pattern = pattern
            self.index = index
        }
        
        func updatePattern<T>(to newPattern: (AnyTokenPattern) -> T) where T: TokenPattern {
            self.pattern = newPattern(AnyTokenPattern(pattern))
        }
    }
    
    func updateRecovery<T>(from index: TokenPatternIndex, newPattern: (AnyTokenPattern) -> T) where T: TokenPattern {
        for recoveryPoint in recoveryPoints where index.isParent(of: recoveryPoint.index) {
            recoveryPoint.updatePattern(to: newPattern)
        }
    }
}
enum TokenParsingResult {
    case success
    case failure
}

struct LogicalOrTokenPattern<P1, P2>: TokenPattern where P1: TokenPattern, P2: TokenPattern {
    var leftPattern: P1
    var rightPattern: P2
    
    var description: String {
        return "(" + leftPattern.description + " || " + rightPattern.description + ")"
    }
    
    init(left: P1, right: P2) {
        self.leftPattern = left
        self.rightPattern = right
        
        // Check if any are interfering patterns, if not, do not add recovery point
    }
    
    func parse(from context: TokenParsingContext, index: TokenPatternIndex) -> TokenParsingResult {
        
        // Get current index of context for new recovery point that is added if leftResult is successful
        let indexSnapshot = context.currentIndex

        let leftResult = leftPattern.parse(from: context, index: index.childIndex(stepping: .left))
        
        // Update recovery points that are children of index, adding right-hand pattern
        context.updateRecovery(from: index) { $0 || rightPattern }

        guard case .success = leftResult else {
            return rightPattern.parse(from: context, index: index)
        }
        
        // Add recovery point for right pattern using initial index of context when being passed into this function
        context.addRecoveryPoint(at: index, from: indexSnapshot, pattern: rightPattern)
        
        return .success
    }
}
func ||<T, U>(lhs: T, rhs: U) -> LogicalOrTokenPattern<T, U> {
    return LogicalOrTokenPattern(left: lhs, right: rhs)
}

struct SequentialTokenPattern<P1, P2>: TokenPattern where P1: TokenPattern, P2: TokenPattern {
    
    var pattern1: P1
    var pattern2: P2

    var description: String {
        return "(" + pattern1.description + " + " + pattern2.description + ")"
    }
    func parse(from context: TokenParsingContext, index: TokenPatternIndex) -> TokenParsingResult {
        let result1 = pattern1.parse(from: context, index: index.childIndex(stepping: .left))
        context.updateRecovery(from: index) { $0 + pattern2 }

        guard case .success = result1 else {
            return .failure
        }
        let result2 = pattern2.parse(from: context, index: index.childIndex(stepping: .right))
        return result2
    }
}
func +<T, U>(lhs: T, rhs: U) -> SequentialTokenPattern<T, U> {
    return SequentialTokenPattern(pattern1: lhs, pattern2: rhs)
}

final class AnyTokenPattern: TokenPattern {
    private let pattern: TokenPattern
    
    var description: String {
        return pattern.description
    }
    
    init(_ pattern: TokenPattern) {
        self.pattern = pattern
    }
    init<P>(_ pattern: P) where P: TokenPattern {
        self.pattern = pattern
    }
    func parse(from context: TokenParsingContext, index: TokenPatternIndex) -> TokenParsingResult {
        return pattern.parse(from: context, index: index)
    }
}

struct AnchorTokenPattern: TokenPattern {
    var location: Location
    
    var description: String {
        return location == .start ? "^" : "$"
    }
    
    init(at location: Location) {
        self.location = location
    }
    
    func parse(from context: TokenParsingContext, index: TokenPatternIndex) -> TokenParsingResult {
        switch (context.currentIndex, location) {
        case (context.input.startIndex, .start), (context.input.startIndex, .end):
            return .success
        default:
            return .failure
        }
    }
    
    enum Location {
        case start, end
    }
}

struct EmptyTokenPattern: TokenPattern {
    var description: String {
        return ""
    }
    func parse(from context: TokenParsingContext, index: TokenPatternIndex) -> TokenParsingResult {
        return .success
    }
}

struct OptionalTokenPattern<T>: TokenPattern where T: TokenPattern {
    var pattern: T
    
    var description: String {
        return pattern.description + "?"
    }
    
    init(_ pattern: T) {
        self.pattern = pattern
    }

    func parse(from context: TokenParsingContext, index: TokenPatternIndex) -> TokenParsingResult {
        let newPattern = pattern || EmptyTokenPattern()
        return newPattern.parse(from: context, index: index)
    }
}
extension TokenPattern {
    func opt() -> OptionalTokenPattern<Self> {
        return OptionalTokenPattern(self)
    }
}

struct RepeatingTokenPattern<T>: TokenPattern where T: TokenPattern {
    
    var pattern: T
    private var range: RangeBox
    
    var description: String {
        return pattern.description + "{" + range.description + "}"
    }
    
    private init(pattern: T, count: RangeBox) {
        self.pattern = pattern
        self.range = count
    }
    init(_ pattern: T, count: ClosedRange<UInt>) {
        self.pattern = pattern
        self.range = .closedRange(count)
    }
    init(_ pattern: T, count: PartialRangeFrom<UInt>) {
        self.pattern = pattern
        self.range = .partialRangeFrom(count)
    }
    init(_ pattern: T, count: PartialRangeThrough<UInt>) {
        self.pattern = pattern
        self.range = .closedRange(0...count.upperBound)
    }
    init(_ pattern: T, count: PartialRangeUpTo<UInt>) {
        self.pattern = pattern
        self.range = .range(0..<count.upperBound)
    }
    init(_ pattern: T, count: Range<UInt>) {
        self.pattern = pattern
        self.range = .range(count)
    }
    init(_ pattern: T, count: UInt) {
        self.pattern = pattern
        self.range = .closedRange(count...count)
    }
    
    private enum RangeBox: CustomStringConvertible {
        case closedRange(ClosedRange<UInt>)
        case range(Range<UInt>)
        case partialRangeFrom(PartialRangeFrom<UInt>)
        
        var lowerBound: UInt {
            switch self {
            case let .closedRange(closedRange):
                return closedRange.lowerBound
            case let .range(range):
                return range.lowerBound
            case let .partialRangeFrom(partialRangeFrom):
                return partialRangeFrom.lowerBound
            }
        }
        private var upperBound: UInt {
            switch self {
            case let .closedRange(closedRange):
                return closedRange.upperBound
            case let .range(range):
                return range.upperBound
            case .partialRangeFrom:
                return UInt.max
            }
        }
        var isEmpty: Bool {
            if case let .range(range) = self {
                return range.isEmpty
            }
            return false
        }
        var count: Int {
            switch self {
            case let .closedRange(closedRange):
                return closedRange.count
                
            case let .range(range):
                return range.count
                
            case .partialRangeFrom:
                return -1
            }
        }
        var isZeroRange: Bool {
            switch self {
            case .closedRange(0...0), .range(0..<1): return true
            default: return false
            }
        }
        var description: String {
            switch self {
            case let .closedRange(closedRange):
                return "\(closedRange.lowerBound)...\(closedRange.upperBound)"
            case let .range(range):
                return "\(range.lowerBound)..<\(range.upperBound)"
            case let .partialRangeFrom(partialRangeFrom):
                return "\(partialRangeFrom.lowerBound)..."
            }
        }
        func shifting(lowerBoundBy lowerBoundShift: Int, upperBoundBy upperBoundShift: Int) -> Self {
            let newLowerBound = lowerBoundShift > 0 ? lowerBound + UInt(lowerBoundShift) : lowerBound - UInt(lowerBoundShift)
            
            switch self {
            case .closedRange:
                let newUpperBound = upperBoundShift > 0 ? upperBound + UInt(upperBoundShift) : upperBound - UInt(upperBoundShift)
                return .closedRange(newLowerBound...newUpperBound)
            case .range:
                let newUpperBound = upperBoundShift > 0 ? upperBound + UInt(upperBoundShift) : upperBound - UInt(upperBoundShift)
                return .range(newLowerBound..<newUpperBound)
            case .partialRangeFrom:
                return .partialRangeFrom(newLowerBound...)
            }
        }
    }
    
    func parse(from context: TokenParsingContext, index: TokenPatternIndex) -> TokenParsingResult {
        guard !range.isEmpty, !range.isZeroRange else {
            return .success
        }
        
        let lowerBound = range.lowerBound
        var idx: UInt = 0
        // Consumes pattern lowerBound times
        while idx < lowerBound {
            let result = pattern.parse(from: context, index: index)
            guard case .success = result else {
                return .failure
            }
            idx += 1
        }
        
        let recoveryPattern: TokenPattern
        
        // Check if the range has an upper bound
        if case .partialRangeFrom = range {
            recoveryPattern = RepeatingTokenPattern(pattern, count: 1...)
        } else {
            let rangeCount = range.count
            // Check if the max pattern repetition count has parsed
            guard rangeCount > 1 else {
                return .success
            }
            
            if rangeCount > 2 {
                // If lowerBound is 1, only shift the upper bound
                let lowerBoundShift = lowerBound == 1 ? 0 : 1 - Int(lowerBound)
                let updatedRange = range.shifting(lowerBoundBy: lowerBoundShift, upperBoundBy: -Int(lowerBound))
                recoveryPattern = RepeatingTokenPattern(pattern: pattern, count: updatedRange)
            } else {
                // There is only a single element more that can be consumed
                recoveryPattern = pattern
            }
        }
        context.addRecoveryPoint(at: index, pattern: recoveryPattern)
        return .success
    }
}
extension TokenPattern {
    func repeating(count: ClosedRange<UInt>) -> RepeatingTokenPattern<Self> {
        return RepeatingTokenPattern(self, count: count)
    }
    func repeating(count: PartialRangeFrom<UInt>) -> RepeatingTokenPattern<Self> {
        return RepeatingTokenPattern(self, count: count)
    }
    func repeating(count: PartialRangeThrough<UInt>) -> RepeatingTokenPattern<Self> {
        return RepeatingTokenPattern(self, count: count)
    }
    func repeating(count: PartialRangeUpTo<UInt>) -> RepeatingTokenPattern<Self> {
        return RepeatingTokenPattern(self, count: count)
    }
    func repeating(count: Range<UInt>) -> RepeatingTokenPattern<Self> {
        return RepeatingTokenPattern(self, count: count)
    }
    func repeating(count: UInt) -> RepeatingTokenPattern<Self> {
        return RepeatingTokenPattern(self, count: count)
    }
}

class RecursiveTokenPattern: TokenPattern {
    var pattern: AnyTokenPattern!
    
    var description: String {
        return "r()" //+ pattern.description + ")"
    }
    init<T>(_ recursivePattern: (_ selfPattern: RecursiveTokenPattern) -> T) where T: TokenPattern {
        self.pattern = AnyTokenPattern(recursivePattern(self))
    }
    func parse(from context: TokenParsingContext, index: TokenPatternIndex) -> TokenParsingResult {
        pattern.parse(from: context, index: index)
    }
}

class SharedTokenPattern: TokenPattern {
    
    var pattern: AnyTokenPattern
    
    var description: String {
        return pattern.description
    }
    
    init() {
        pattern = AnyTokenPattern(NeverTokenPattern())
    }
    init<T>(_ pattern: T) where T: TokenPattern {
        self.pattern = AnyTokenPattern(pattern)
    }
    
    func parse(from context: TokenParsingContext, index: TokenPatternIndex) -> TokenParsingResult {
        pattern.parse(from: context, index: index)
    }
}

struct NeverTokenPattern: TokenPattern {
    
    init() { }
    
    var description: String {
        return "NEVER"
    }
    
    func parse(from context: TokenParsingContext, index: TokenPatternIndex) -> TokenParsingResult {
        return .failure
    }
}

extension String: TokenPattern {
    func parse(from context: TokenParsingContext, index: TokenPatternIndex) -> TokenParsingResult {
        for char in self {
            guard context.currentIndex < context.input.endIndex, char == context.input[context.currentIndex] else {
                return .failure
            }
            context.advanceIndex(by: 1)
        }
        return .success
    }
}

struct RegexTokenPattern: TokenPattern {
//    let pattern: String
    let regex: NSRegularExpression
    
    var description: String {
        return regex.pattern
    }
    
    init(_ pattern: String, options: NSRegularExpression.Options = []) {
        guard let regex = try? NSRegularExpression(pattern: pattern, options: options) else {
            fatalError("Invalid regular expresion.")
        }
        self.regex = regex
    }
    
    func parse(from context: TokenParsingContext, index: TokenPatternIndex) -> TokenParsingResult {
        let range = NSRange(context.currentIndex..<context.input.endIndex, in: context.input)
        guard let result = regex.firstMatch(in: context.input, options: .anchored, range: range) else {
            return .failure
        }
        let newIndex = Range(result.range, in: context.input)!.upperBound
        context.advanceIndex(to: newIndex)
//        guard let range = context.input[context.currentIndex...].range(of: pattern, options: [.anchored, .regularExpression]) else {
//            return .failure
//        }
//        context.advanceIndex(to: range.upperBound)
        return .success
    }
}

extension KeyPath: CustomStringConvertible {
    public var description: String {
        return String(describing: self)
    }
}

extension KeyPath: TokenPattern where Root == Character, Value == Bool {
    
    func parse(from context: TokenParsingContext, index: TokenPatternIndex) -> TokenParsingResult {
        guard context.currentIndex < context.input.endIndex, context.input[context.currentIndex][keyPath: self] else {
            return .failure
        }
        context.advanceIndex(by: 1)
        return .success
    }
}



//MARK: - GrammarPattern

protocol GrammarPattern {
    associatedtype Base: Parsable
    func parse(from context: GrammarParsingContext<Base>, index: GrammarPatternIndex) -> GrammarParsingResult
}

extension Parsable {
    func parse(from context: GrammarParsingContext<Base>, index: GrammarPatternIndex) -> GrammarParsingResult {
        guard let result = TokenParsingContext.parse(input: context.input, from: context.currentIndex, using: pattern) else {
            return .failure
        }
        
        let (newIndex, recoveryPoints) = result
        
//        for recoveryPoint in recoveryPoints {
//            let recoveryPattern = TokenRecoveryPattern(recoveryPoint: recoveryPoint, classification: self)
//            context.addRecoveryPoint(at: index, pattern: recoveryPattern)
//        }
        
        let token = MatchedToken(classification: self, match: context.input[context.currentIndex..<newIndex])
        context.advance(to: newIndex, adding: token)
        return .success
    }
}

fileprivate struct TokenRecoveryPattern<T>: GrammarPattern where T: Parsable {
    typealias Base = T
    var recoveryPoint: TokenParsingContext.RecoveryPoint
    var classification: T
    
    func parse(from context: GrammarParsingContext<T>, index: GrammarPatternIndex) -> GrammarParsingResult {
        guard let result = TokenParsingContext.parse(input: context.input, from: recoveryPoint.parsingIndex, using: recoveryPoint.pattern) else {
            return .failure
        }
        for newRecoveryPoint in result.recoveryPoints {
            let recoveryPattern = TokenRecoveryPattern(recoveryPoint: newRecoveryPoint, classification: classification)
            context.addRecoveryPoint(at: index, pattern: recoveryPattern)
        }
        let token = MatchedToken(classification: classification, match: context.input[context.currentIndex..<result.currentIndex])
        context.advance(to: result.currentIndex, adding: token)
        return .success
    }
}

struct GrammarPatternIndex {
    private(set) var depth: Int
    private(set) var path: [Step] = []
    
    fileprivate init() {
        self.depth = 0
    }
    private init(depth: Int, path: [Step]) {
        self.depth = depth
        self.path = path
    }
    
    static let root = GrammarPatternIndex()
    
    func parentIndex() -> GrammarPatternIndex? {
        guard !path.isEmpty else {
            return nil
        }
        var parent = self
        parent.depth -= 1
        parent.path.removeLast()
        return parent
    }
    func isParent(of index: GrammarPatternIndex) -> Bool {
        guard index.depth > depth else {
            return false
        }
        return isRoot || zip(path, index.path).allSatisfy { $0 == $0 }
    }
    
    var isRoot: Bool {
        return path.isEmpty
    }
    
    func childIndex(stepping direction: Step) -> GrammarPatternIndex {
        var newIndex = self
        newIndex.depth += 1
        newIndex.path.append(direction)
        return newIndex
    }
    enum Step {
        case left, right
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
    private var recoveryPoints: [RecoveryPoint] = []
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
        let initialResult = basePattern.parse(from: self, index: .root)
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
    
    func generateSnapshot() -> Snapshot {
        return Snapshot(tokens: consumedTokens[...], currentIndex: currentIndex)
    }
    
    func addRecoveryPoint<P>(at index: GrammarPatternIndex, pattern: P) where P: GrammarPattern, P.Base == T {
        let snapshot = Snapshot(tokens: consumedTokens[...], currentIndex: currentIndex)
        addRecoveryPoint(at: index, from: snapshot, pattern: pattern)
    }
    func addRecoveryPoint<P>(at index: GrammarPatternIndex, from snapshot: Snapshot, pattern: P) where P: GrammarPattern, P.Base == T {
        if let lastRecoveryPoint = recoveryPoints.last, index.isParent(of: lastRecoveryPoint.index) {//index.isParent(of: lastRecoveryPoint.index) {
            return
        }
        let recoveryPoint = RecoveryPoint(snapshot: snapshot, index: index, pattern: pattern)
        recoveryPoints.append(recoveryPoint)
    }
    
    var lastRecoveryPoint: RecoveryPoint? {
        return recoveryPoints.last
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
        self.consumedTokens = Array(recoveryPoint.snapshot.tokens)
        return recoveryPoint.pattern.parse(from: self, index: recoveryPoint.index)
    }
    
    final class Snapshot {
        var tokens: ArraySlice<MatchedToken<T>>
        var currentIndex: String.Index
        
        fileprivate init(tokens: ArraySlice<MatchedToken<T>>, currentIndex: String.Index) {
            self.tokens = tokens
            self.currentIndex = currentIndex
        }
    }
    final class RecoveryPoint {
        var snapshot: Snapshot
        var pattern: AnyGrammarPattern<T>
        var index: GrammarPatternIndex
        
        internal init<P>(snapshot: Snapshot, index: GrammarPatternIndex, pattern: P) where P: GrammarPattern, P.Base == T {
            self.snapshot = snapshot
            self.pattern = AnyGrammarPattern(pattern)
            self.index = index
        }
        
        func updatePattern<P>(to newPattern: (AnyGrammarPattern<T>) -> P) where P: GrammarPattern, P.Base == T {
            self.pattern = AnyGrammarPattern(newPattern(self.pattern))
        }
    }
    func updateRecovery<P>(from index: GrammarPatternIndex, newPattern: (AnyGrammarPattern<T>) -> P) where P: GrammarPattern, P.Base == T {
        for recoveryPoint in recoveryPoints where index.isParent(of: recoveryPoint.index) {
            recoveryPoint.updatePattern(to: newPattern)
        }
    }
}

enum GrammarParsingResult {
    case success
    case failure
}

struct LogicalOrGrammarPattern<P1, P2>: GrammarPattern where P1: GrammarPattern, P2: GrammarPattern, P1.Base == P2.Base {
    typealias Base = P1.Base

    var leftPattern: P1
    var rightPattern: P2
    
    init(left: P1, right: P2) {
        self.leftPattern = left
        self.rightPattern = right
    }
    
    func parse(from context: GrammarParsingContext<Base>, index: GrammarPatternIndex) -> GrammarParsingResult {

        // Generate snapshot of current context for new recovery point that is added if leftResult is successful
        let snapshot = context.generateSnapshot()
        
        let leftResult = leftPattern.parse(from: context, index: index.childIndex(stepping: .left))

        // Update recovery points that are children of index, adding right-hand pattern
        context.updateRecovery(from: index) { $0 || rightPattern }

        guard case .success = leftResult else {
//            if let lastRecoveryPoint = context.lastRecoveryPoint, index.isParent(of: lastRecoveryPoint.index) {
//                return .failure
//            } else {
                return rightPattern.parse(from: context, index: index)
//            }
        }
        
        // Add recovery point for right pattern using snapshot of initial state of context when being passed into this function
        context.addRecoveryPoint(at: index, from: snapshot, pattern: rightPattern)
        
        return .success
    }
}

func ||<T, U>(lhs: T, rhs: U) -> LogicalOrGrammarPattern<T, U> {
    return LogicalOrGrammarPattern(left: lhs, right: rhs)
}

struct SequentialGrammarPattern<P1, P2>: GrammarPattern where P1: GrammarPattern, P2: GrammarPattern, P1.Base == P2.Base {
    typealias Base = P1.Base
    
    var pattern1: P1
    var pattern2: P2
    
    func parse(from context: GrammarParsingContext<P1.Base>, index: GrammarPatternIndex) -> GrammarParsingResult {
        let result1 = pattern1.parse(from: context, index: index.childIndex(stepping: .left))
        
        // Update recovery points that are children of index, adding right-hand pattern
        /// - Note: This is done to reconstruct the total pattern in terms of the recovery point's pattern
        context.updateRecovery(from: index) { $0 + pattern2 }

        guard case .success = result1 else {
            // Propogate failure upwards
            return .failure
        }
        let result2 = pattern2.parse(from: context, index: index.childIndex(stepping: .right))
        return result2
    }
}
func +<T, U>(lhs: T, rhs: U) -> SequentialGrammarPattern<T, U> {
    return SequentialGrammarPattern(pattern1: lhs, pattern2: rhs)
}

fileprivate class _AnyGrammarPatternBoxBase<T>: GrammarPattern where T: Parsable {
    typealias Base = T
    
    func parse(from context: GrammarParsingContext<Base>, index: GrammarPatternIndex) -> GrammarParsingResult {
        fatalError()
    }
}
fileprivate class _AnyGrammarPatternBox<T: GrammarPattern>: _AnyGrammarPatternBoxBase<T.Base> {
    let base: T
    init(_ base: T) {
        self.base = base
    }
    override func parse(from context: GrammarParsingContext<Base>, index: GrammarPatternIndex) -> GrammarParsingResult {
        return base.parse(from: context, index: index)
    }
}
final class AnyGrammarPattern<U>: GrammarPattern where U: Parsable {
    typealias Base = U
    private let box: _AnyGrammarPatternBoxBase<U>
    
    init<P>(_ base: P) where P: GrammarPattern, P.Base == Base {
        self.box = _AnyGrammarPatternBox(base)
    }
    func parse(from context: GrammarParsingContext<Base>, index: GrammarPatternIndex) -> GrammarParsingResult {
        return box.parse(from: context, index: index)
    }
}

struct EmptyGrammarPattern<U>: GrammarPattern where U: Parsable {
    typealias Base = U
    func parse(from context: GrammarParsingContext<U>, index: GrammarPatternIndex) -> GrammarParsingResult {
        return .success
    }
}

struct OptionalGrammarPattern<T>: GrammarPattern where T: GrammarPattern {
    typealias Base = T.Base
    
    var pattern: T
    init(_ pattern: T) {
        self.pattern = pattern
    }
    
    func parse(from context: GrammarParsingContext<Base>, index: GrammarPatternIndex) -> GrammarParsingResult {
        
        let newPattern = pattern || EmptyGrammarPattern()
        return newPattern.parse(from: context, index: index)
    }
}
extension GrammarPattern {
    func opt() -> OptionalGrammarPattern<Self> {
        return OptionalGrammarPattern(self)
    }
}

struct RepeatingGrammarPattern<T>: GrammarPattern where T: GrammarPattern {
    typealias Base = T.Base
    
    var pattern: T
    private var range: RangeBox
    
    private init(pattern: T, count: RangeBox) {
        self.pattern = pattern
        self.range = count
    }
    init(_ pattern: T, count: ClosedRange<UInt>) {
        self.pattern = pattern
        self.range = .closedRange(count)
    }
    init(_ pattern: T, count: PartialRangeFrom<UInt>) {
        self.pattern = pattern
        self.range = .partialRangeFrom(count)
    }
    init(_ pattern: T, count: PartialRangeThrough<UInt>) {
        self.pattern = pattern
        self.range = .closedRange(0...count.upperBound)
    }
    init(_ pattern: T, count: PartialRangeUpTo<UInt>) {
        self.pattern = pattern
        self.range = .range(0..<count.upperBound)
    }
    init(_ pattern: T, count: Range<UInt>) {
        self.pattern = pattern
        self.range = .range(count)
    }
    init(_ pattern: T, count: UInt) {
        self.pattern = pattern
        self.range = .closedRange(count...count)
    }
    
    private enum RangeBox: CustomStringConvertible {
        case closedRange(ClosedRange<UInt>)
        case range(Range<UInt>)
        case partialRangeFrom(PartialRangeFrom<UInt>)
        
        var lowerBound: UInt {
            switch self {
            case let .closedRange(closedRange):
                return closedRange.lowerBound
            case let .range(range):
                return range.lowerBound
            case let .partialRangeFrom(partialRangeFrom):
                return partialRangeFrom.lowerBound
            }
        }
        private var upperBound: UInt {
            switch self {
            case let .closedRange(closedRange):
                return closedRange.upperBound
            case let .range(range):
                return range.upperBound
            case .partialRangeFrom:
                return UInt.max
            }
        }
        var isEmpty: Bool {
            if case let .range(range) = self {
                return range.isEmpty
            }
            return false
        }
        var count: Int {
            switch self {
            case let .closedRange(closedRange):
                return closedRange.count
                
            case let .range(range):
                return range.count
                
            case .partialRangeFrom:
                return -1
            }
        }
        var isZeroRange: Bool {
            switch self {
            case .closedRange(0...0), .range(0..<1): return true
            default: return false
            }
        }
        var description: String {
            switch self {
            case let .closedRange(closedRange):
                return "\(closedRange.lowerBound)...\(closedRange.upperBound)"
            case let .range(range):
                return "\(range.lowerBound)..<\(range.upperBound)"
            case let .partialRangeFrom(partialRangeFrom):
                return "\(partialRangeFrom.lowerBound)..."
            }
        }
        func shifting(lowerBoundBy lowerBoundShift: Int, upperBoundBy upperBoundShift: Int) -> Self {
            let newLowerBound = lowerBoundShift > 0 ? lowerBound + UInt(lowerBoundShift) : lowerBound - UInt(lowerBoundShift)
            
            switch self {
            case .closedRange:
                let newUpperBound = upperBoundShift > 0 ? upperBound + UInt(upperBoundShift) : upperBound - UInt(upperBoundShift)
                return .closedRange(newLowerBound...newUpperBound)
            case .range:
                let newUpperBound = upperBoundShift > 0 ? upperBound + UInt(upperBoundShift) : upperBound - UInt(upperBoundShift)
                return .range(newLowerBound..<newUpperBound)
            case .partialRangeFrom:
                return .partialRangeFrom(newLowerBound...)
            }
        }
    }
    
    func parse(from context: GrammarParsingContext<T.Base>, index: GrammarPatternIndex) -> GrammarParsingResult {
        guard !range.isEmpty, !range.isZeroRange else {
            return .success
        }
        
        let lowerBound = range.lowerBound
        var idx: UInt = 0
        // Consumes pattern lowerBound times
        while idx < lowerBound {
            let result = pattern.parse(from: context, index: index)
            guard case .success = result else {
                return .failure
            }
            idx += 1
        }
        
        let recoveryPattern: AnyGrammarPattern<Base>
        
        // Check if the range has an upper bound
        if case .partialRangeFrom = range {
            let recoveryRange = RepeatingGrammarPattern(pattern, count: 1...)
            recoveryPattern = AnyGrammarPattern(recoveryRange)
        } else {
            let rangeCount = range.count
            // Check if the max pattern repetition count has parsed
            guard rangeCount > 1 else {
                return .success
            }
            
            if rangeCount > 2 {
                // If lowerBound is 1, only shift the upper bound
                let lowerBoundShift = lowerBound == 1 ? 0 : 1 - Int(lowerBound)
                let updatedRange = range.shifting(lowerBoundBy: lowerBoundShift, upperBoundBy: -Int(lowerBound))
                let recoveryRange = RepeatingGrammarPattern(pattern: pattern, count: updatedRange)
                recoveryPattern = AnyGrammarPattern(recoveryRange)
            } else {
                // There is only a single element more that can be consumed
                recoveryPattern = AnyGrammarPattern(pattern)
            }
        }
        context.addRecoveryPoint(at: index, pattern: recoveryPattern)
        return .success
    }
}
extension GrammarPattern {
    func repeating(count: ClosedRange<UInt>) -> RepeatingGrammarPattern<Self> {
        return RepeatingGrammarPattern(self, count: count)
    }
    func repeating(count: PartialRangeFrom<UInt>) -> RepeatingGrammarPattern<Self> {
        return RepeatingGrammarPattern(self, count: count)
    }
    func repeating(count: PartialRangeThrough<UInt>) -> RepeatingGrammarPattern<Self> {
        return RepeatingGrammarPattern(self, count: count)
    }
    func repeating(count: PartialRangeUpTo<UInt>) -> RepeatingGrammarPattern<Self> {
        return RepeatingGrammarPattern(self, count: count)
    }
    func repeating(count: Range<UInt>) -> RepeatingGrammarPattern<Self> {
        return RepeatingGrammarPattern(self, count: count)
    }
    func repeating(count: UInt) -> RepeatingGrammarPattern<Self> {
        return RepeatingGrammarPattern(self, count: count)
    }
}

class RecursiveGrammarPattern<U>: GrammarPattern where U: Parsable {
    typealias Base = U
    
    var pattern: AnyGrammarPattern<U>!
    init<P>(_ recursivePattern: (_ selfPattern: RecursiveGrammarPattern<U>) -> P) where P: GrammarPattern, P.Base == U {
        self.pattern = AnyGrammarPattern(recursivePattern(self))
    }
    func parse(from context: GrammarParsingContext<U>, index: GrammarPatternIndex) -> GrammarParsingResult {
        pattern.parse(from: context, index: index)
    }
}

class SharedGrammarPattern<U>: GrammarPattern where U: Parsable {
    typealias Base = U
    
    var pattern: AnyGrammarPattern<U>
    
    init() {
        pattern = AnyGrammarPattern(NeverGrammarPattern())
    }
    init<P>(_ pattern: P) where P: GrammarPattern, P.Base == U {
        self.pattern = AnyGrammarPattern(pattern)
    }
    
    func parse(from context: GrammarParsingContext<U>, index: GrammarPatternIndex) -> GrammarParsingResult {
        pattern.parse(from: context, index: index)
    }
}

struct NeverGrammarPattern<U>: GrammarPattern where U: Parsable {
    typealias Base = U
    init() { }
    
    func parse(from context: GrammarParsingContext<U>, index: GrammarPatternIndex) -> GrammarParsingResult {
        return .failure
    }
}
