//
//  Parsable.swift
//  String Parsing
//
//  Created by Noah Wilder on 2020-05-15.
//  Copyright © 2020 Noah Wilder. All rights reserved.
//

import Foundation


//TODO: Add options static var that takes a ParseOptions option set with things like .ignoreWhitespace
protocol Parsable: GrammarPattern where TokenType == Self { //} where GrammarPattern.Token == Self {
    var pattern: Pattern { get }
    var label: String { get }
    static var availableTokens: [Self] { get }
    static var grammarPattern: AnyGrammarPattern<Self> { get }
}
extension Parsable where Self: CaseIterable {
    static var availableTokens: [Self] {
        return Array(allCases)
    }
}
////extension Parsable {
////    typealias TokenType = Self
////}
//
extension Parsable {
    func consuming<T>(input: T) -> (tokens: [MatchedToken<TokenType>], remaining: Substring)? where T : StringProtocol, T.SubSequence == Substring {
        if let remaining = pattern.consumed(from: input) {
            return (tokens: [MatchedToken(classification: self, match: input[input.startIndex..<remaining.startIndex])], remaining: remaining)
        }
        return nil
    }
}


//extension Parsable {
//    func consumed<T>(from input: T) -> Substring? where T : StringProtocol, T.SubSequence == Substring {
//        return pattern.consumed(from: input)
//    }
//}



extension Parsable {
    var label: String {
        return String(describing: self)
    }
}
extension Parsable where Self: RawRepresentable, RawValue == String {
    var label: String {
        return rawValue
    }
}

//MARK: - Errors

enum ParseError: LocalizedError {
    case couldNotParseTokens
    case cannotMatchToken(source: String, position: String.Index)

    var failureReason: String? {
        switch self {
        case .couldNotParseTokens:
            return "Tokenization failed."
        case let .cannotMatchToken(source, position):
            return "Tokenization failed; could not find any tokens to match past this \'\(source[position])\'."
        }
    }
    var recoverySuggestion: String? {
        switch self {
        case .couldNotParseTokens:
            return "Please ensure that your expression has valid grammar."
        case let .cannotMatchToken(source, position):
            guard !source[position].isNewline else { return "" }
            var lineStartIndex = source.startIndex
            if let idx = source[..<position].lastIndex(where: \.isNewline), idx != lineStartIndex {
                lineStartIndex = source.index(before: source.startIndex)
            }
            let lineEndIndex = source[lineStartIndex...].firstIndex(where: \.isNewline) ?? source.endIndex
            let positionOffset = source.distance(from: lineStartIndex, to: position)
            let incorrectLength = source.distance(from: position, to: lineEndIndex) - 1
            return "\(source[lineStartIndex..<lineEndIndex])\n" + String(repeating: " ", count: positionOffset) + "^" + String(repeating: "~", count: incorrectLength)
        }
    }
    var localizedDescription: String {
        return "Parsing error: " + failureReason! + "\n\t" + recoverySuggestion!.replacingOccurrences(of: "\n", with: "\n\t")
    }
//    var errorDescription: String {
//        switch self {
//        case let .cannotMatchToken(source, position):
//            let positionOffset = source.distance(from: source.startIndex, to: position)
//            var errorMessage = "error:
//            errorMessage += "\n\t" + source
//            errorMessage += "\n\t" + String(repeating: " ", count: positionOffset) + "^~~"
//            return errorMessage
//        }
//    }
}

//MARK: - Tokenizer
struct Tokenizer<T> where T: Parsable {
    init(using tokenSource: T.Type) { }
    
    func tokenize(_ input: String) throws -> [MatchedToken<T>] {
        guard let output = T.grammarPattern.consuming(input: input) else {
            throw ParseError.couldNotParseTokens
        }
        if output.remaining.count > 0 {
            print(String(output.remaining))
            throw ParseError.cannotMatchToken(source: input, position: output.remaining.startIndex)
        }
        return output.tokens
//        let tokenClassifications = T.availableTokens
//        var substr = input[...]
//        var matchedTokens = [MatchedToken<T>]()
//        // Attempts to consume tokens until substr is empty, at which point the whole input has been tokenized
//
//        searchLoop: while !substr.isEmpty {
//            // Tries to consume each token from the start of substr
//            for classification in tokenClassifications {
//                if let consumedString = classification.pattern.consumed(from: substr) {
//                    let matchedToken = MatchedToken(
//                        classification: classification,
//                        match: substr[..<consumedString.startIndex]
//                    )
//                    substr = consumedString
//                    matchedTokens.append(matchedToken)
//                    continue searchLoop
//                }
//            }
//            let failureIndex = substr.startIndex
//            throw ParseError.cannotMatchToken(source: input, position: failureIndex)
//        }
//        return matchedTokens
    }
}

//MARK: - Intermediate Tokens
struct MatchedToken<T> where T: Parsable {
    var classification: T
    var match: Substring
}
extension MatchedToken: CustomStringConvertible {
    var description: String {
        return classification.label
    }
}
