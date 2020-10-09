//
//  Parsable.swift
//  String Parsing
//
//  Created by Noah Wilder on 2020-05-15.
//  Copyright © 2020 Noah Wilder. All rights reserved.
//

import Foundation


//TODO: Add options static var that takes a ParseOptions option set with things like .ignoreWhitespace
protocol Parsable: GrammarPattern where Base == Self { //} where GrammarPattern.Token == Self {
    var pattern: TokenPattern { get }//Pattern { get }
    var label: String { get }
    static var availableTokens: [Self] { get }
    static var grammarPattern: AnyGrammarPattern<Self> { get }
}
extension Parsable where Self: CaseIterable {
    static var availableTokens: [Self] {
        return Array(allCases)
    }
}

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
        let context = GrammarParsingContext<T>(input: input)
        guard let tokens = context.parseInput() else {
            throw ParseError.cannotMatchToken(source: input, position: context.furthestIndex)
        }
        return tokens
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


