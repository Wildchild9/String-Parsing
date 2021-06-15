# Parsing Example

Provided below is the starting code for this example.

```swift
enum BaseToken: Parsable, CaseIterable {
    typealias Base = Self
    
    case a
    case ab
    case bc
    case bcc
    case ccd
    case cdd
    
    var pattern: TokenPattern {
        switch self {
        case .a: return "A"
        case .ab: return "AB"
        case .bc: return "BC"
        case .bcc: return "BCC"
        case .ccd: return "CCD"
        case .cdd: return "CDD"
        }
    }
    
    static var grammarPattern: AnyGrammarPattern<BaseToken> {
	    let pattern = (a || ab) + (bc || bcc) + (ccd || cdd)
        return AnyGrammarPattern(pattern)
    }
}
```

##Parsing Steps

In this section we will go over how the above code can be used to tokenize a string.

```swift
let input = "ABBCCCDD"
let tokenizer = Tokenizer(using: BaseToken.self)
let tokens = try tokenizer.tokenize(input)
```

### Steps:
 
 1. Create an input string, `"ABBCCCDD"`, that will be tokenized.
 
 2. Initialize a `Tokenizer<BaseToken>` instance using `Tokenizer(using:)`, passing `BaseToken.self` as the argument.
 
 3. Call `tokenize(_:)` method on tokenizer instance passing in the input string to be parsed. (note that all of the steps following this one happen internally).
 
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
 
 47. Propogate return value of `parseInput()` to return value of the invocation the `tokenize(_:)` method on our tokenizer instance.
