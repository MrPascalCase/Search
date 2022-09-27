module Tests.LevensteinMapTest

open NUnit.Framework
open Search.Levenshtein
open Search.LevensteinMap
open Tests.ExpectedQueryResult
open Search.Operation

[<Test>]
let Test_Match_a () =
    // Arrange
    let map = LevenshteinMap.empty |> LevenshteinMap.add "a"
    
    // Act
    let results = map |> LevenshteinMap.query "a"

    // Assert
    AssertResults
        [ { word = "a"
            distance = 0.0
            edits = [ Match 'a' ] } ]
        results

[<Test>]
let Test_Match_ab () =
    // Arrange
    let map = LevenshteinMap.empty |> LevenshteinMap.add "ab"
    
    // Act
    let results = map |> LevenshteinMap.query "ab"

    // Assert
    AssertResults
        [ { word = "ab"
            distance = 0.0
            edits = [ Match 'a'; Match 'b' ] } ]
        results

[<Test>]
let Test_Match_abc () =
    // Arrange
    let map =
        LevenshteinMap.empty |> LevenshteinMap.add "abc"
        
    // Act
    let results = map |> LevenshteinMap.query "abc"

    // Assert
    AssertResults
        [ { word = "abc"
            distance = 0.0
            edits = [ Match 'a'; Match 'b'; Match 'c' ] } ]
        results

[<Test>]
let Test_Substitute_a_for_b () =
    // Arrange
    let map = LevenshteinMap.empty |> LevenshteinMap.add "a"
    
    // Act
    let results = map |> LevenshteinMap.query "b"

    // Assert
    AssertResults
        [ { word = "a"
            distance = 1.0
            edits = [ Substitution('a', 'b') ] } ]
        results

[<Test>]
let Test_Match_a_or_Substitute_b_for_a () =
    // Arrange
    let map =
        LevenshteinMap.empty 
        |> LevenshteinMap.add "ac" 
        |> LevenshteinMap.add "bc" 
        
    // Act
    let results = map |> LevenshteinMap.query "ac"

    // Assert
    AssertResults
        [ { word = "ac"
            distance = 0.0
            edits = [ Match 'a'; Match 'c' ] }
          { word = "bc"
            distance = 1.0
            edits = [ Substitution('b', 'a'); Match 'c' ] } ]
        results

[<Test>]
let Test_in_word_substitution () =
    // Arrange
    let map =
        LevenshteinMap.empty |> LevenshteinMap.add "abccba"
        
    // Act
    let results = map |> LevenshteinMap.query "abddba"

    // Assert
    AssertResults
        [ { word = "abccba"
            distance = 2.0
            edits =
                [ Match 'a'
                  Match 'b'
                  Substitution('c', 'd')
                  Substitution('c', 'd')
                  Match 'b'
                  Match 'a' ] } ]
        results

[<Test>]
let Test_in_word_discard () =
    // Arrange
    let map =
        LevenshteinMap.empty |> LevenshteinMap.add "abba"

    // Act
    let results = map |> LevenshteinMap.query "abddba"

    // Assert
    AssertResults
        [ { word = "abba"
            distance = 2.0
            edits =
                [ Match 'a'
                  Match 'b'
                  Discard 'd'
                  Discard 'd'
                  Match 'b'
                  Match 'a' ] } ]
        results
        
[<Test>]
let Test_in_word_generate () =
    // Arrange
    let map =
        LevenshteinMap.empty |> LevenshteinMap.add "abccba"

    // Act
    let results = map |> LevenshteinMap.query "abba"

    // Assert
    AssertResults
        [ { word = "abccba"
            distance = 2.0
            edits =
                [ Match 'a'
                  Match 'b'
                  Generation 'c'
                  Generation 'c'
                  Match 'b'
                  Match 'a' ] } ]
        results
