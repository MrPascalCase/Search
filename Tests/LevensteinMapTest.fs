module Tests.LevensteinMapTest

open NUnit.Framework
open Search.Levenstein
open Search.LevensteinMap
open Tests.ExpectedQueryResult
open Search.Operation

[<Test>]
let Test_Match_a () =
    // Arrange
    let map = LevensteinMap.empty |> LevensteinMap.add "a" 1
    
    // Act
    let results = map |> LevensteinMap.query "a"

    // Assert
    AssertResults
        [ { value = 1
            distance = 0.0
            edits = [ Match 'a' ] } ]
        results

[<Test>]
let Test_Match_ab () =
    // Arrange
    let map = LevensteinMap.empty |> LevensteinMap.add "ab" 1
    
    // Act
    let results = map |> LevensteinMap.query "ab"

    // Assert
    AssertResults
        [ { value = 1
            distance = 0.0
            edits = [ Match 'a'; Match 'b' ] } ]
        results

[<Test>]
let Test_Match_abc () =
    // Arrange
    let map =
        LevensteinMap.empty |> LevensteinMap.add "abc" 1
        
    // Act
    let results = map |> LevensteinMap.query "abc"

    // Assert
    AssertResults
        [ { value = 1
            distance = 0.0
            edits = [ Match 'a'; Match 'b'; Match 'c' ] } ]
        results

[<Test>]
let Test_Substitute_a_for_b () =
    // Arrange
    let map = LevensteinMap.empty |> LevensteinMap.add "a" 1
    
    // Act
    let results = map |> LevensteinMap.query "b"

    // Assert
    AssertResults
        [ { value = 1
            distance = 1.0
            edits = [ Substitution('a', 'b') ] } ]
        results

[<Test>]
let Test_Match_a_or_Substitute_b_for_a () =
    // Arrange
    let map =
        LevensteinMap.empty 
        |> LevensteinMap.add "ac" 1
        |> LevensteinMap.add "bc" 2
        
    // Act
    let results = map |> LevensteinMap.query "ac"

    // Assert
    AssertResults
        [ { value = 1
            distance = 0.0
            edits = [ Match 'a'; Match 'c' ] }
          { value = 2
            distance = 1.0
            edits = [ Substitution('b', 'a'); Match 'c' ] } ]
        results

[<Test>]
let Test_in_word_substitution () =
    // Arrange
    let map =
        LevensteinMap.empty |> LevensteinMap.add "abccba" 1
        
    // Act
    let results = map |> LevensteinMap.query "abddba"

    // Assert
    AssertResults
        [ { value = 1
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
        LevensteinMap.empty |> LevensteinMap.add "abba" 1

    // Act
    let results = map |> LevensteinMap.query "abddba"

    // Assert
    AssertResults
        [ { value = 1
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
        LevensteinMap.empty |> LevensteinMap.add "abccba" 1

    // Act
    let results = map |> LevensteinMap.query "abba"

    // Assert
    AssertResults
        [ { value = 1
            distance = 2.0
            edits =
                [ Match 'a'
                  Match 'b'
                  Generation 'c'
                  Generation 'c'
                  Match 'b'
                  Match 'a' ] } ]
        results
