module Tests.ExpectedQueryResult

open NUnit.Framework
open Search.Operation
open Search.QueryResult

type ExpectedQueryResult =
    { word : string
      distance: double
      edits: Operation list }

let AssertEdits (expected: seq<Operation>) (actual: seq<Operation>) : unit =
    for x, y in
        Seq.zip expected actual
        |> Seq.filter (fun (x, y) -> x <> y) do
        Assert.Fail $"Expected operation ({x}) <> actual operation ({y})."

let AssertResult (expected: ExpectedQueryResult) (actual: QueryResult) : unit =
    if expected.word <> actual.word then
        Assert.Fail $"Expected word ({expected.word}) <> actual word ({actual.word})."
    
    if expected.distance <> actual.distance then
        Assert.Fail $"Expected edit distance ({expected.distance}) <> actual edit distance ({actual.distance})."

    AssertEdits expected.edits actual.edits

let AssertResults (expected: ExpectedQueryResult seq) (actual: QueryResult seq) : unit =
    for x, y in Seq.zip expected actual do
        AssertResult x y
