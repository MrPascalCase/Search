module Tests.ExpectedQueryResult

open NUnit.Framework
open Search.Operation
open Search.QueryResult

type ExpectedQueryResult<'a> =
    { value: 'a
      distance: double
      edits: Operation list }

let AssertEdits (expected: seq<Operation>) (actual: seq<Operation>) : unit =
    for x, y in
        Seq.zip expected actual
        |> Seq.filter (fun (x, y) -> x <> y) do
        Assert.Fail $"Expected operation ({x}) <> actual operation ({y})."

let AssertResult (expected: ExpectedQueryResult<'a>) (actual: QueryResult<'a>) : unit =
    if expected.value <> actual.value then
        Assert.Fail $"Expected value ({expected.value}) <> actual value ({actual.value})."

    if expected.distance <> actual.distance then
        Assert.Fail $"Expected editDistance ({expected.distance}) <> actual editDistance ({actual.distance})."

    AssertEdits expected.edits actual.edits

let AssertResults (expected: ExpectedQueryResult<'a> seq) (actual: QueryResult<'a> seq) : unit =
    for x, y in Seq.zip expected actual do
        AssertResult x y
