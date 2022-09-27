module Search.QueryResult

open System
open Search.Levenshtein
open Search.Operation

type public QueryResult =
    { word: string
      query: string
      distance: double
      edits: Operation list
      searchTime: TimeSpan
      openNodes: int
      closedNodes: int }

    override this.ToString() =
        let operations =
            "["
            + String.concat ", " (this.edits |> List.map (fun x -> x.ToString()))
            + "]"

        $"Word={this.word}, Distance={this.distance}, Edits={operations}"

    member this.WriteToConsoleLine() =
        for edit in this.edits do
            edit.WriteToConsole()

        Console.Write $" Distance={this.distance}"
        Console.Write $" Word={this.word}"
        Console.Write $" ({this.searchTime.TotalMilliseconds}ms)"
        Console.WriteLine()

let internal assembleQueryResult (l: Levenshtein) (word: string) (query: string) (searchTime: TimeSpan) =
    let takeOperation node =
        match node with
        | Root _ -> None
        | Node n -> Some n.operation

    { word = word
      query = query
      distance = getCost l
      edits =
        l
        |> ancestors
        |> List.choose takeOperation
        |> List.rev
      searchTime = searchTime
      openNodes = 0
      closedNodes = 0 }
