module Search.QueryResult

open System
open Search.Levenstein
open Search.Operation

type public QueryResult<'a> =
    { key: string
      value: 'a
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

        $"Value={this.value.ToString()}, Distance={this.distance}, Edits={operations}"

    member this.WriteToConsoleLine() =
        for edit in this.edits do
            edit.WriteToConsole()

        Console.Write $" Distance={this.distance}"
        Console.Write $" Value={this.value.ToString()}"
        Console.Write $" ({this.searchTime.TotalMilliseconds}ms)"
        Console.WriteLine()

let internal assembleQueryResult (l: Levenstein<'a>) (key: string) (value: 'a) (query: string) (searchTime: TimeSpan) =
    { key = key
      value = value
      query = query
      distance = l.cost
      edits = l.Ancestors
                  |> List.map (fun x -> x.operation.Operation)
                  |> List.rev
                  |> List.skip 1 // RootNode
      searchTime = searchTime
      openNodes = 0
      closedNodes = 0 }
