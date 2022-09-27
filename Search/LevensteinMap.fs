module Search.LevensteinMap

open Search
open Search.QueryResult
open Search.Operation

let public StdCostFunction (op: Operation) : double =
    match op with
    | Match _ -> 0.0
    | Substitution _ -> 1.0
    | Discard _ -> 1.0
    | Generation _ -> 1.0
    | _ -> failwith "Not supported."

type public LevenshteinMap private (t: Tries.Tries, costFunction: Operation -> double) =
    member internal this.Tries: Tries.Tries = t
    member internal this.CostFunction = costFunction

    static member public empty: LevenshteinMap =
        LevenshteinMap(Tries.emptyTries, StdCostFunction)

    static member public addCostFunction (costFunction: Operation -> double) (map: LevenshteinMap) =
        LevenshteinMap(map.Tries, costFunction)

    static member public add<'a> (key: string) (map: LevenshteinMap) : LevenshteinMap =
        let newTries = map.Tries |> Tries.add key 
        LevenshteinMap(newTries, map.CostFunction)

    static member public query (key: string) (map: LevenshteinMap) : QueryResult seq =
        Query.enumerateResults map.Tries key map.CostFunction
