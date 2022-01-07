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

type public LevensteinMap<'a> private (t: Tries.Tries<'a>, costFunction: Operation -> double) =
    member internal this.Tries: Tries.Tries<'a> = t
    member internal this.CostFunction = costFunction

    static member public empty: LevensteinMap<'a> =
        LevensteinMap(Tries.emptyTries, StdCostFunction)

    static member public withCostFunction (costFunction: Operation -> double) (map: LevensteinMap<'a>) =
        LevensteinMap(map.Tries, costFunction)

    static member public add<'a> (key: string) (value: 'a) (map: LevensteinMap<'a>) : LevensteinMap<'a> =
        let newTries = map.Tries |> Tries.add key value
        LevensteinMap(newTries, map.CostFunction)

    static member public query<'a> (key: string) (map: LevensteinMap<'a>) : QueryResult<'a> seq =
        Query.enumerateResults map.Tries key map.CostFunction
