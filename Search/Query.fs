module Search.Query

open System
open System.Collections.Generic
open Search
open Search.QueryResult
open Search.TriesVisitor
open Search.Operation
open Search.Tries
open Search.Levenshtein

type internal SearchResultWithContext =
    { pathToResult: Levenshtein
      searchTime : TimeSpan }

let internal enumerateResults (t: Tries) (query: string) (cost : Operation -> double) =
    let mutable heap = Heap.empty |> Heap.insert (empty (emptyVisitor t))
    let mutable openNodes = 1
    let mutable closedNodes = 1
    let stopwatch = System.Diagnostics.Stopwatch.StartNew ()
    let hashset = new HashSet<string>()
    
    seq {
        while heap.Count > 0 do
            let min = Heap.min heap
            heap <- heap |> Heap.removeMin
            closedNodes <- closedNodes + 1

            if isFinal min query then
                let results = getResults min
                yield!
                    results
                    |> Seq.filter (fun r -> not (hashset.Contains(r)))
                    |> Seq.map (fun x -> assembleQueryResult min x query stopwatch.Elapsed)

                hashset.UnionWith(results)
            else
                let successors = (Successors min query cost)
                for x in successors do
                    heap <- heap |> Heap.insert x
                    openNodes <- openNodes + 1
    }
