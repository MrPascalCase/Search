module Search.Levenshtein

open Search.TriesVisitor
open Search.Operation

// type OperationWithTriesContext(operation: Operation, tries: TriesVisitor option) =
//     do
//         // Class invariant
//         match (operation, tries) with
//         | Match x, Some t when x = t.Character -> () // ok
//         | Substitution (x, y), Some t when x = t.Character && y <> x -> () // ok
//         | Discard _, None -> () // ok
//         | Generation x, Some t when x = t.Character -> () // ok
//         | _ -> failwith "Assert class invariant failed."
//
//     member this.Operation = operation
//     member this.Tries = tries

type Node =
    { parent: Levenshtein
      tries: TriesVisitor
      operation: Operation
      cost: double }

and Levenshtein =
    | Root of TriesVisitor
    | Node of Node

let getCost l =
    match l with
    | Root _ -> 0.0
    | Node n -> n.cost

let rec ancestors l =
    match l with
    | Root _ -> []
    | Node n -> l :: (ancestors n.parent)

let tries l =
    match l with
    | Root t -> t
    | Node n -> n.tries

let private countQueryAdvance (l: Levenshtein) =
    let getOperation (x: Levenshtein) =
        match x with
        | Root _ -> None
        | Node n -> Some n.operation

    let doesConsumeQuery op =
        match op with
        | Some (Match _) -> true
        | Some (Substitution _) -> true
        | Some (Discard _) -> true
        | _ -> false

    l
    |> ancestors
    |> List.filter (getOperation >> doesConsumeQuery)
    |> List.length

let internal isFinal l (query: string) = countQueryAdvance l = query.Length

let rec private getContinuations (l: Levenshtein) : seq<TriesVisitor> = (tries l |> continuations)

let rec private merge (element: Levenshtein) (sequence: Levenshtein seq) =
    seq {
        match Seq.tryHead sequence with
        | None -> yield element
        | Some head ->
            if getCost element <= getCost head then
                yield element
                yield! sequence
            else
                yield head
                yield! (merge element (Seq.tail sequence))
    }

/// It never makes sense to have a Discard after a Generation (or vice-versa)
/// as a Substitution should be used instead.
type private SuccessorTypes =
    | All
    | NoDiscard
    | NoGeneration

let private getSuccessorTypes (l: Levenshtein) =
    match l with
    | Root _ -> All
    | Node n ->
        match n.operation with
        | Discard _ -> NoGeneration
        | Generation _ -> NoDiscard
        | _ -> All

let private getMatch (l: Levenshtein) (c: char) (cost: Operation -> double) =
    let matchingChar (x: TriesVisitor) = x.Character = c

    let matchingContinuation =
        (getContinuations l) |> Seq.tryFind matchingChar

    seq {
        if matchingContinuation.IsSome then
            yield
                Node
                    { parent = l
                      operation = Match c
                      tries = matchingContinuation.Value
                      cost = getCost l + (cost (Match c)) }
    }

let private getSubstitutions (l: Levenshtein) (c: char) (cost: Operation -> double) =
    let createNode (visitor: TriesVisitor) =
        let op = Substitution(visitor.Character, c)
        Node
            { parent = l
              operation = op
              tries = visitor
              cost = getCost l + (cost op) }

    (getContinuations l)
    |> Seq.filter (fun x -> x.Character <> c)
    |> Seq.map createNode
    |> Seq.sortBy getCost

let private getDiscard (l: Levenshtein) (c: char) (cost: Operation -> double) : Levenshtein =
    let op = Discard c
    Node
        { parent = l
          operation = op
          tries = tries l
          cost = getCost l + (cost op) }

let rec private getGenerations (l: Levenshtein) (c: char) (cost: Operation -> double) : Levenshtein seq =
    let createNode (visitor: TriesVisitor) =
        let op = Generation(visitor.Character)
        Node
            { parent = l
              operation = op
              tries = visitor
              cost = getCost l + (cost op) }

    (getContinuations l)
    |> Seq.map createNode
    |> Seq.collect (fun l -> getSuccessors l c cost)

and private getSuccessors (l: Levenshtein) (c: char) (cost: Operation -> double) : Levenshtein seq =
    seq {
        yield! (getMatch l c cost)
        yield! (getSubstitutions l c cost) // Substitutions have Cost <= 1

        match getSuccessorTypes l with
        | NoDiscard -> yield! (getGenerations l c cost)
        | NoGeneration -> yield (getDiscard l c cost)
        | All -> yield! merge (getDiscard l c cost) (getGenerations l c cost)
    }

let internal Successors (l: Levenshtein) (query: string) (cost: Operation -> double) : Levenshtein seq =
    let nextQueryChar =
        query.Substring(countQueryAdvance l).Chars 0

    getSuccessors l nextQueryChar cost

let internal getResults (l: Levenshtein) : seq<string> = getResults (tries l)
