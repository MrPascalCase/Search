module Search.Levenshtein

open System
open Search.TriesVisitor
open Search.Operation

[<CustomComparison; CustomEquality>]
type internal Levenshtein =
    { parent: Levenshtein option
      operation: OperationWithTriesContext
      cost: double }
    member this.IsRoot = this.parent = None

    member this.Ancestors =
        this
        :: (match this.parent with
            | Some parent -> parent.Ancestors
            | None -> [])

    override this.Equals other =
        match other with
        | :? Levenshtein as p -> (this :> IComparable<_>).Equals p
        | _ -> failwith ""

    interface IComparable with
        member this.CompareTo other =
            match other with
            | :? Levenshtein as p -> (this :> IComparable<_>).CompareTo p
            | _ -> failwith ""

    interface IComparable<Levenshtein> with
        member this.CompareTo other = this.cost.CompareTo other.cost

    interface IEquatable<Levenshtein> with
        member this.Equals(other: Levenshtein) : bool = this.cost.Equals other.cost

    override this.GetHashCode() = failwith "Not supported."


let internal empty (t: TriesVisitor) =
    { parent = None
      operation = OperationWithTriesContext(NoOperation, Some t)
      cost = 0.0 }

let private getLastTries (l: Levenshtein) : TriesVisitor = l.Ancestors |> List.pick (fun x -> x.operation.Tries)

let private countQueryAdvance (l: Levenshtein) =
    let getOperation (x: Levenshtein) = x.operation.Operation

    let doesConsumeQuery op =
        match op with
        | Match _ -> true
        | Substitution _ -> true
        | Discard _ -> true
        | _ -> false

    l.Ancestors
    |> List.filter (getOperation >> doesConsumeQuery)
    |> List.length

let internal isFinal l (query: string) = countQueryAdvance l = query.Length

let rec private getContinuations (l: Levenshtein) : seq<TriesVisitor> =
    (l.Ancestors
     |> Seq.pick (fun n -> n.operation.Tries))
     |> TriesVisitor.continuations

let rec private merge (element: Levenshtein) (sequence: Levenshtein seq) =
    seq {
        match Seq.tryHead sequence with
        | None -> yield element
        | Some head ->
            if element.cost <= head.cost then
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
    match l.operation.Operation with
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
                { parent = Some l
                  operation = OperationWithTriesContext(Match c, matchingContinuation)
                  cost = l.cost + cost (Match c) }
    }

let private getSubstitutions (l: Levenshtein) (c: char) (cost: Operation -> double) =
    let createNode (visitor: TriesVisitor) =
        let op = Substitution(visitor.Character, c)

        let opContext =
            OperationWithTriesContext(op, Some visitor)

        { parent = Some l
          operation = opContext
          cost = l.cost + (cost op) }

    (getContinuations l)
    |> Seq.filter (fun x -> x.Character <> c)
    |> Seq.map createNode
    |> Seq.sort 

let private getDiscard (l: Levenshtein) (c: char) (cost: Operation -> double) : Levenshtein =
    let op = Discard c
    let opContext = OperationWithTriesContext(op, None)

    { parent = Some l
      operation = opContext
      cost = l.cost + (cost op) }

let rec private getGenerations (l: Levenshtein) (c: char) (cost: Operation -> double) : Levenshtein seq =
    let createNode (visitor: TriesVisitor) =
        let op = Generation(visitor.Character)

        let opContext =
            OperationWithTriesContext(op, Some visitor)

        { parent = Some l
          operation = opContext
          cost = l.cost + (cost op) }

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
//|> Seq.sortBy (fun l -> l.Cost)

let internal Successors (l: Levenshtein) (query : string) (cost: Operation -> double) : Levenshtein seq =
        let nextQueryChar = query.Substring(countQueryAdvance l).Chars 0
        getSuccessors l nextQueryChar cost
    
let internal getResults : Levenshtein -> seq<string> =
    getLastTries >> TriesVisitor.getResults
