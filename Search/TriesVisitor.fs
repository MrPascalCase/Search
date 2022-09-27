module Search.TriesVisitor

open Search.Tries

type internal TriesVisitor(parent: TriesVisitor option, node: Tries, character: char) =
    member this.Node = node
    member this.Parent = parent
    member this.Character = character

    member this.Ancestors =
        this
        :: (match this.Parent with
            | Some parent -> parent.Ancestors
            | None -> [])

    member this.Word =
        this.Ancestors
        |> List.rev // Root first
        |> List.skip 1 // Skip root
        |> List.map (fun n -> n.Character.ToString())
        |> String.concat ""

let rec internal continuations (t : TriesVisitor) : TriesVisitor seq =
        t.Node.Continuations
        |> Map.toSeq
        |> Seq.map (fun (k, v) -> TriesVisitor(Some t, v, k))

let internal emptyVisitor (t: Tries) = TriesVisitor(None, t, '\000')

let rec internal getResults (t: TriesVisitor) : seq<string> =
    seq {
        if t.Node.IsResult then yield t.Word

        for child in continuations t do
            yield! (getResults child)
    }
