module Search.TriesVisitor

open Search.Tries


type internal TriesVisitor<'a>(parent: TriesVisitor<'a> option, node: Tries<'a>, character: char) =
    member this.Node = node
    member this.Parent = parent
    member this.Character = character

    member this.Continuations: TriesVisitor<'a> seq =
        this.Node.Continuations
        |> Map.toSeq
        |> Seq.map (fun (k, v) -> (TriesVisitor(Some this, v, k)))

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


let internal emptyVisitor (t: Tries<'a>) = TriesVisitor(None, t, '\000')

let rec internal getResults<'a> (t: TriesVisitor<'a>) : seq<string * 'a> =
    seq {
        yield! t.Node.Results |> List.map (fun x -> (t.Word, x))

        for child in t.Continuations do
            yield! (getResults child)
    }
