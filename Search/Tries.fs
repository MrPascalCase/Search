module Search.Tries

type Tries =
    { Continuations: Map<char, Tries>
      IsResult: bool }

let emptyTries =
    { Continuations = Map.empty
      IsResult = false }

let add (key: string) (tries: Tries) : Tries =
    let rec addRec (word: char list) (tries: Tries) : Tries =
        match word with
        | [] ->
            { tries with IsResult = true }
        | firstLetter :: restWord ->
            if not (Map.containsKey firstLetter tries.Continuations) then
                let newChild =
                    { Continuations = Map.empty
                      IsResult = false }
                    |> addRec restWord

                { tries with
                      Continuations =
                          tries.Continuations
                          |> (Map.add firstLetter newChild) }
            else
                { tries with
                      Continuations =
                          tries.Continuations
                          |> Map.change
                              firstLetter
                              (fun d ->
                                  match d with
                                  | None -> None
                                  | Some d -> Some(addRec restWord d)) }

    addRec (key |> Seq.toList) tries


