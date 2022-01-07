module Search.Tries

type internal Tries<'a> =
    { Continuations: Map<char, Tries<'a>>
      Results: 'a list }

let internal emptyTries =
    { Continuations = Map.empty
      Results = [] }

let internal add (key: string) (value: 'a) (tries: Tries<'a>) : Tries<'a> =
    let rec addRec (word: char list) (value: 'a) (tries: Tries<'a>) : Tries<'a> =
        match word with
        | [] ->
            { tries with
                  Results = value :: tries.Results }
        | firstLetter :: restWord ->
            if not (Map.containsKey firstLetter tries.Continuations) then
                let newChild =
                    { Continuations = Map.empty
                      Results = [] }
                    |> addRec restWord value

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
                                  | Some d -> Some(addRec restWord value d)) }

    addRec (key |> Seq.toList) value tries



//let allWords (t: Tries<'a>) : (string * 'a) list =
//    let rec AllNodes (x: Tries<'a>) : Tries<'a> list =
//        x
//        :: (x.Continuations
//            |> Map.toList
//            |> List.map snd
//            |> List.collect AllNodes)
//
//    t
//    |> AllNodes
//    |> List.collect (fun n -> n.Results |> List.map (fun p -> (n.Word, p)))


