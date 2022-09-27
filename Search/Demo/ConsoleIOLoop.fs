module Search.ConsoleIOLoop

open System
open Search.LevensteinMap

type State =
    { currentQuery: string
      doExit: bool
      map: LevenshteinMap
      resultShowCount: int }

let Init (results: string list) : State =
    let mutable dictionary = LevenshteinMap.empty

    for result in results do
        dictionary <- dictionary |> LevenshteinMap.add result

    { currentQuery = ""
      doExit = false
      map = dictionary
      resultShowCount = 10 }

let Show state =
    Console.Clear()
    Console.WriteLine($"Query={state.currentQuery}")

    let results =
        state.map
        |> LevenshteinMap.query state.currentQuery
        |> Seq.truncate state.resultShowCount

    for result in results do
        result.WriteToConsoleLine()

    Console.SetCursorPosition($"Query={state.currentQuery}".Length, 0)

let ReadKey state =
    let shortenCurrentQueryEnd state =
        let newQuery =
            if state.currentQuery.Length > 0 then
                state.currentQuery.Substring(0, state.currentQuery.Length - 1)
            else
                state.currentQuery

        { state with currentQuery = newQuery }

    let setExit state = { state with doExit = true }

    let appendCharToQuery character state =
        { state with currentQuery = state.currentQuery + character.ToString() }

    let info = Console.ReadKey()

    match info.Key with
    | ConsoleKey.Backspace -> shortenCurrentQueryEnd state
    | ConsoleKey.Escape -> setExit state
    | _ -> appendCharToQuery info.KeyChar state

let rec Run (state: State) : unit =
    Show state
    let newState: State = ReadKey state

    if not newState.doExit then
        Run newState
    else
        ()
