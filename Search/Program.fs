open Search

[<EntryPoint>]
let main (argv: string []) : int =
    let initialState = ConsoleIOLoop.Init (GenerateExampleFile.ReadExampleFile ())
    ConsoleIOLoop.Run initialState
    0 // return an integer exit code
