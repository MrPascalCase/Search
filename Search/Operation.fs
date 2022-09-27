module Search.Operation

open System

type public Operation =
    | Match of char
    | Substitution of char * char
    | Discard of char
    | Generation of char

    override this.ToString() =
        match this with
        | Match x -> $"Match '{x}''"
        | Substitution (d, q) -> $"Substitute '{d}' for '{q}'"
        | Discard q -> $"Discard '{q}'"
        | Generation d -> $"Generate '{d}'"

    member private this.SetConsoleColor() =
        match this with
        | Match _ -> Console.ForegroundColor <- ConsoleColor.Green
        | Substitution _ -> Console.ForegroundColor <- ConsoleColor.Magenta
        | Discard _ -> Console.ForegroundColor <- ConsoleColor.Gray
        | Generation _ -> Console.ForegroundColor <- ConsoleColor.Red

    member private this.GetDisplayCharacter() =
        match this with
        | Match x -> x
        | Substitution (_, y) -> y
        | Discard x -> x
        | Generation x -> x

    member public this.WriteToConsole() =
        let currentForeground = Console.ForegroundColor
        this.SetConsoleColor()
        Console.Write(this.GetDisplayCharacter())
        Console.ForegroundColor <- currentForeground