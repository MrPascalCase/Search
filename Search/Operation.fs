module Search.Operation

open System
open Search.TriesVisitor

type public Operation =
    | NoOperation
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
        | NoOperation -> ""

    member private this.SetConsoleColor() =
        match this with
        | Match _ -> Console.ForegroundColor <- ConsoleColor.Green
        | Substitution _ -> Console.ForegroundColor <- ConsoleColor.Magenta
        | Discard _ -> Console.ForegroundColor <- ConsoleColor.Gray
        | Generation _ -> Console.ForegroundColor <- ConsoleColor.Red
        | _ -> failwith "Not supported"

    member private this.GetDisplayCharacter() =
        match this with
        | Match x -> x
        | Substitution (_, y) -> y
        | Discard x -> x
        | Generation x -> x
        | _ -> failwith "Not supported"

    member public this.WriteToConsole() =
        let currentForeground = Console.ForegroundColor
        this.SetConsoleColor()
        Console.Write(this.GetDisplayCharacter())
        Console.ForegroundColor <- currentForeground

type internal OperationWithTriesContext(operation: Operation, tries: TriesVisitor option) =
    do
        // Class invariant
        match (operation, tries) with
        | Match x, Some t when x = t.Character -> () // ok
        | Substitution (x, y), Some t when x = t.Character && y <> x -> () // ok
        | Discard _, None -> () // ok
        | Generation x, Some t when x = t.Character -> () // ok
        | NoOperation, Some t when '\000' = t.Character -> () // ok
        | _ -> failwith "Assert class invariant failed."

    member this.Operation = operation
    member this.Tries = tries