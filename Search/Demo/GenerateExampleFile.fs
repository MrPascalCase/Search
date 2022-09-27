module Search.GenerateExampleFile

open System
open System.IO

let GenerateWordFile () : unit =
    let isAllLetters word =
        word
        |> Seq.tryFind (fun c -> not (Char.IsLetter c)) = None

    let inputPath = "../../../wikipage.txt"
    let outputPath = "../../../exampleWords.txt"

    let content = File.ReadAllText inputPath

    let words =
        (content.Split())
        |> Array.toList
        |> List.filter (fun word -> not (String.IsNullOrWhiteSpace(word)))
        |> List.filter isAllLetters
        |> List.filter (fun word -> word.Length >= 5)
        |> List.map (fun word -> word.ToLower())
        |> List.distinct
        |> List.sort

    File.WriteAllLines(outputPath, words)

let ReadExampleFile () : string list =
    let outputPath = "../../../exampleWords.txt"
    (File.ReadAllLines outputPath) |> Array.toList
