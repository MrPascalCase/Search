module Search.KeyboardCost

let upperRow =
    "\000qwertyuiop\000".ToCharArray()

let middleRow =
    "\000asdfghjkl\000".ToCharArray()

let lowerRow =
    "\000yxcvbnm\000".ToCharArray()

let directNeighbours =
    seq {
        for i in [ 1 .. (upperRow.Length - 2) ] do
            yield (upperRow[i], [| upperRow[i - 1]; upperRow[i + 1] |])

        for i in [ 1 .. (middleRow.Length - 2) ] do
            yield (middleRow[i], [| middleRow[i - 1]; middleRow[i + 1] |])

        for i in [ 1 .. (lowerRow.Length - 2) ] do
            yield (lowerRow[i], [| lowerRow[i - 1]; lowerRow[i + 1] |])
    }
    |> Map.ofSeq

let directBelow =
    seq {
        for i in [ 1 .. (upperRow.Length - 2) ] do
            yield (upperRow[i], [| middleRow[i] |])

        for i in [ 1 .. (middleRow.Length - 2) ] do
            yield (middleRow[i], [| lowerRow[i - 1]; lowerRow[i] |])
    }
    |> Map.ofSeq
    
let indirect =
    seq {
        for i in [ 1 .. (upperRow.Length - 2) ] do
            yield (upperRow[i], [| middleRow[i-1] ; middleRow[i+1] |])

        for i in [ 1 .. (middleRow.Length - 2) ] do
            yield (middleRow[i], [| lowerRow[i - 1]; lowerRow[i] |])
    }
    |> Map.ofSeq
    
// let array =
//     let mutable array = new Array<Array
//     seq {
//         for c0 in ['a' .. 'z'] do
//             for c1 in ['a' .. 'z'] do
//                 yield (int c0 - int 'a', int c1 - int 'a', 1.0)      
//     }
//     |> Seq.toArray
