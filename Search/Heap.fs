module Search.Heap

type internal Heap<'a when 'a: comparison> =
    private
        { List: System.Collections.Generic.List<'a> }

    member this.Count = this.List.Count

    override this.ToString() =
        "["
        + (this.List
           |> Seq.sort
           |> Seq.map (fun e -> e.ToString())
           |> String.concat ", ")
        + "]"

let internal empty<'a when 'a: comparison> =
    { List = new System.Collections.Generic.List<'a>() }

let private leftChild i = 2 * i + 1
let private rightChild i = 2 * i + 2
let private parent i = (i - 1) / 2
let private hasLeftChild i (h: Heap<_>) = (leftChild i) < h.Count
let private hasRightChild i (h: Heap<_>) = (rightChild i) < h.Count

let private swap i j h =
    let tmp = h.List.[i]
    h.List.[i] <- h.List.[j]
    h.List.[j] <- tmp

let rec private swim i (h: Heap<_>) =
    if h.List.[i] < h.List.[parent i] then
        swap i (parent i) h
        swim (parent i) h

let rec private sink i this =
    if not (hasLeftChild i this) then
        ()
    else
        let mutable minChild = leftChild i

        if hasRightChild i this
           && this.List.[rightChild i] < this.List.[leftChild i] then
            minChild <- rightChild i

        if this.List.[minChild] < this.List.[i] then
            swap i minChild this
            sink minChild this

let internal insert t h =
    h.List.Add t
    swim (h.List.Count - 1) h
    h

let internal min (h: Heap<_>) =
    if h.Count = 0 then
        failwith "Cannot get the Min from an empty heap."
    else
        h.List.[0]

let internal removeMin (h: Heap<_>) =
    if (h.Count = 0) then
        h
    else
        swap 0 (h.Count - 1) h
        h.List.RemoveAt(h.List.Count - 1)
        sink 0 h
        h
