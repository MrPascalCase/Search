module Search.Heap

type internal Heap<'a, 'b when 'b: comparison> =
    private
        { CompareBy: 'a -> 'b
          List: System.Collections.Generic.List<'a> }

    member this.Count = this.List.Count

    override this.ToString() =
        "["
        + (this.List
           |> Seq.sortBy this.CompareBy
           |> Seq.map (fun e -> e.ToString())
           |> String.concat ", ")
        + "]"

let internal empty compareBy =
    { CompareBy = compareBy
      List = System.Collections.Generic.List<_>() }

let private leftChild i = 2 * i + 1
let private rightChild i = 2 * i + 2
let private parent i = (i - 1) / 2
let private hasLeftChild i (h: Heap<_, _>) = (leftChild i) < h.Count
let private hasRightChild i (h: Heap<_, _>) = (rightChild i) < h.Count

let private swap i j h =
    let tmp = h.List.[i]
    h.List.[i] <- h.List.[j]
    h.List.[j] <- tmp

let rec private swim i h =
    if h.CompareBy h.List.[i] < h.CompareBy h.List.[parent i] then
        swap i (parent i) h
        swim (parent i) h

let rec private sink index heap =
    if not (hasLeftChild index heap) then
        ()
    else
        let mutable minChild = leftChild index

        if hasRightChild index heap
           && heap.CompareBy heap.List.[rightChild index] < heap.CompareBy heap.List.[leftChild index] then
            minChild <- rightChild index

        if heap.CompareBy heap.List.[minChild] < heap.CompareBy heap.List.[index] then
            swap index minChild heap
            sink minChild heap

let internal insert t h =
    h.List.Add t
    swim (h.List.Count - 1) h
    h

let internal min (h: Heap<_, _>) =
    if h.Count = 0 then
        failwith "Cannot get the Min from an empty heap."
    else
        h.List.[0]

let internal removeMin (h: Heap<_, _>) =
    if (h.Count > 0) then
        swap 0 (h.Count - 1) h
        h.List.RemoveAt(h.List.Count - 1)
        sink 0 h

    h