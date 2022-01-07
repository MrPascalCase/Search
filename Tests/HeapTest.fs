module Tests.HeapTest

open NUnit.Framework
open Search

[<Test>]
let Test_1_insert () =
    let h =
        Heap.empty
        |> Heap.insert 1
        
    Assert.AreEqual(1, Heap.min h)  

[<Test>]
let Test_2_inserts () =
    let h =
        Heap.empty 
        |> Heap.insert 1
        |> Heap.insert 0
        
    Assert.AreEqual(0, Heap.min h)   
        
[<Test>]
let Test_emptyHeap () =
    let h =
        Heap.empty<int> 
        |> Heap.insert 1
        |> Heap.removeMin
        
    Assert.AreEqual (0, h.Count)
    
[<Test>]
let Test_1_Remove () =
    let h =
        Heap.empty<int>
        |> Heap.removeMin
        
    Assert.AreEqual (0, h.Count) 
    
[<Test>]
let Test_3_inserts () =
    let h =
        Heap.empty<int>
        |> Heap.insert 1
        |> Heap.insert 0
        |> Heap.insert -1
        
    Assert.AreEqual(-1, Heap.min h)
    
[<Test>]
let Test_3_inserts_1_Remove () =
    let h =
        Heap.empty 
        |> Heap.insert 1
        |> Heap.insert 0
        |> Heap.insert -1
        |> Heap.removeMin 
        
    Assert.AreEqual(0, Heap.min h)  

[<Test>]
let Test_Mixed_inserts_Removes () =
    let h =
        Heap.empty<int> 
        |> Heap.insert 1
        |> Heap.insert 0
        |> Heap.removeMin
        |> Heap.insert -1
        |> Heap.insert -2
        |> Heap.removeMin 
        
    Assert.AreEqual(-1, Heap.min h)
    
[<Test>]
let Test_inserts_All_Removed () =
    let h =
        Heap.empty<int> 
        |> Heap.insert 1
        |> Heap.insert 0
        |> Heap.insert -1
        |> Heap.removeMin
        |> Heap.removeMin 
        |> Heap.removeMin 
        
    Assert.AreEqual (0, h.Count) 
    
[<Test>]
let Test_Mixed_inserts_Removes_2 () =
    let h =
        Heap.empty<int>
        |> Heap.insert 1
        |> Heap.insert 0
        |> Heap.insert 0
        |> Heap.removeMin
        |> Heap.removeMin
        |> Heap.removeMin
        |> Heap.insert -1
        |> Heap.insert -2
        |> Heap.insert -2
        |> Heap.removeMin 
        
    Assert.AreEqual(-2, Heap.min h)
    
[<Test>]
let Test_ToString () =
    let h =
        Heap.empty<int>
        |> Heap.insert 1
        |> Heap.insert 0
        |> Heap.insert 2
        
    Assert.AreEqual("[0, 1, 2]", h.ToString())
    

[<StructuralEquality; CustomComparison>] 
type CmpDummy =
    | A
    | B
    | C
    interface System.IComparable with 
        member this.CompareTo otherObj =
            let other = otherObj :?> CmpDummy
            match (this, other) with
            | A, A -> 0
            | B, B -> 0
            | C, C -> 0
            | A, _ -> -1
            | B, C -> -1
            | _ -> 1
    
[<Test>]
let Test_Custom_Compare () =
    let h =
        Heap.empty<CmpDummy>
        |> Heap.insert B
        |> Heap.insert C
        |> Heap.insert A
        
    Assert.AreEqual(A, Heap.min h)
    
[<Test>]
let Test_Custom_Compare_With_Remove () =
    let h =
        Heap.empty<CmpDummy>
        |> Heap.insert B
        |> Heap.insert C
        |> Heap.insert A
        |> Heap.removeMin
        
    Assert.AreEqual(B, Heap.min h)
    
[<Test>]
let Test_Custom_Compare_With_2_Removes () =
    let h =
        Heap.empty<CmpDummy>
        |> Heap.insert B
        |> Heap.insert C
        |> Heap.insert A
        |> Heap.removeMin
        |> Heap.removeMin
        
    Assert.AreEqual(C, Heap.min h)
        
