namespace Minori.Slices.Test.Slice

open System
open Testexp
open Minori.Slices
open Minori.Slices.Test
open System.Collections.Generic


module length =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice ArgGen.int (ArgGen.intRange 0 16)
        test Slice.length slice ==> Assert.equal slice.Length
    }


module add =
    [<Fact>]
    let Emptyに値を追加 () = testing {
        let! item = ArgGen.asciiChar
        let! result = test Slice.add<char> (item, Slice.empty)
        Assert.sequentialEqual [| item |] result
    }

    [<Fact>]
    let キャパシティが足りているSliceに追加 () = testing {
        let! capacity = ArgGen.intRange 1 8
        let! item = ArgGen.asciiChar
        let! result = test Slice.add<char> (item, Slice.create capacity)
        Assert.sequentialEqual [| item |] result
        Assert.equal capacity result.Capacity
    }

    [<Fact>]
    let オーナーでないSliceに追加 () = testing {
        let! item0 = 16, ArgGen.asciiChar
        let! item1 = 16, ArgGen.asciiChar
        let! item2 = 16, ArgGen.asciiChar
        let slice = Slice.add item0 (Slice.create 4)
        let otherSlice = Slice.add item1 slice
        let! result = test Slice.add<char> (item2, slice)
        Assert.sequentialEqual [| item0; item1 |] otherSlice
        Assert.sequentialEqual [| item0; item2 |] result
    }


module addSpan =
    let testFunc (items: char[]) slice = Slice.addSpan (ReadOnlySpan(items)) slice

    [<Fact>]
    let Emptyに値を追加 () = testing {
        let! items = ArgGen.array ArgGen.asciiChar (ArgGen.intRange 0 8)
        let! result = test testFunc (items, Slice.empty)
        Assert.sequentialEqual items result
    }

    [<Fact>]
    let キャパシティが足りているSliceに追加 () = testing {
        let! items = ArgGen.array ArgGen.asciiChar (ArgGen.intRange 0 8)
        let! result = test testFunc (items, Slice.create 16)
        Assert.sequentialEqual items result
    }

    [<Fact>]
    let 長さが1024を超えるSliceに追加 () = testing {
        let init = Array.zeroCreate<char> 1024
        let slice = Slice.addSpan (ReadOnlySpan(init)) (Slice.create 1024)
        let! items = 8, ArgGen.array ArgGen.asciiChar (ArgGen.intRange 0 512)
        let! result = test testFunc (items, slice)
        Assert.sequentialEqual (Seq.append init items) result
    }

    [<Fact>]
    let オーナーでないSliceに追加 () = testing {
        let! item0 = 16, ArgGen.asciiChar
        let! item1 = 16, ArgGen.asciiChar
        let! items = 16, ArgGen.array ArgGen.asciiChar (ArgGen.intRange 0 8)
        let slice = Slice.add item0 (Slice.create 4)
        let otherSlice = Slice.add item1 slice
        let! result = test testFunc (items, slice)
        Assert.sequentialEqual [| item0; item1 |] otherSlice
        Assert.sequentialEqual (Seq.append [| item0 |] items) result
    }


module addRange =
    [<Fact>]
    let Sliceを追加 () = testing {
        let! slice1 = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 0 16)
        let! slice2 = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 0 16)
        let! result = test Slice.addRange<char> (slice2, slice1)
        Assert.sequentialEqual (Seq.append slice1 slice2) result
    }

    [<Fact>]
    let 配列を追加 () = testing {
        let! slice = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 0 16)
        let! array = ArgGen.array ArgGen.asciiChar (ArgGen.intRange 0 16)
        let! result = test Slice.addRange<char> (array, slice)
        Assert.sequentialEqual (Seq.append slice array) result
    }

    [<Fact>]
    let ICollectionを追加 () = testing {
        let! slice = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 0 16)
        let! array = ArgGen.array ArgGen.asciiChar (ArgGen.intRange 0 16)
        let collection =
            { new ICollection<char> with
                member _.Count = array.Length
                member _.GetEnumerator() : System.Collections.IEnumerator = array.GetEnumerator()
                member _.GetEnumerator() : IEnumerator<char> = (array :> IEnumerable<char>).GetEnumerator()
                member _.CopyTo(a, index) = array.CopyTo(a, index)
                member _.Add(_) = raise (System.NotImplementedException())
                member _.Clear() = raise (System.NotImplementedException())
                member _.Contains(_) = raise (System.NotImplementedException())
                member _.IsReadOnly = raise (System.NotImplementedException())
                member _.Remove(_) = raise (System.NotImplementedException())
            }
        let! result = test Slice.addRange<char> (collection, slice)
        Assert.sequentialEqual (Seq.append slice array) result
    }

    [<Fact>]
    let IReadOnlyCollectionを追加 () = testing {
        let! slice = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 0 16)
        let! array = ArgGen.array ArgGen.asciiChar (ArgGen.intRange 0 16)
        let collection =
            { new IReadOnlyCollection<char> with
                member _.Count = array.Length
                member _.GetEnumerator() : System.Collections.IEnumerator = array.GetEnumerator()
                member _.GetEnumerator() : IEnumerator<char> = (array :> IEnumerable<char>).GetEnumerator()
            }
        let! result = test Slice.addRange<char> (collection, slice)
        Assert.sequentialEqual (Seq.append slice array) result
    }

    [<Fact>]
    let seqを追加 () = testing {
        let! slice = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 0 16)
        let! array = ArgGen.array ArgGen.asciiChar (ArgGen.intRange 0 16)
        let collection = seq { for item in array -> item }
        let! result = test Slice.addRange<char> (collection, slice)
        Assert.sequentialEqual (Seq.append slice array) result
    }

    [<Fact>]
    let nullを追加 () = testing {
        let! slice = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 0 16)
        let! result = test Slice.addRange<char> (null, slice)
        Assert.equal slice result
    }


module addSlice =
    [<Fact>]
    let 正常 () = testing {
        let! slice1 = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 0 16)
        let! slice2 = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 0 16)
        let expected = Seq.append slice1 slice2
        test Slice.addSlice (slice2, slice1) ==> Assert.sequentialEqual expected
    }

    [<Fact>]
    let Empty () = test Slice.addSlice (Slice.empty, Slice.empty) ==> Assert.equal Slice.empty


module copyWithCap =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 0 16)
        let! capacity = ArgGen.intRange 0 24
        let! result = test Slice.copyWithCap<char> (capacity, slice)
        Assert.sequentialEqual slice result
        Assert.greaterThanOrEqual slice.Capacity result.Capacity
        Assert.greaterThanOrEqual capacity result.Capacity
    }


module copy =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 0 16)
        let! result = test Slice.copy<char> slice
        Assert.sequentialEqual slice result
        Assert.greaterThanOrEqual slice.Capacity result.Capacity
    }


module iter =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice ArgGen.int (ArgGen.intRange 0 16)
        let actualItems = ResizeArray()
        let expectedItems = ResizeArray()
        Seq.iter expectedItems.Add slice
        do! test Slice.iter (actualItems.Add, slice)
        Assert.sequentialEqual expectedItems actualItems
    }


module iteri =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice ArgGen.int (ArgGen.intRange 0 16)
        let actualItems = ResizeArray()
        let expectedItems = ResizeArray()
        Seq.iteri (fun i x -> expectedItems.Add(i, x)) slice
        do! test Slice.iteri ((fun i x -> actualItems.Add(i, x)), slice)
        Assert.sequentialEqual expectedItems actualItems
    }


module iter2 =
    [<Fact>]
    let 正常 () = testing {
        let! slice1 = ArgGen.slice ArgGen.int (ArgGen.intRange 0 16)
        let! slice2 = ArgGen.slice ArgGen.int (ArgGen.intRange 0 16)
        let actualItems = ResizeArray()
        let expectedItems = ResizeArray()
        Seq.iter2 (fun x y -> expectedItems.Add(x, y)) slice1 slice2
        do! test Slice.iter2 ((fun x y -> actualItems.Add(x, y)), slice1, slice2)
        Assert.sequentialEqual expectedItems actualItems
    }


module iteri2 =
    [<Fact>]
    let 正常 () = testing {
        let! slice1 = ArgGen.slice ArgGen.int (ArgGen.intRange 0 16)
        let! slice2 = ArgGen.slice ArgGen.int (ArgGen.intRange 0 16)
        let actualItems = ResizeArray()
        let expectedItems = ResizeArray()
        Seq.iteri2 (fun i x y -> expectedItems.Add(i, x, y)) slice1 slice2
        do! test Slice.iteri2 ((fun i x y -> actualItems.Add(i, x, y)), slice1, slice2)
        Assert.sequentialEqual expectedItems actualItems
    }


module Cons =
    [<Fact>]
    let 一致 () = testing {
        let! slice = ArgGen.slice ArgGen.int (ArgGen.intRange 1 16)
        let actual = ValueSome struct (slice[0], slice[1..])
        test Slice.(|Cons|_|) slice ==> Assert.equal actual
    }

    [<Fact>]
    let 不一致 () = testing {
        let! slice = ArgGen.slice ArgGen.int (ArgGen.constant 0)
        test Slice.(|Cons|_|) slice ==> Assert.vnone
    }


module Cons2 =
    [<Fact>]
    let 一致 () = testing {
        let! slice = ArgGen.slice ArgGen.int (ArgGen.intRange 2 16)
        let actual = ValueSome struct (slice[0], slice[1], slice[2..])
        test Slice.(|Cons2|_|) slice ==> Assert.equal actual
    }

    [<Fact>]
    let 不一致 () = testing {
        let! slice = ArgGen.slice ArgGen.int (ArgGen.intRange 0 2)
        test Slice.(|Cons2|_|) slice ==> Assert.vnone
    }


module Cons3 =
    [<Fact>]
    let 一致 () = testing {
        let! slice = ArgGen.slice ArgGen.int (ArgGen.intRange 3 16)
        let actual = ValueSome struct (slice[0], slice[1], slice[2], slice[3..])
        test Slice.(|Cons3|_|) slice ==> Assert.equal actual
    }

    [<Fact>]
    let 不一致 () = testing {
        let! slice = ArgGen.slice ArgGen.int (ArgGen.intRange 0 3)
        test Slice.(|Cons3|_|) slice ==> Assert.vnone
    }


module ConsBack =
    [<Fact>]
    let 一致 () = testing {
        let! slice = ArgGen.slice ArgGen.int (ArgGen.intRange 1 16)
        let actual = ValueSome struct (slice[.. slice.Length - 1], slice[slice.Length - 1])
        test Slice.(|ConsBack|_|) slice ==> Assert.equal actual
    }

    [<Fact>]
    let 不一致 () = testing {
        let! slice = ArgGen.slice ArgGen.int (ArgGen.constant 0)
        test Slice.(|ConsBack|_|) slice ==> Assert.vnone
    }


module ConsBack2 =
    [<Fact>]
    let 一致 () = testing {
        let! slice = ArgGen.slice ArgGen.int (ArgGen.intRange 2 16)
        let actual =
            ValueSome struct (slice[.. slice.Length - 2], slice[slice.Length - 2], slice[slice.Length - 1])
        test Slice.(|ConsBack2|_|) slice ==> Assert.equal actual
    }

    [<Fact>]
    let 不一致 () = testing {
        let! slice = ArgGen.slice ArgGen.int (ArgGen.intRange 0 2)
        test Slice.(|ConsBack2|_|) slice ==> Assert.vnone
    }


module ConsBack3 =
    [<Fact>]
    let 一致 () = testing {
        let! slice = ArgGen.slice ArgGen.int (ArgGen.intRange 3 16)
        let actual =
            ValueSome
                struct (slice[.. slice.Length - 3],
                        slice[slice.Length - 3],
                        slice[slice.Length - 2],
                        slice[slice.Length - 1])
        test Slice.(|ConsBack3|_|) slice ==> Assert.equal actual
    }

    [<Fact>]
    let 不一致 () = testing {
        let! slice = ArgGen.slice ArgGen.int (ArgGen.intRange 0 3)
        test Slice.(|ConsBack3|_|) slice ==> Assert.vnone
    }
