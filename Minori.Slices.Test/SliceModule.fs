namespace Minori.Slices.Test.Slice

open System
open Testexp
open Minori.Slices
open Minori.Slices.Test
open System.Collections.Generic

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
        test Slice.addSlice (slice2, slice1) ==> Assert.sequentialEqual (Seq.append slice1 slice2)
    }

    [<Fact>]
    let Empty () = test Slice.addSlice (Slice.empty, Slice.empty) ==> Assert.equal Slice.empty


module ofArray =
    [<Fact>]
    let 正常 () = testing {
        let! array = ArgGen.array ArgGen.asciiChar (ArgGen.intRange 0 16)
        test Slice.ofArray<char> array ==> Assert.sequentialEqual array
    }


module ofSpan =
    let testFunc (array: char[]) = Slice.ofSpan (ReadOnlySpan(array))

    [<Fact>]
    let 正常 () = testing {
        let! array = ArgGen.array ArgGen.asciiChar (ArgGen.intRange 0 16)
        test testFunc array ==> Assert.sequentialEqual array
    }


module ofSeq =
    [<Fact>]
    let 正常 () = testing {
        let! array = ArgGen.array ArgGen.asciiChar (ArgGen.intRange 0 16)
        let seq = seq { for item in array -> item }
        test Slice.ofSeq<char> seq ==> Assert.sequentialEqual array
    }


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
