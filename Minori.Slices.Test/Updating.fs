namespace Minori.Slices.Test.Slice

open Testexp
open Minori.Slices
open Minori.Slices.Test
open System


module insertAt =
    [<Fact>]
    let 正常 () = testing {
        let! slice = 16, ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 1 16)
        let! index = 16, ArgGen.intRange 0 slice.Length
        let! value = 16, ArgGen.asciiChar
        let expected = Seq.insertAt index value slice
        test Slice.insertAt (index, value, slice) ==> Assert.sequentialEqual expected
    }


module insertSliceAt =
    [<Fact>]
    let 正常 () = testing {
        let! slice = 16, ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 1 16)
        let! index = 16, ArgGen.intRange 0 slice.Length
        let! values = 16, ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 1 16)
        let expected = Seq.insertManyAt index values slice
        test Slice.insertSliceAt (index, values, slice) ==> Assert.sequentialEqual expected
    }


module insertManyAt =
    [<Fact>]
    let 正常 () = testing {
        let! slice = 16, ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 1 16)
        let! index = 16, ArgGen.intRange 0 slice.Length
        let! values = 16, ArgGen.array ArgGen.asciiChar (ArgGen.intRange 1 16)
        let expected = Seq.insertManyAt index values slice
        test Slice.insertManyAt (index, values, slice) ==> Assert.sequentialEqual expected
    }


module removeAt =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 1 16)
        let! index = ArgGen.intRange 0 slice.Length
        let expected = Seq.removeAt index slice
        test Slice.removeAt (index, slice) ==> Assert.sequentialEqual expected
    }


module removeManyAt =
    [<Fact>]
    let 正常 () = testing {
        let! slice = 16, ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 1 16)
        let! index = 16, ArgGen.intRange 0 slice.Length
        let! length = 16, ArgGen.intRange 0 (slice.Length - index)
        let expected = Seq.removeManyAt index length slice
        test Slice.removeManyAt (index, length, slice) ==> Assert.sequentialEqual expected
    }

    let 範囲外でもエラーにならない () = testing {
        let! slice = 16, ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 1 16)
        let! index = 16, ArgGen.intRange 0 16
        let! length = 16, ArgGen.intRange 0 16
        let expectedRemoveLength = min length (slice.Length - index)
        let! result = test Slice.removeManyAt (index, length, slice)
        Assert.equal (slice.Length - expectedRemoveLength) result.Length
    }


module updateAt =
    [<Fact>]
    let 正常 () = testing {
        let! slice = 16, ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 1 16)
        let! index = 16, ArgGen.intRange 0 slice.Length
        let! value = 16, ArgGen.asciiChar
        let expected = Seq.updateAt index value slice
        test Slice.updateAt (index, value, slice) ==> Assert.sequentialEqual expected
    }

    [<Fact>]
    let 異常_インデックスが大きすぎる () = testing {
        let! slice = 16, ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 1 16)
        let! index = 16, ArgGen.intRange slice.Length 99
        let! value = 16, ArgGen.asciiChar
        test Slice.updateAt (index, value, slice) ==> Assert.thrown<ArgumentException>
    }

    [<Fact>]
    let 異常_インデックスが小さすぎる () = testing {
        let! slice = 16, ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 1 16)
        let! index = 16, ArgGen.intRange -99 -1
        let! value = 16, ArgGen.asciiChar
        test Slice.updateAt (index, value, slice) ==> Assert.thrown<ArgumentException>
    }
