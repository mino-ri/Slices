namespace Minori.Slices.Test.Slice

open System
open Testexp
open Minori.Slices
open Minori.Slices.Test
open System.Collections.Generic


module Capacity =
    let testFunc (slice: 'T slice) = slice.Capacity

    [<Fact>]
    let Emptyは0 () = test testFunc Slice.empty ==> Assert.equal 0

    [<Fact>]
    let createWithCap () = testing {
        let! capacity = ArgGen.intRange -8 8
        let expected = max 1 capacity
        test testFunc (Slice.create capacity) ==> Assert.equal expected
    }


module AsSpan =
    let testFunc (slice: 'T slice) = slice.AsSpan().ToArray()

    [<Fact>]
    let Empty () = test testFunc Slice.empty ==> Assert.sequentialEqual Slice.empty

    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 0 16)
        test testFunc slice ==> Assert.sequentialEqual slice
    }


module AsMemory =
    let testFunc (slice: 'T slice) = slice.AsMemory().ToArray()

    [<Fact>]
    let Empty () = test testFunc Slice.empty ==> Assert.sequentialEqual Slice.empty

    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 0 16)
        test testFunc slice ==> Assert.sequentialEqual slice
    }


module Slice1 =
    let testFunc (slice: 'T slice) length = slice.Slice(length)

    [<Fact>]
    let Empty () = test testFunc (Slice.empty, 0) ==> Assert.equal Slice.empty

    [<Fact>]
    let 終了インデックス省略 () = testing {
        let! slice = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 0 16)
        let! startIndex = ArgGen.intRange -16 16
        let expectedStart = System.Int32.Clamp(startIndex, 0, slice.Length)
        let! result = test testFunc (slice, startIndex)
        Assert.sequentialEqual (Seq.skip expectedStart slice) result
    }


module Slice2 =
    let testFunc (slice: 'T slice) startIndex length = slice.Slice(startIndex, length)

    [<Fact>]
    let Empty () = test testFunc (Slice.empty, 0, 0) ==> Assert.equal Slice.empty

    [<Fact>]
    let 両インデックス指定 () = testing {
        let! slice = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 0 16)
        let! startIndex = ArgGen.intRange -16 16
        let! endIndex = ArgGen.intRange -16 16
        let expectedStart = System.Int32.Clamp(startIndex, 0, slice.Length)
        let expectedEnd = System.Int32.Clamp(endIndex, expectedStart, slice.Length)
        let! result = test testFunc (slice, startIndex, endIndex)
        Assert.sequentialEqual (slice |> Seq.skip expectedStart |> Seq.take (expectedEnd - expectedStart)) result
    }


module GetSlice =
    // compilation check
    let _ = Slice.empty[0..]

    let testFunc (slice: 'T slice) startIndex endIndex = slice.GetSlice(startIndex, endIndex)

    [<Fact>]
    let Empty () = test testFunc (Slice.empty, None, None) ==> Assert.equal Slice.empty

    [<Fact>]
    let 開始インデックス省略 () = testing {
        let! slice = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 0 16)
        let! endIndex = ArgGen.intRange -16 16
        let expectedEnd = System.Int32.Clamp(endIndex, 0, slice.Length)
        let! result = test testFunc (slice, None, Some(endIndex))
        Assert.sequentialEqual (Seq.take expectedEnd slice) result
    }

    [<Fact>]
    let 終了インデックス省略 () = testing {
        let! slice = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 0 16)
        let! startIndex = ArgGen.intRange -16 16
        let expectedStart = System.Int32.Clamp(startIndex, 0, slice.Length)
        let! result = test testFunc (slice, Some(startIndex), None)
        Assert.sequentialEqual (Seq.skip expectedStart slice) result
    }

    [<Fact>]
    let 両インデックス指定 () = testing {
        let! slice = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 0 16)
        let! startIndex = ArgGen.intRange -16 16
        let! endIndex = ArgGen.intRange -16 16
        let expectedStart = System.Int32.Clamp(startIndex, 0, slice.Length)
        let expectedEnd = System.Int32.Clamp(endIndex, expectedStart, slice.Length)
        let! result = test testFunc (slice, Some(startIndex), Some(endIndex))
        Assert.sequentialEqual (slice |> Seq.skip expectedStart |> Seq.take (expectedEnd - expectedStart)) result
    }


module GetEnumerator =
    let testFunc (slice: 'T slice) =
        let result = ResizeArray(slice.Length)
        for item in slice do
            result.Add(item)
        result

    let testFunc2 (slice: 'T slice) =
        let result = ResizeArray(slice.Length)
        for item in slice :> IEnumerable<'T> do
            result.Add(item)
        result

    let testFunc3 (slice: 'T slice) =
        let result = ResizeArray(slice.Length)
        for item in slice :> System.Collections.IEnumerable do
            result.Add(item :?> 'T)
        result

    [<Fact>]
    let Empty () = test testFunc Slice.empty ==> Assert.sequentialEqual Slice.empty

    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 0 16)
        test testFunc slice ==> Assert.sequentialEqual slice
    }

    [<Fact>]
    let 正常_IEnumeableT () = testing {
        let! slice = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 0 16)
        test testFunc2 slice ==> Assert.sequentialEqual slice
    }

    [<Fact>]
    let 正常_IEnumeable () = testing {
        let! slice = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 0 16)
        test testFunc3 slice ==> Assert.sequentialEqual slice
    }


module Item =
    let testFunc (slice: 'T slice) index = slice[index]
    let testFunc2 (slice: 'T slice) index = (slice :> IReadOnlyList<'T>)[index]

    [<Fact>]
    let 正常 () = testing {
        let! array = ArgGen.array ArgGen.asciiChar (ArgGen.intRange 1 16)
        let slice = Slice.ofArray array
        let! index = ArgGen.intRange 0 slice.Length
        test testFunc (slice, index) ==> Assert.equal array[index]
    }

    [<Fact>]
    let 異常_Empty () = testing {
        let! index = ArgGen.intRange 0 16
        test testFunc (Slice.empty, index) ==> Assert.thrown<ArgumentOutOfRangeException>
    }

    [<Fact>]
    let 異常_インデックスが範囲外 () = testing {
        let! array = ArgGen.array ArgGen.asciiChar (ArgGen.intRange 1 16)
        let slice = Slice.ofArray array
        let! index = ArgGen.intRange 17 99
        test testFunc (slice, index) ==> Assert.thrown<ArgumentOutOfRangeException>
    }

    [<Fact>]
    let 異常_インデックスが負 () = testing {
        let! array = ArgGen.array ArgGen.asciiChar (ArgGen.intRange 1 16)
        let slice = Slice.ofArray array
        let! index = ArgGen.intRange -99 -1
        test testFunc (slice, index) ==> Assert.thrown<ArgumentOutOfRangeException>
    }

    [<Fact>]
    let 正常_インターフェイス実装 () = testing {
        let! array = ArgGen.array ArgGen.asciiChar (ArgGen.intRange 1 16)
        let slice = Slice.ofArray array
        let! index = ArgGen.intRange 0 slice.Length
        test testFunc2 (slice, index) ==> Assert.equal array[index]
    }


module Count =
    let testFunc (slice: 'T slice) = (slice :> IReadOnlyList<'T>).Count

    [<Fact>]
    let Empty () = test testFunc Slice.empty ==> Assert.equal 0

    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 0 16)
        test testFunc slice ==> Assert.equal slice.Length
    }


module Equals =
    let testFunc1 (slice1: char slice) (slice2: char slice) = slice1.Equals(slice2)
    let testFunc2 (slice1: char slice) (other: obj) = slice1.Equals(other)

    [<Fact>]
    let Emptyとゼロスライスは等しい () =
        test testFunc1 (Slice.empty, Slice.create 1) ==> Assert.isTrue
        test testFunc1 (Slice.create 1, Slice.empty) ==> Assert.isTrue

    [<Fact>]
    let スライスの有無に関わらず等しい () =
        test testFunc1 (Slice.ofArray [| 'a' |], (Slice.ofArray [| 'b'; 'a'; 'c' |]).[1..2])
        ==> Assert.isTrue

    [<Fact>]
    let 要素の数が違うと等しくない () = testing {
        let! slice1 = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 0 8)
        let! slice2 = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 8 16)
        test testFunc1 (slice1, slice2) ==> Assert.isFalse
    }

    [<Fact>]
    let 要素の中身が違うと等しくない1 () = test testFunc1 (Slice.ofArray [| 'a' |], Slice.ofArray [| 'b' |]) ==> Assert.isFalse

    [<Fact>]
    let 要素の中身が違うと等しくない2 () = test testFunc1 (Slice.ofArray [| 'a'; 'b' |], Slice.ofArray [| 'a'; 'c' |]) ==> Assert.isFalse

    [<Fact>]
    let 別の型と等しくない () = test testFunc2 (Slice.ofSeq "abc", "abc") ==> Assert.isFalse
