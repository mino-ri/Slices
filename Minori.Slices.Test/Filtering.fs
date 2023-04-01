namespace Minori.Slices.Test.Slice

open System
open Testexp
open Minori.Slices
open Minori.Slices.Test


module filter =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice (ArgGen.asciiChar) (ArgGen.intRange 0 16)
        let expected = Seq.filter Char.IsAsciiDigit slice
        test Slice.filter (Char.IsAsciiDigit, slice) ==> Assert.sequentialEqual expected
    }

    [<Fact>]
    let Empty () = test Slice.filter (Char.IsAsciiDigit, Slice.empty) ==> Assert.equal Slice.empty


module choose =
    let chooser (x: int) = if x % 2 = 0 then Some(x) else None

    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice ArgGen.int (ArgGen.intRange 0 16)
        let expected = Seq.choose chooser slice
        test Slice.choose (chooser, slice) ==> Assert.sequentialEqual expected
    }

    [<Fact>]
    let Empty () = test Slice.choose (chooser, Slice.empty) ==> Assert.equal Slice.empty


module vChoose =
    let chooser (x: int) = if x % 2 = 0 then ValueSome(x) else ValueNone
    let chooser2 (x: int) = if x % 2 = 0 then Some(x) else None

    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice ArgGen.int (ArgGen.intRange 0 16)
        let expected = Seq.choose chooser2 slice
        test Slice.vChoose (chooser, slice) ==> Assert.sequentialEqual expected
    }

    [<Fact>]
    let Empty () = test Slice.vChoose (chooser, Slice.empty) ==> Assert.equal Slice.empty


module skip =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 0 16)
        let! count = ArgGen.intRange 0 (slice.Length + 1)
        let expected = Seq.skip count slice
        test Slice.skip (count, slice) ==> Assert.sequentialEqual expected
    }

    [<Fact>]
    let Empty () = test Slice.skip (0, Slice.empty) ==> Assert.equal Slice.empty


module skipWhile =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice (ArgGen.asciiChar) (ArgGen.intRange 0 16)
        let expected = Seq.skipWhile Char.IsAsciiDigit slice
        test Slice.skipWhile (Char.IsAsciiDigit, slice) ==> Assert.sequentialEqual expected
    }

    [<Fact>]
    let Empty () = test Slice.skipWhile (Char.IsAsciiDigit, Slice.empty) ==> Assert.equal Slice.empty


module take =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 0 16)
        let! count = ArgGen.intRange 0 (slice.Length + 1)
        let expected = Seq.take count slice
        test Slice.take (count, slice) ==> Assert.sequentialEqual expected
    }

    [<Fact>]
    let Empty () = test Slice.take (0, Slice.empty) ==> Assert.equal Slice.empty


module takeWhile =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice (ArgGen.asciiChar) (ArgGen.intRange 0 16)
        let expected = Seq.takeWhile Char.IsAsciiDigit slice
        test Slice.takeWhile (Char.IsAsciiDigit, slice) ==> Assert.sequentialEqual expected
    }

    [<Fact>]
    let Empty () = test Slice.takeWhile (Char.IsAsciiDigit, Slice.empty) ==> Assert.equal Slice.empty


module partition =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice ArgGen.int (ArgGen.intRange 0 16)
        let expected1, expected2 = Array.partition ((<) 0) (Array.ofSeq slice)
        let! actual1, actual2 = test Slice.partition (((<) 0), slice)
        Assert.sequentialEqual expected1 actual1
        Assert.sequentialEqual expected2 actual2
    }

    let Empty () = test Slice.partition (((<) 0), Slice.empty) ==> Assert.equal (Slice.empty, Slice.empty)


module vPartition =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice ArgGen.int (ArgGen.intRange 0 16)
        let expected1, expected2 = Array.partition ((<) 0) (Array.ofSeq slice)
        let! struct (actual1, actual2) = test Slice.vPartition (((<) 0), slice)
        Assert.sequentialEqual expected1 actual1
        Assert.sequentialEqual expected2 actual2
    }

    let Empty () =
        test Slice.vPartition (((<) 0), Slice.empty)
        ==> Assert.equal (struct (Slice.empty, Slice.empty))


module splitAt =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 0 16)
        let! count = ArgGen.intRange 0 (slice.Length + 1)
        let expected1, expected2 = Array.splitAt count (Slice.toArray slice)
        let! actual1, actual2 = test Slice.splitAt (count, slice)
        Assert.sequentialEqual expected1 actual1
        Assert.sequentialEqual expected2 actual2
    }

    [<Fact>]
    let Empty () = test Slice.splitAt (0, Slice.empty) ==> Assert.equal (Slice.empty, Slice.empty)


module vSplitAt =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 0 16)
        let! count = ArgGen.intRange 0 (slice.Length + 1)
        let expected1, expected2 = Array.splitAt count (Slice.toArray slice)
        let! actual1, actual2 = test Slice.vSplitAt (count, slice)
        Assert.sequentialEqual expected1 actual1
        Assert.sequentialEqual expected2 actual2
    }

    [<Fact>]
    let Empty () = test Slice.vSplitAt (0, Slice.empty) ==> Assert.equal (Slice.empty, Slice.empty)


module tail =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 1 16)
        let expected = Seq.tail slice
        test Slice.tail slice ==> Assert.sequentialEqual expected
    }

    [<Fact>]
    let Empty () = test Slice.tail Slice.empty ==> Assert.thrown<ArgumentException>
