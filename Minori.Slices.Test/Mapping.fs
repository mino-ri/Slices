namespace Minori.Slices.Test.Slice

open System
open Testexp
open Minori.Slices
open Minori.Slices.Test

module map =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice (ArgGen.intRange -1024 1023) (ArgGen.intRange 0 16)
        test Slice.map ((~-), slice) ==> Assert.sequentialEqual (Seq.map (~-) slice)
    }

    [<Fact>]
    let Empty () = test Slice.map ((~-), Slice.empty) ==> Assert.equal Slice.empty


module mapi =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice (ArgGen.intRange -1024 1023) (ArgGen.intRange 0 16)
        test Slice.mapi ((*), slice) ==> Assert.sequentialEqual (Seq.mapi (*) slice)
    }

    [<Fact>]
    let Empty () = test Slice.mapi ((*), Slice.empty) ==> Assert.equal Slice.empty


module map2 =
    [<Fact>]
    let 正常 () = testing {
        let! slice1 = ArgGen.slice (ArgGen.intRange -1024 1023) (ArgGen.intRange 0 16)
        let! slice2 = ArgGen.slice (ArgGen.intRange -1024 1023) (ArgGen.intRange 0 16)
        let expected = Seq.map2 (*) slice1 slice2
        test Slice.map2 ((*), slice1, slice2) ==> Assert.sequentialEqual expected
    }

    [<Fact>]
    let Empty () = test Slice.map2 ((*), Slice.empty, Slice.empty) ==> Assert.equal Slice.empty


module mapi2 =
    let mapping (i: int) x y = i * x * y
    [<Fact>]
    let 正常 () = testing {
        let! slice1 = ArgGen.slice (ArgGen.intRange -1024 1023) (ArgGen.intRange 0 16)
        let! slice2 = ArgGen.slice (ArgGen.intRange -1024 1023) (ArgGen.intRange 0 16)
        let expected = Seq.mapi2 mapping slice1 slice2
        test Slice.mapi2 (mapping, slice1, slice2) ==> Assert.sequentialEqual expected
    }

    [<Fact>]
    let Empty () = test Slice.mapi2 (mapping, Slice.empty, Slice.empty) ==> Assert.equal Slice.empty


module allPairs =
    [<Fact>]
    let 正常 () = testing {
        let! slice1 = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 0 16)
        let! slice2 = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 0 16)
        let expected = Seq.allPairs slice1 slice2 |> Seq.toArray
        test Slice.allPairs (slice1, slice2) ==> Assert.sequentialEqual expected
    }

    [<Fact>]
    let Empty () = test Slice.allPairs (Slice.empty, Slice.empty) ==> Assert.equal Slice.empty


module vAllPairs =
    [<Fact>]
    let 正常 () = testing {
        let! slice1 = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 0 16)
        let! slice2 = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 0 16)
        let expected = Seq.allPairs slice1 slice2 |> Helpers.toValueTupleSeq |> Seq.toArray
        test Slice.vAllPairs (slice1, slice2) ==> Assert.sequentialEqual expected
    }

    [<Fact>]
    let Empty () = test Slice.vAllPairs (Slice.empty, Slice.empty) ==> Assert.equal Slice.empty


module indexed =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice (ArgGen.intRange -1024 1023) (ArgGen.intRange 0 16)
        let expected = Seq.indexed slice |> Seq.toArray
        test Slice.indexed slice ==> Assert.sequentialEqual expected
    }

    [<Fact>]
    let Empty () = test Slice.indexed Slice.empty ==> Assert.equal Slice.empty


module vIndexed =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice (ArgGen.intRange -1024 1023) (ArgGen.intRange 0 16)
        let expected = Seq.indexed slice |> Helpers.toValueTupleSeq |> Seq.toArray
        test Slice.vIndexed slice ==> Assert.sequentialEqual expected
    }

    [<Fact>]
    let Empty () = test Slice.vIndexed Slice.empty ==> Assert.equal Slice.empty


module zip =
    [<Fact>]
    let 正常 () = testing {
        let! slice1 = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 0 16)
        let! slice2 = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 0 16)
        let expected = Seq.zip slice1 slice2 |> Seq.toArray
        test Slice.zip (slice1, slice2) ==> Assert.sequentialEqual expected
    }

    [<Fact>]
    let Empty () = test Slice.zip (Slice.empty, Slice.empty) ==> Assert.equal Slice.empty


module vZip =
    [<Fact>]
    let 正常 () = testing {
        let! slice1 = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 0 16)
        let! slice2 = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 0 16)
        let expected = Seq.zip slice1 slice2 |> Helpers.toValueTupleSeq |> Seq.toArray
        test Slice.vZip (slice1, slice2) ==> Assert.sequentialEqual expected
    }

    [<Fact>]
    let Empty () = test Slice.vZip (Slice.empty, Slice.empty) ==> Assert.equal Slice.empty


module zip3 =
    [<Fact>]
    let 正常 () = testing {
        let! slice1 = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 0 16)
        let! slice2 = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 0 16)
        let! slice3 = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 0 16)
        let expected = Seq.zip3 slice1 slice2 slice3 |> Seq.toArray
        test Slice.zip3 (slice1, slice2, slice3) ==> Assert.sequentialEqual expected
    }

    [<Fact>]
    let Empty () = test Slice.zip3 (Slice.empty, Slice.empty, Slice.empty) ==> Assert.equal Slice.empty


module vZip3 =
    [<Fact>]
    let 正常 () = testing {
        let! slice1 = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 0 16)
        let! slice2 = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 0 16)
        let! slice3 = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 0 16)
        let expected = Seq.zip3 slice1 slice2 slice3 |> Helpers.toValueTupleSeq3 |> Seq.toArray
        test Slice.vZip3 (slice1, slice2, slice3) ==> Assert.sequentialEqual expected
    }

    [<Fact>]
    let Empty () = test Slice.vZip3 (Slice.empty, Slice.empty, Slice.empty) ==> Assert.equal Slice.empty


module unzip =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice (ArgGen.tuple ArgGen.asciiChar) (ArgGen.intRange 0 16)
        let expected1, expected2 = slice |> List.ofSeq |> List.unzip
        let! actual1, actual2 = test Slice.unzip slice
        Assert.sequentialEqual expected1 actual1
        Assert.sequentialEqual expected2 actual2
    }

    [<Fact>]
    let Empty () = test Slice.unzip Slice.empty ==> Assert.equal (Slice.empty, Slice.empty)


module vUnzip =
    [<Fact>]
    let 正常 () = testing {
        let! slice1 = ArgGen.slice (ArgGen.tuple ArgGen.asciiChar) (ArgGen.intRange 0 16)
        let slice2 = Slice.map Helpers.toValueTuple slice1
        let expected1, expected2 = slice1 |> List.ofSeq |> List.unzip
        let! actual1, actual2 = test Slice.vUnzip slice2
        Assert.sequentialEqual expected1 actual1
        Assert.sequentialEqual expected2 actual2
    }

    [<Fact>]
    let Empty () = test Slice.unzip Slice.empty ==> Assert.equal (Slice.empty, Slice.empty)


module unzip3 =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice (ArgGen.tuple3 ArgGen.asciiChar) (ArgGen.intRange 0 16)
        let expected1, expected2, expected3 = slice |> List.ofSeq |> List.unzip3
        let! actual1, actual2, actual3 = test Slice.unzip3 slice
        Assert.sequentialEqual expected1 actual1
        Assert.sequentialEqual expected2 actual2
        Assert.sequentialEqual expected3 actual3
    }

    [<Fact>]
    let Empty () = test Slice.unzip Slice.empty ==> Assert.equal (Slice.empty, Slice.empty)


module vUnzip3 =
    [<Fact>]
    let 正常 () = testing {
        let! slice1 = ArgGen.slice (ArgGen.tuple3 ArgGen.asciiChar) (ArgGen.intRange 0 16)
        let slice2 = Slice.map Helpers.toValueTuple3 slice1
        let expected1, expected2, expected3 = slice1 |> List.ofSeq |> List.unzip3
        let! actual1, actual2, actual3 = test Slice.vUnzip3 slice2
        Assert.sequentialEqual expected1 actual1
        Assert.sequentialEqual expected2 actual2
        Assert.sequentialEqual expected3 actual3
    }

    [<Fact>]
    let Empty () = test Slice.unzip Slice.empty ==> Assert.equal (Slice.empty, Slice.empty)
