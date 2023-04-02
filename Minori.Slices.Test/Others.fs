namespace Minori.Slices.Test.Slice

open System
open Testexp
open Minori.Slices
open Minori.Slices.Test


module countBy =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 0 16)
        let expected = slice |> Seq.countBy id |> Seq.toArray
        test Slice.countBy (id, slice) ==> Assert.sequentialEqual expected
    }

    [<Fact>]
    let Empty () = test Slice.countBy (id, Slice.empty) ==> Assert.equal Slice.empty


module vCountBy =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 0 16)
        let expected = slice |> Seq.countBy id |> Helpers.toValueTupleSeq |> Seq.toArray
        test Slice.vCountBy (id, slice) ==> Assert.sequentialEqual expected
    }

    [<Fact>]
    let Empty () = test Slice.vCountBy (id, Slice.empty) ==> Assert.equal Slice.empty


module groupBy =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 0 16)
        let expected = slice |> Seq.groupBy id |> Seq.map (fun (x, y) -> x, Slice.ofSeq y) |> Seq.toArray
        test Slice.groupBy (id, slice) ==> Assert.sequentialEqual expected
    }

    [<Fact>]
    let Empty () = test Slice.groupBy (id, Slice.empty) ==> Assert.equal Slice.empty


module vGroupBy =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 0 16)
        let expected =
            slice
            |> Seq.groupBy id
            |> Seq.map (fun (x, y) -> struct (x, Slice.ofSeq y))
            |> Seq.toArray
        test Slice.vGroupBy (id, slice) ==> Assert.sequentialEqual expected
    }

    [<Fact>]
    let Empty () = test Slice.vGroupBy (id, Slice.empty) ==> Assert.equal Slice.empty


module distinct =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 0 16)
        test Slice.distinct slice ==> Assert.sequentialEqual (Seq.distinct slice)
    }

    [<Fact>]
    let Empty () = test Slice.distinct Slice.empty ==> Assert.equal Slice.empty


module distinctBy =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 0 16)
        let expected = Seq.distinctBy int slice
        test Slice.distinctBy (int, slice) ==> Assert.sequentialEqual expected
    }

    [<Fact>]
    let Empty () = test Slice.distinctBy (int, Slice.empty<char>) ==> Assert.equal Slice.empty


module except =
    [<Fact>]
    let 正常 () = testing {
        let! slice1 = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 0 16)
        let! slice2 = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 0 8)
        let expected = Seq.except slice2 slice1
        test Slice.except (slice2, slice1) ==> Assert.sequentialEqual expected
    }

    [<Fact>]
    let Empty () = test Slice.except (Slice.empty, Slice.empty) ==> Assert.sequentialEqual Slice.empty


module pairwise =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 2 16)
        let expected = Seq.pairwise slice
        test Slice.pairwise slice ==> Assert.sequentialEqual expected
    }

    [<Fact>]
    let Empty () = test Slice.pairwise Slice.empty ==> Assert.thrown<ArgumentException>


module vPairwise =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 2 16)
        let expected = Seq.pairwise slice |> Helpers.toValueTupleSeq
        test Slice.vPairwise slice ==> Assert.sequentialEqual expected
    }

    [<Fact>]
    let Empty () = test Slice.vPairwise Slice.empty ==> Assert.thrown<ArgumentException>


module chunkBySize =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice ArgGen.int (ArgGen.intRange 0 16)
        let! chunkSize = ArgGen.intRange 1 16
        let expected = Seq.chunkBySize chunkSize slice |> Seq.map Slice.ofArray
        test Slice.chunkBySize (chunkSize, slice) ==> Assert.sequentialEqual expected
    }

    [<Fact>]
    let 異常_chunkSizeが0以下 () = testing {
        let! slice = ArgGen.slice ArgGen.int (ArgGen.intRange 0 16)
        let! chunkSize = ArgGen.intRange -16 0
        test Slice.chunkBySize (chunkSize, slice) ==> Assert.thrown<ArgumentException>
    }

    [<Fact>]
    let Empty () = testing {
        let! chunkSize = ArgGen.intRange 1 16
        test Slice.chunkBySize (chunkSize, Slice.empty) ==> Assert.equal Slice.empty
    }


module transpose =
    [<Fact>]
    let 正常 () = testing {
        let! length = ArgGen.intRange 0 16
        let! slice = ArgGen.slice (ArgGen.slice ArgGen.asciiChar (ArgGen.constant length)) (ArgGen.intRange 0 16)
        let expected = Seq.transpose slice |> Helpers.toSliceSlice
        test Slice.transpose slice ==> Assert.sequentialEqual expected
    }

    [<Fact>]
    let Empty () = test Slice.transpose Slice.empty ==> Assert.equal Slice.empty

    [<Fact>]
    let 異常_長さが一定でない () = testing {
        let! length1, length2 = ArgGen.different (ArgGen.intRange 0 16)
        let! slice1 = ArgGen.slice ArgGen.asciiChar (ArgGen.constant length1)
        let! slice2 = ArgGen.slice ArgGen.asciiChar (ArgGen.constant length2)
        let slice = Slice.ofArray [| slice1; slice2 |]
        test Slice.transpose slice ==> Assert.thrown<ArgumentException>
    }


module splitInto =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 1 16)
        let! count = ArgGen.intRange 1 16
        let expected = Seq.splitInto count slice |> Helpers.toSliceSlice
        test Slice.splitInto (count, slice) ==> Assert.sequentialEqual expected
    }

    [<Fact>]
    let Empty () = testing {
        let! count = ArgGen.intRange 1 16
        test Slice.splitInto (count, Slice.empty) ==> Assert.equal Slice.empty
    }

    [<Fact>]
    let 異常_サイズが小さすぎる () = testing {
        let! slice = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 1 16)
        let! count = ArgGen.intRange -16 0
        test Slice.splitInto (count, slice) ==> Assert.thrown<ArgumentException>
    }


module windowed =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 1 16)
        let! windowSize = ArgGen.intRange 1 16
        let expected = Seq.windowed windowSize slice |> Helpers.toSliceSlice
        test Slice.windowed (windowSize, slice) ==> Assert.sequentialEqual expected
    }

    [<Fact>]
    let Empty () = testing {
        let! windowSize = ArgGen.intRange 1 16
        test Slice.windowed (windowSize, Slice.empty) ==> Assert.equal Slice.empty
    }

    [<Fact>]
    let 異常_サイズが小さすぎる () = testing {
        let! slice = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 1 16)
        let! windowSize = ArgGen.intRange -16 0
        test Slice.windowed (windowSize, slice) ==> Assert.thrown<ArgumentException>
    }
