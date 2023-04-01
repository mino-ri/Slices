namespace Minori.Slices.Test.Slice

open System
open Testexp
open Minori.Slices
open Minori.Slices.Test


module sort =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 0 16)
        test Slice.sort slice ==> Assert.sequentialEqual (Seq.sort slice)
    }


module sortBy =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 0 16)
        test Slice.sortBy (int, slice) ==> Assert.sequentialEqual (Seq.sortBy int slice)
    }


module sortDescending =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 0 16)
        let expected = Seq.sortDescending slice
        test Slice.sortDescending slice ==> Assert.sequentialEqual expected
    }


module sortByDescending =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 0 16)
        let expected = Seq.sortByDescending int slice
        test Slice.sortByDescending (int, slice) ==> Assert.sequentialEqual expected
    }


module sortWith =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 0 16)
        let expected = Seq.sortWith compare slice
        test Slice.sortWith (compare, slice) ==> Assert.sequentialEqual expected
    }


module rev =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 0 16)
        test Slice.rev slice ==> Assert.sequentialEqual (Seq.rev slice)
    }


module permute =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice ArgGen.int (ArgGen.intRange 1 16)
        let expected = Array.permute (fun x -> (x + 1) % slice.Length) (Array.ofSeq slice)
        test Slice.permute ((fun x -> (x + 1) % slice.Length), slice)
        ==> Assert.sequentialEqual expected
    }

    let Empty () = test Slice.permute (id, Slice.empty) ==> Assert.equal Slice.empty
