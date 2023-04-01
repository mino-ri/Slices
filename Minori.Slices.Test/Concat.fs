namespace Minori.Slices.Test.Slice

open Testexp
open Minori.Slices
open Minori.Slices.Test


module append =
    [<Fact>]
    let 正常 () = testing {
        let! slice1 = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 0 16)
        let! slice2 = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 0 16)
        let expected = Seq.append slice1 slice2
        test Slice.append (slice1, slice2) ==> Assert.sequentialEqual expected
    }

    [<Fact>]
    let Empty () = test Slice.append (Slice.empty, Slice.empty) ==> Assert.equal Slice.empty


module collect =
    let mapping x = Slice.create 2 |> Slice.add x |> Slice.add x
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice ArgGen.int (ArgGen.intRange 0 16)
        let expected = Seq.collect mapping slice
        test Slice.collect (mapping, slice) ==> Assert.sequentialEqual expected
    }

    [<Fact>]
    let Empty () = test Slice.collect (mapping, Slice.empty) ==> Assert.equal Slice.empty


module concat =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice (ArgGen.slice ArgGen.int (ArgGen.intRange 0 5)) (ArgGen.intRange 0 5)
        let expected = Seq.concat slice
        test Slice.concat slice ==> Assert.sequentialEqual expected
    }

    [<Fact>]
    let Empty () = test Slice.concat Slice.empty ==> Assert.equal Slice.empty

    [<Fact>]
    let Null () = test Slice.concat null ==> Assert.equal Slice.empty
