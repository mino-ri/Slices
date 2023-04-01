namespace Minori.Slices.Test.Slice

open System
open Testexp
open Minori.Slices
open Minori.Slices.Test


module top =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 1 16)
        test Slice.top ((<), slice) ==> Assert.equal (Seq.max slice)
    }

    [<Fact>]
    let Empty () = test Slice.top ((<), Slice.empty) ==> Assert.thrown<ArgumentException>


module topBy =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 1 16)
        test Slice.topBy (int, (<), slice) ==> Assert.equal (Seq.maxBy int slice)
    }

    [<Fact>]
    let Empty () = test Slice.topBy (int, (<), Slice.empty) ==> Assert.thrown<ArgumentException>


module max =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 1 16)
        test Slice.max slice ==> Assert.equal (Seq.max slice)
    }

    [<Fact>]
    let Empty () = test Slice.max Slice.empty ==> Assert.thrown<ArgumentException>


module maxBy =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 1 16)
        test Slice.maxBy (int, slice) ==> Assert.equal (Seq.maxBy int slice)
    }

    [<Fact>]
    let Empty () = test Slice.maxBy (int, Slice.empty<char>) ==> Assert.thrown<ArgumentException>


module min =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 1 16)
        test Slice.min slice ==> Assert.equal (Seq.min slice)
    }

    [<Fact>]
    let Empty () = test Slice.min Slice.empty ==> Assert.thrown<ArgumentException>


module minBy =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 1 16)
        test Slice.minBy (int, slice) ==> Assert.equal (Seq.minBy int slice)
    }

    [<Fact>]
    let Empty () = test Slice.minBy (int, Slice.empty<char>) ==> Assert.thrown<ArgumentException>


module sum =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice (ArgGen.floatRange -1024.0 1024.0) (ArgGen.intRange 1 16)
        test Slice.sum slice ==> Assert.equal (Seq.sum slice)
    }

    [<Fact>]
    let Empty () = test Slice.sum Slice.empty<float> ==> Assert.equal 0.0


module sumBy =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice (ArgGen.intRange -1024 1023) (ArgGen.intRange 1 16)
        test Slice.sumBy (float, slice) ==> Assert.equal (Seq.sumBy float slice)
    }

    [<Fact>]
    let Empty () = test Slice.sumBy (float, Slice.empty<int>) ==> Assert.equal 0.0


module average =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice (ArgGen.floatRange -1024.0 1024.0) (ArgGen.intRange 1 16)
        test Slice.average slice ==> Assert.equal (Seq.average slice)
    }

    [<Fact>]
    let Empty () = test Slice.average Slice.empty<float> ==> Assert.thrown<ArgumentException>


module averageBy =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice (ArgGen.intRange -1024 1023) (ArgGen.intRange 1 16)
        test Slice.averageBy (float, slice) ==> Assert.equal (Seq.averageBy float slice)
    }

    [<Fact>]
    let Empty () = test Slice.averageBy (float, Slice.empty<int>) ==> Assert.thrown<ArgumentException>
