namespace Minori.Slices.Test.Slice

open Testexp
open Minori.Slices
open Minori.Slices.Test


module contains =
    [<Fact>]
    let 正常 () = testing {
        let! value = ArgGen.asciiChar
        let! slice = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 0 16)
        let expected = Seq.contains value slice
        test (Slice.contains: char -> _) (value, slice) ==> Assert.equal expected
    }

    [<Fact>]
    let Empty () = testing {
        let! value = ArgGen.asciiChar
        test (Slice.contains: char -> _) (value, Slice.empty) ==> Assert.isFalse
    }


module exists =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 0 16)
        let! index = ArgGen.intRange 0 18
        let expected = Seq.exists (Mock.trueWhen<char> index) slice
        test Slice.exists ((Mock.trueWhen<char> index), slice) ==> Assert.equal expected
    }

    [<Fact>]
    let Empty () = test Slice.exists ((fun _ -> true), Slice.empty) ==> Assert.isFalse


module exists2 =
    [<Fact>]
    let 正常 () = testing {
        let! slice1 = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 0 16)
        let! slice2 = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 0 16)
        let expected = Seq.exists2 (=) slice1 slice2
        test Slice.exists2 ((=), slice1, slice2) ==> Assert.equal expected
    }

    [<Fact>]
    let Empty () = test Slice.exists2 ((=), Slice.empty, Slice.empty) ==> Assert.isFalse


module forall =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 0 16)
        let! index = ArgGen.intRange 0 18
        let expected = Seq.forall (Mock.falseWhen<char> index) slice
        test Slice.forall ((Mock.falseWhen<char> index), slice) ==> Assert.equal expected
    }

    [<Fact>]
    let Empty () = test Slice.forall ((fun _ -> true), Slice.empty) ==> Assert.isTrue


module forall2 =
    [<Fact>]
    let 正常 () = testing {
        let! slice1 = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 0 16)
        let! slice2 = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 0 16)
        let expected = Seq.forall2 (=) slice1 slice2
        test Slice.forall2 ((=), slice1, slice2) ==> Assert.equal expected
    }

    [<Fact>]
    let Empty () = test Slice.forall2 ((=), Slice.empty, Slice.empty) ==> Assert.isTrue


module compareWith =
    let compare (x: char) y = x.CompareTo(y)

    [<Fact>]
    let 正常 () = testing {
        let! slice1 = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 0 16)
        let! slice2 = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 0 16)
        let expected = Seq.compareWith compare slice1 slice2
        let! result = test Slice.compareWith (compare, slice1, slice2)
        Assert.equal (sign expected) (sign result)
    }

    [<Fact>]
    let Empty () = test Slice.compareWith (compare, Slice.empty, Slice.empty) ==> Assert.equal 0
