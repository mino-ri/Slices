namespace Minori.Slices.Test.Slice

open System
open Testexp
open Minori.Slices
open Minori.Slices.Test


module init =
    [<Fact>]
    let 正常 () = testing {
        let! length = ArgGen.intRange 0 16
        let expected = List.init length char
        test Slice.init (length, char) ==> Assert.sequentialEqual expected
    }


module create =
    [<Fact>]
    let 正常 () = testing {
        let! capacity = ArgGen.intRange 0 16
        let! result = test Slice.create capacity
        Assert.equal 0 result.Length
        Assert.equal capacity result.Capacity
    }


module singleton =
    [<Fact>]
    let 正常 () = testing {
        let! item = ArgGen.asciiChar
        let! result = test Slice.singleton item
        Assert.equal 1 result.Length
        Assert.equal item result[0]
    }


module replicate =
    [<Fact>]
    let 正常 () = testing {
        let! count = ArgGen.intRange 0 16
        let! c = ArgGen.asciiChar
        let expected = Array.replicate count c
        test Slice.replicate (count, c) ==> Assert.sequentialEqual expected
    }

    [<Fact>]
    let 異常_countが負 () = testing {
        let! count = ArgGen.intRange -16 -1
        let! c = ArgGen.asciiChar
        test Slice.replicate (count, c) ==> Assert.thrown<ArgumentException>
    }


module unfold =
    let generator state = if state > 5 then None else Some(char state, state + 1)

    [<Fact>]
    let 正常 () =
        let expected = List.unfold generator 0
        test Slice.unfold (generator, 0) ==> Assert.sequentialEqual expected


module vUnfold =
    let generator1 state = if state > 5 then None else Some(char state, state + 1)
    let generator2 state =
        if state > 5 then
            ValueNone
        else
            ValueSome struct (char state, state + 1)

    [<Fact>]
    let 正常 () =
        let expected = List.unfold generator1 0
        test Slice.vUnfold (generator2, 0) ==> Assert.sequentialEqual expected
