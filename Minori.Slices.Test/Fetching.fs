namespace Minori.Slices.Test.Slice

open System
open Testexp
open Minori.Slices
open Minori.Slices.Test
open System.Collections.Generic


module exactlyOne =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice ArgGen.asciiChar (ArgGen.constant 1)
        test Slice.exactlyOne slice ==> Assert.equal slice[0]
    }

    [<Fact>]
    let 異常_要素が2つ以上 () = testing {
        let! slice = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 2 16)
        test Slice.exactlyOne slice ==> Assert.thrown<ArgumentException>
    }

    [<Fact>]
    let 異常_Empty () = test Slice.exactlyOne Slice.empty ==> Assert.thrown<ArgumentException>


module tryExactlyOne =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice ArgGen.asciiChar (ArgGen.constant 1)
        test Slice.tryExactlyOne slice ==> Assert.equal (Some(slice[0]))
    }

    [<Fact>]
    let 要素が2つ以上 () = testing {
        let! slice = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 2 16)
        test Slice.tryExactlyOne slice ==> Assert.none
    }

    [<Fact>]
    let Empty () = test Slice.tryExactlyOne Slice.empty ==> Assert.none


module vTryExactlyOne =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice ArgGen.asciiChar (ArgGen.constant 1)
        test Slice.vTryExactlyOne slice ==> Assert.equal (ValueSome(slice[0]))
    }

    [<Fact>]
    let 要素が2つ以上 () = testing {
        let! slice = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 2 16)
        test Slice.vTryExactlyOne slice ==> Assert.vnone
    }

    [<Fact>]
    let Empty () = test Slice.vTryExactlyOne Slice.empty ==> Assert.vnone


module find =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 1 16)
        let! index = ArgGen.intRange 0 slice.Length
        test Slice.find ((Mock.trueWhen<char> index), slice) ==> Assert.equal slice[index]
    }

    [<Fact>]
    let 異常 () = testing {
        let! slice = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 1 16)
        test Slice.find ((fun _ -> false), slice) ==> Assert.thrown<KeyNotFoundException>
    }

    [<Fact>]
    let Empty () = test Slice.find ((fun _ -> true), Slice.empty) ==> Assert.thrown<KeyNotFoundException>


module tryFind =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 1 16)
        let! index = ArgGen.intRange 0 slice.Length
        test Slice.tryFind ((Mock.trueWhen<char> index), slice)
        ==> Assert.equal (Some(slice[index]))
    }

    [<Fact>]
    let 存在しない () = testing {
        let! slice = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 1 16)
        test Slice.tryFind ((fun _ -> false), slice) ==> Assert.none
    }

    [<Fact>]
    let Empty () = test Slice.tryFind ((fun _ -> true), Slice.empty) ==> Assert.none


module vTryFind =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 1 16)
        let! index = ArgGen.intRange 0 slice.Length
        test Slice.vTryFind ((Mock.trueWhen<char> index), slice)
        ==> Assert.equal (ValueSome(slice[index]))
    }

    [<Fact>]
    let 存在しない () = testing {
        let! slice = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 1 16)
        test Slice.vTryFind ((fun _ -> false), slice) ==> Assert.vnone
    }

    [<Fact>]
    let Empty () = test Slice.vTryFind ((fun _ -> true), Slice.empty) ==> Assert.vnone


module findBack =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 1 16)
        let! index = ArgGen.intRange 0 slice.Length
        let expected = slice[slice.Length - index - 1]
        test Slice.findBack ((Mock.trueWhen<char> index), slice) ==> Assert.equal expected
    }

    [<Fact>]
    let 異常 () = testing {
        let! slice = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 1 16)
        test Slice.findBack ((fun _ -> false), slice) ==> Assert.thrown<KeyNotFoundException>
    }

    [<Fact>]
    let Empty () = test Slice.findBack ((fun _ -> true), Slice.empty) ==> Assert.thrown<KeyNotFoundException>


module tryFindBack =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 1 16)
        let! index = ArgGen.intRange 0 slice.Length
        let expected = Some(slice[slice.Length - index - 1])
        test Slice.tryFindBack ((Mock.trueWhen<char> index), slice) ==> Assert.equal expected
    }

    [<Fact>]
    let 存在しない () = testing {
        let! slice = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 1 16)
        test Slice.tryFindBack ((fun _ -> false), slice) ==> Assert.none
    }

    [<Fact>]
    let Empty () = test Slice.tryFindBack ((fun _ -> true), Slice.empty) ==> Assert.none


module vTryFindBack =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 1 16)
        let! index = ArgGen.intRange 0 slice.Length
        let expected = ValueSome(slice[slice.Length - index - 1])
        test Slice.vTryFindBack ((Mock.trueWhen<char> index), slice) ==> Assert.equal expected
    }

    [<Fact>]
    let 存在しない () = testing {
        let! slice = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 1 16)
        test Slice.vTryFindBack ((fun _ -> false), slice) ==> Assert.vnone
    }

    [<Fact>]
    let Empty () = test Slice.vTryFindBack ((fun _ -> true), Slice.empty) ==> Assert.vnone


module findIndex =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 1 16)
        let! index = ArgGen.intRange 0 slice.Length
        test Slice.findIndex ((Mock.trueWhen<char> index), slice) ==> Assert.equal index
    }

    [<Fact>]
    let 異常 () = testing {
        let! slice = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 1 16)
        test Slice.findIndex ((fun _ -> false), slice) ==> Assert.thrown<KeyNotFoundException>
    }

    [<Fact>]
    let Empty () =
        test Slice.findIndex ((fun _ -> true), Slice.empty)
        ==> Assert.thrown<KeyNotFoundException>


module tryFindIndex =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 1 16)
        let! index = ArgGen.intRange 0 slice.Length
        test Slice.tryFindIndex ((Mock.trueWhen<char> index), slice)
        ==> Assert.equal (Some(index))
    }

    [<Fact>]
    let 存在しない () = testing {
        let! slice = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 1 16)
        test Slice.tryFindIndex ((fun _ -> false), slice) ==> Assert.none
    }

    [<Fact>]
    let Empty () = test Slice.tryFindIndex ((fun _ -> true), Slice.empty) ==> Assert.none


module vTryFindIndex =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 1 16)
        let! index = ArgGen.intRange 0 slice.Length
        test Slice.vTryFindIndex ((Mock.trueWhen<char> index), slice)
        ==> Assert.equal (ValueSome(index))
    }

    [<Fact>]
    let 存在しない () = testing {
        let! slice = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 1 16)
        test Slice.vTryFindIndex ((fun _ -> false), slice) ==> Assert.vnone
    }

    [<Fact>]
    let Empty () = test Slice.vTryFindIndex ((fun _ -> true), Slice.empty) ==> Assert.vnone


module findIndexBack =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 1 16)
        let! index = ArgGen.intRange 0 slice.Length
        let expected = slice.Length - index - 1
        test Slice.findIndexBack ((Mock.trueWhen<char> index), slice) ==> Assert.equal expected
    }

    [<Fact>]
    let 異常 () = testing {
        let! slice = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 1 16)
        test Slice.findIndexBack ((fun _ -> false), slice) ==> Assert.thrown<KeyNotFoundException>
    }

    [<Fact>]
    let Empty () =
        test Slice.findIndexBack ((fun _ -> true), Slice.empty)
        ==> Assert.thrown<KeyNotFoundException>


module tryFindIndexBack =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 1 16)
        let! index = ArgGen.intRange 0 slice.Length
        let expected = Some(slice.Length - index - 1)
        test Slice.tryFindIndexBack ((Mock.trueWhen<char> index), slice) ==> Assert.equal expected
    }

    [<Fact>]
    let 存在しない () = testing {
        let! slice = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 1 16)
        test Slice.tryFindIndexBack ((fun _ -> false), slice) ==> Assert.none
    }

    [<Fact>]
    let Empty () = test Slice.tryFindIndexBack ((fun _ -> true), Slice.empty) ==> Assert.none


module vTryFindIndexBack =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 1 16)
        let! index = ArgGen.intRange 0 slice.Length
        let expected = ValueSome(slice.Length - index - 1)
        test Slice.vTryFindIndexBack ((Mock.trueWhen<char> index), slice)
        ==> Assert.equal expected
    }

    [<Fact>]
    let 存在しない () = testing {
        let! slice = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 1 16)
        test Slice.vTryFindIndexBack ((fun _ -> false), slice) ==> Assert.vnone
    }

    [<Fact>]
    let Empty () = test Slice.vTryFindIndexBack ((fun _ -> true), Slice.empty) ==> Assert.vnone


module head =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice ArgGen.int (ArgGen.intRange 1 16)
        test Slice.head slice ==> Assert.equal slice[0]
    }

    [<Fact>]
    let Empty () = test Slice.head Slice.empty ==> Assert.thrown<ArgumentException>


module tryHead =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice ArgGen.int (ArgGen.intRange 1 16)
        test Slice.tryHead slice ==> Assert.equal (Some(slice[0]))
    }

    [<Fact>]
    let Empty () = test Slice.tryHead Slice.empty ==> Assert.none


module vTryHead =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice ArgGen.int (ArgGen.intRange 1 16)
        test Slice.vTryHead slice ==> Assert.equal (ValueSome(slice[0]))
    }

    [<Fact>]
    let Empty () = test Slice.vTryHead Slice.empty ==> Assert.vnone


module last =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice ArgGen.int (ArgGen.intRange 1 16)
        test Slice.last slice ==> Assert.equal slice[slice.Length - 1]
    }

    [<Fact>]
    let Empty () = test Slice.last Slice.empty ==> Assert.thrown<ArgumentException>


module tryLast =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice ArgGen.int (ArgGen.intRange 1 16)
        test Slice.tryLast slice ==> Assert.equal (Some(slice[slice.Length - 1]))
    }

    [<Fact>]
    let Empty () = test Slice.tryLast Slice.empty ==> Assert.none


module vTryLast =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice ArgGen.int (ArgGen.intRange 1 16)
        test Slice.vTryLast slice ==> Assert.equal (ValueSome(slice[slice.Length - 1]))
    }

    [<Fact>]
    let Empty () = test Slice.vTryLast Slice.empty ==> Assert.vnone


module item =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice ArgGen.int (ArgGen.intRange 1 16)
        let! index = ArgGen.intRange 0 slice.Length
        test Slice.item (index, slice) ==> Assert.equal slice[index]
    }

    [<Fact>]
    let インデックスが大きすぎる () = testing {
        let! slice = ArgGen.slice ArgGen.int (ArgGen.intRange 1 16)
        let! index = ArgGen.intRange slice.Length 99
        test Slice.item (index, slice) ==> Assert.thrown<ArgumentOutOfRangeException>
    }

    [<Fact>]
    let インデックスが小さすぎる () = testing {
        let! slice = ArgGen.slice ArgGen.int (ArgGen.intRange 1 16)
        let! index = ArgGen.intRange -99 -1
        test Slice.item (index, slice) ==> Assert.thrown<ArgumentOutOfRangeException>
    }

    [<Fact>]
    let Empty () = testing {
        let! index = ArgGen.intRange -99 99
        test Slice.item (index, Slice.empty) ==> Assert.thrown<ArgumentOutOfRangeException>
    }


module tryItem =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice ArgGen.int (ArgGen.intRange 1 16)
        let! index = ArgGen.intRange 0 slice.Length
        test Slice.tryItem (index, slice) ==> Assert.equal (Some(slice[index]))
    }

    [<Fact>]
    let インデックスが大きすぎる () = testing {
        let! slice = ArgGen.slice ArgGen.int (ArgGen.intRange 1 16)
        let! index = ArgGen.intRange slice.Length 99
        test Slice.tryItem (index, slice) ==> Assert.none
    }

    [<Fact>]
    let インデックスが小さすぎる () = testing {
        let! slice = ArgGen.slice ArgGen.int (ArgGen.intRange 1 16)
        let! index = ArgGen.intRange -99 -1
        test Slice.tryItem (index, slice) ==> Assert.none
    }

    [<Fact>]
    let Empty () = testing {
        let! index = ArgGen.intRange -99 99
        test Slice.tryItem (index, Slice.empty) ==> Assert.none
    }


module vTryItem =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice ArgGen.int (ArgGen.intRange 1 16)
        let! index = ArgGen.intRange 0 slice.Length
        test Slice.vTryItem (index, slice) ==> Assert.equal (ValueSome(slice[index]))
    }

    [<Fact>]
    let インデックスが大きすぎる () = testing {
        let! slice = ArgGen.slice ArgGen.int (ArgGen.intRange 1 16)
        let! index = ArgGen.intRange slice.Length 99
        test Slice.vTryItem (index, slice) ==> Assert.vnone
    }

    [<Fact>]
    let インデックスが小さすぎる () = testing {
        let! slice = ArgGen.slice ArgGen.int (ArgGen.intRange 1 16)
        let! index = ArgGen.intRange -99 -1
        test Slice.vTryItem (index, slice) ==> Assert.vnone
    }

    [<Fact>]
    let Empty () = testing {
        let! index = ArgGen.intRange -99 99
        test Slice.vTryItem (index, Slice.empty) ==> Assert.vnone
    }


module pick =
    let chooser (x: int) = if x % 2 = 0 then ValueSome(char x) else ValueNone
    let chooser2 (x: int) = if x % 2 = 0 then Some(char x) else None

    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice (ArgGen.int) (ArgGen.intRange 0 16)
        let slice = slice |> Slice.add 2
        test Slice.pick (chooser, slice) ==> Assert.equal (Seq.pick chooser2 slice)
    }

    [<Fact>]
    let Empty () = test Slice.pick (chooser, Slice.empty) ==> Assert.thrown<KeyNotFoundException>


module tryPick =
    let chooser (x: int) = if x % 2 = 0 then Some(char x) else None

    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice (ArgGen.int) (ArgGen.intRange 0 16)
        let expected = Seq.tryPick chooser slice
        test Slice.tryPick (chooser, slice) ==> Assert.equal expected
    }

    [<Fact>]
    let Empty () = test Slice.tryPick (chooser, Slice.empty) ==> Assert.none


module vTryPick =
    let chooser (x: int) = if x % 2 = 0 then ValueSome(char x) else ValueNone
    let chooser2 (x: int) = if x % 2 = 0 then Some(char x) else None

    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice (ArgGen.int) (ArgGen.intRange 0 16)
        let expected = Seq.tryPick chooser2 slice |> Helpers.toValueOption
        test Slice.vTryPick (chooser, slice) ==> Assert.equal expected
    }

    [<Fact>]
    let Empty () = test Slice.vTryPick (chooser, Slice.empty) ==> Assert.vnone
