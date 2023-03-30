namespace Minori.Slices.Test.Slice

open System
open Testexp
open Minori.Slices
open Minori.Slices.Test
open System.Collections.Generic

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


module filter =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice (ArgGen.asciiChar) (ArgGen.intRange 0 16)
        let expected = Seq.filter Char.IsAsciiDigit slice
        test Slice.filter (Char.IsAsciiDigit, slice) ==> Assert.sequentialEqual expected
    }

    [<Fact>]
    let Empty () = test Slice.filter (Char.IsAsciiDigit, Slice.empty) ==> Assert.equal Slice.empty


module allPairs =
    [<Fact>]
    let 正常 () = testing {
        let! slice1 = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 0 16)
        let! slice2 = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 0 16)
        let expected = Seq.allPairs slice1 slice2 |> Helpers.toValueTupleSeq |> Seq.toArray
        test Slice.vAllPairs (slice1, slice2) ==> Assert.sequentialEqual expected
    }

    [<Fact>]
    let Empty () = test Slice.vAllPairs (Slice.empty, Slice.empty) ==> Assert.equal Slice.empty


module append =
    [<Fact>]
    let 正常 () = testing {
        let! slice1 = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 0 16)
        let! slice2 = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 0 16)
        test Slice.append (slice1, slice2) ==> Assert.sequentialEqual (Seq.append slice1 slice2)
    }

    [<Fact>]
    let Empty () = test Slice.append (Slice.empty, Slice.empty) ==> Assert.equal Slice.empty


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


module choose =
    let chooser (x: int) = if x % 2 = 0 then ValueSome(x) else ValueNone
    let chooser2 (x: int) = if x % 2 = 0 then Some(x) else None

    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice (ArgGen.int) (ArgGen.intRange 0 16)
        test Slice.vChoose (chooser, slice) ==> Assert.sequentialEqual (Seq.choose chooser2 slice)
    }

    [<Fact>]
    let Empty () = test Slice.vChoose (chooser, Slice.empty) ==> Assert.equal Slice.empty


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


module countBy =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 0 16)
        let expected = slice |> Seq.countBy id |> Helpers.toValueTupleSeq |> Seq.toArray
        test Slice.vCountBy (id, slice) ==> Assert.sequentialEqual expected
    }

    [<Fact>]
    let Empty () = test Slice.vCountBy (id, Slice.empty) ==> Assert.equal Slice.empty


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
        test Slice.distinctBy (int, slice) ==> Assert.sequentialEqual (Seq.distinctBy int slice)
    }

    [<Fact>]
    let Empty () = test Slice.distinctBy (int, Slice.empty<char>) ==> Assert.equal Slice.empty


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
        test Slice.vTryExactlyOne slice ==> Assert.equal (ValueSome(slice[0]))
    }

    [<Fact>]
    let 要素が2つ以上 () = testing {
        let! slice = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 2 16)
        test Slice.vTryExactlyOne slice ==> Assert.vnone
    }

    [<Fact>]
    let Empty () = test Slice.vTryExactlyOne Slice.empty ==> Assert.vnone


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
        test Slice.vTryFind ((Mock.trueWhen<char> index), slice) ==> Assert.equal (ValueSome(slice[index]))
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
    let Empty () = test Slice.findIndex ((fun _ -> true), Slice.empty) ==> Assert.thrown<KeyNotFoundException>


module tryFindIndex =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 1 16)
        let! index = ArgGen.intRange 0 slice.Length
        test Slice.vTryFindIndex ((Mock.trueWhen<char> index), slice) ==> Assert.equal (ValueSome(index))
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
    let Empty () = test Slice.findIndexBack ((fun _ -> true), Slice.empty) ==> Assert.thrown<KeyNotFoundException>


module tryFindIndexBack =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 1 16)
        let! index = ArgGen.intRange 0 slice.Length
        let expected = ValueSome(slice.Length - index - 1)
        test Slice.vTryFindIndexBack ((Mock.trueWhen<char> index), slice) ==> Assert.equal expected
    }

    [<Fact>]
    let 存在しない () = testing {
        let! slice = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 1 16)
        test Slice.vTryFindIndexBack ((fun _ -> false), slice) ==> Assert.vnone
    }

    [<Fact>]
    let Empty () = test Slice.vTryFindIndexBack ((fun _ -> true), Slice.empty) ==> Assert.vnone


module fold =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice ArgGen.int (ArgGen.intRange 0 16)
        let! seed = ArgGen.int
        let expected = Seq.fold (-) seed slice
        test Slice.fold ((-), seed, slice) ==> Assert.equal expected
    }

    [<Fact>]
    let Empty () = testing {
        let! seed = ArgGen.int
        test Slice.fold ((-), seed, Slice.empty) ==> Assert.equal seed
    }


module foldBack =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice ArgGen.int (ArgGen.intRange 0 16)
        let! seed = ArgGen.int
        let expected = Seq.foldBack (-) slice seed
        test Slice.foldBack ((-), slice, seed) ==> Assert.equal expected
    }

    [<Fact>]
    let Empty () = testing {
        let! seed = ArgGen.int
        test Slice.foldBack ((-), Slice.empty, seed) ==> Assert.equal seed
    }


module fold2 =
    [<Fact>]
    let 正常 () = testing {
        let! slice1 = ArgGen.slice ArgGen.int (ArgGen.intRange 0 16)
        let! slice2 = ArgGen.slice ArgGen.int (ArgGen.intRange 0 16)
        let! seed = ArgGen.int
        let folder a b c = a - b * c
        let expected = Seq.fold2 folder seed slice1 slice2
        test Slice.fold2 (folder, seed, slice1, slice2) ==> Assert.equal expected
    }

    [<Fact>]
    let Empty () = testing {
        let! seed = ArgGen.int
        let folder a b c = a - b * c
        test Slice.fold2 (folder, seed, Slice.empty, Slice.empty) ==> Assert.equal seed
    }


module foldBack2 =
    [<Fact>]
    let 正常 () = testing {
        let! slice1 = ArgGen.slice ArgGen.int (ArgGen.intRange 0 16)
        let! slice2 = ArgGen.slice ArgGen.int (ArgGen.intRange 0 16)
        let! seed = ArgGen.int
        let folder a b c = a - b * c
        let expected = Seq.foldBack2 folder slice1 slice2 seed
        test Slice.foldBack2 (folder, slice1, slice2, seed) ==> Assert.equal expected
    }

    [<Fact>]
    let Empty () = testing {
        let! seed = ArgGen.int
        let folder a b c = a - b * c
        test Slice.foldBack2 (folder, Slice.empty, Slice.empty, seed) ==> Assert.equal seed
    }


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


module groupBy =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 0 16)
        let expected =
            slice |> Seq.groupBy id |> Seq.map (fun (x, y) -> struct (x, Slice.ofSeq y)) |> Seq.toArray
        test Slice.vGroupBy (id, slice) ==> Assert.sequentialEqual expected
    }

    [<Fact>]
    let Empty () = test Slice.vGroupBy (id, Slice.empty) ==> Assert.equal Slice.empty


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
        test Slice.vTryHead slice ==> Assert.equal (ValueSome(slice[0]))
    }

    [<Fact>]
    let Empty () = test Slice.vTryHead Slice.empty ==> Assert.vnone


module indexed =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice (ArgGen.intRange -1024 1023) (ArgGen.intRange 0 16)
        let expected = Seq.indexed slice |> Helpers.toValueTupleSeq |> Seq.toArray
        test Slice.vIndexed slice ==> Assert.sequentialEqual expected
    }

    [<Fact>]
    let Empty () = test Slice.vIndexed Slice.empty ==> Assert.equal Slice.empty


module init =
    [<Fact>]
    let 正常 () = testing {
        let! length = ArgGen.intRange 0 16
        let expected = List.init length char
        test Slice.init (length, char) ==> Assert.sequentialEqual expected
    }


module insertAt =
    [<Fact>]
    let 正常 () = testing {
        let! slice = 16, ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 1 16)
        let! index = 16, ArgGen.intRange 0 slice.Length
        let! value = 16, ArgGen.asciiChar
        let expected = Seq.insertAt index value slice
        test Slice.insertAt (index, value, slice) ==> Assert.sequentialEqual expected
    }


module insertSliceAt =
    [<Fact>]
    let 正常 () = testing {
        let! slice = 16, ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 1 16)
        let! index = 16, ArgGen.intRange 0 slice.Length
        let! values = 16, ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 1 16)
        let expected = Seq.insertManyAt index values slice
        test Slice.insertSliceAt (index, values, slice) ==> Assert.sequentialEqual expected
    }


module insertManyAt =
    [<Fact>]
    let 正常 () = testing {
        let! slice = 16, ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 1 16)
        let! index = 16, ArgGen.intRange 0 slice.Length
        let! values = 16, ArgGen.array ArgGen.asciiChar (ArgGen.intRange 1 16)
        let expected = Seq.insertManyAt index values slice
        test Slice.insertManyAt (index, values, slice) ==> Assert.sequentialEqual expected
    }


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


module iter =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice ArgGen.int (ArgGen.intRange 0 16)
        let actualItems = ResizeArray()
        let expectedItems = ResizeArray()
        Seq.iter expectedItems.Add slice
        do! test Slice.iter (actualItems.Add, slice)
        Assert.sequentialEqual expectedItems actualItems
    }


module iteri =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice ArgGen.int (ArgGen.intRange 0 16)
        let actualItems = ResizeArray()
        let expectedItems = ResizeArray()
        Seq.iteri (fun i x -> expectedItems.Add(i, x)) slice
        do! test Slice.iteri ((fun i x -> actualItems.Add(i, x)), slice)
        Assert.sequentialEqual expectedItems actualItems
    }


module iter2 =
    [<Fact>]
    let 正常 () = testing {
        let! slice1 = ArgGen.slice ArgGen.int (ArgGen.intRange 0 16)
        let! slice2 = ArgGen.slice ArgGen.int (ArgGen.intRange 0 16)
        let actualItems = ResizeArray()
        let expectedItems = ResizeArray()
        Seq.iter2 (fun x y -> expectedItems.Add(x, y)) slice1 slice2
        do! test Slice.iter2 ((fun x y -> actualItems.Add(x, y)), slice1, slice2)
        Assert.sequentialEqual expectedItems actualItems
    }


module iteri2 =
    [<Fact>]
    let 正常 () = testing {
        let! slice1 = ArgGen.slice ArgGen.int (ArgGen.intRange 0 16)
        let! slice2 = ArgGen.slice ArgGen.int (ArgGen.intRange 0 16)
        let actualItems = ResizeArray()
        let expectedItems = ResizeArray()
        Seq.iteri2 (fun i x y -> expectedItems.Add(i, x, y)) slice1 slice2
        do! test Slice.iteri2 ((fun i x y -> actualItems.Add(i, x, y)), slice1, slice2)
        Assert.sequentialEqual expectedItems actualItems
    }


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
        test Slice.vTryLast slice ==> Assert.equal (ValueSome(slice[slice.Length - 1]))
    }

    [<Fact>]
    let Empty () = test Slice.vTryLast Slice.empty ==> Assert.vnone


module length =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice ArgGen.int (ArgGen.intRange 0 16)
        test Slice.length slice ==> Assert.equal slice.Length
    }


module mapFold =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 0 16)
        let! state = ArgGen.int
        let expectedMapped, expecdexFolded = Seq.mapFold (fun s x -> uint16 x, s + int x) state slice
        let! struct (mapped, folded) = test Slice.vMapFold ((fun s x -> uint16 x, s + int x), state, slice)
        Assert.equal expecdexFolded folded
        Assert.sequentialEqual expectedMapped mapped
    }

    [<Fact>]
    let Empty () = testing {
        let! state = ArgGen.int
        let! struct (mapped, folded) = test Slice.vMapFold ((fun s x -> uint16 x, s + int x), state, Slice.empty)
        Assert.equal state folded
        Assert.sequentialEqual Slice.empty mapped
    }


module mapFoldBack =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 0 16)
        let! state = ArgGen.int
        let expectedMapped, expecdexFolded = Seq.mapFoldBack (fun x s -> uint16 x, s + int x) slice state
        let! struct (mapped, folded) = test Slice.vMapFoldBack ((fun x s -> uint16 x, s + int x), slice, state)
        Assert.equal expecdexFolded folded
        Assert.sequentialEqual expectedMapped mapped
    }

    [<Fact>]
    let Empty () = testing {
        let! state = ArgGen.int
        let! struct (mapped, folded) = test Slice.vMapFoldBack ((fun s x -> uint16 x, s + int x), Slice.empty, state)
        Assert.equal state folded
        Assert.sequentialEqual Slice.empty mapped
    }


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


module pairwise =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 2 16)
        let expected = Seq.pairwise slice |> Helpers.toValueTupleSeq
        test Slice.vPairwise slice ==> Assert.sequentialEqual expected
    }

    [<Fact>]
    let Empty () = test Slice.vPairwise Slice.empty ==> Assert.thrown<ArgumentException>


module vPartition =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice ArgGen.int (ArgGen.intRange 0 16)
        let expected1, expected2 = Array.partition ((<) 0) (Array.ofSeq slice)
        let! struct (actual1, actual2) = test Slice.vPartition (((<) 0), slice)
        Assert.sequentialEqual expected1 actual1
        Assert.sequentialEqual expected2 actual2
    }

    let Empty () = test Slice.vPartition (((<) 0), Slice.empty) ==> Assert.equal (struct (Slice.empty, Slice.empty))


module permute =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice ArgGen.int (ArgGen.intRange 1 16)
        let expected = Array.permute (fun x -> (x + 1) % slice.Length) (Array.ofSeq slice)
        test Slice.permute ((fun x -> (x + 1) % slice.Length), slice) ==> Assert.sequentialEqual expected
    }

    let Empty () = test Slice.permute (id, Slice.empty) ==> Assert.equal Slice.empty


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


module reduce =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice ArgGen.int (ArgGen.intRange 1 16)
        let expected = Seq.reduce (-) slice
        test Slice.reduce ((-), slice) ==> Assert.equal expected
    }

    [<Fact>]
    let Empty () = test Slice.reduce ((-), Slice.empty) ==> Assert.thrown<ArgumentException>


module reduceBack =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice ArgGen.int (ArgGen.intRange 1 16)
        let expected = Seq.reduceBack (-) slice
        test Slice.reduceBack ((-), slice) ==> Assert.equal expected
    }

    [<Fact>]
    let Empty () = test Slice.reduceBack ((-), Slice.empty) ==> Assert.thrown<ArgumentException>

module removeAt =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 1 16)
        let! index = ArgGen.intRange 0 slice.Length
        let expected = Seq.removeAt index slice
        test Slice.removeAt (index, slice) ==> Assert.sequentialEqual expected
    }


module removeManyAt =
    [<Fact>]
    let 正常 () = testing {
        let! slice = 16, ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 1 16)
        let! index = 16, ArgGen.intRange 0 slice.Length
        let! length = 16, ArgGen.intRange 0 (slice.Length - index)
        let expected = Seq.removeManyAt index length slice
        test Slice.removeManyAt (index, length, slice) ==> Assert.sequentialEqual expected
    }

    let 範囲外でもエラーにならない () = testing {
        let! slice = 16, ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 1 16)
        let! index = 16, ArgGen.intRange 0 16
        let! length = 16, ArgGen.intRange 0 16
        let expectedRemoveLength = min length (slice.Length - index)
        let! result = test Slice.removeManyAt (index, length, slice)
        Assert.equal (slice.Length - expectedRemoveLength) result.Length
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


module rev =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 0 16)
        test Slice.rev slice ==> Assert.sequentialEqual (Seq.rev slice)
    }
