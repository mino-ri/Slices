namespace Minori.Slices.Test.Slice

open System
open Testexp
open Minori.Slices
open Minori.Slices.Test


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


module scan =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice ArgGen.int (ArgGen.intRange 0 16)
        let! seed = ArgGen.int
        let expected = Seq.scan (-) seed slice
        test Slice.scan ((-), seed, slice) ==> Assert.sequentialEqual expected
    }

    [<Fact>]
    let Empty () = testing {
        let! seed = ArgGen.int
        test Slice.scan ((-), seed, Slice.empty) ==> Assert.sequentialEqual [| seed |]
    }


module scanBack =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice ArgGen.int (ArgGen.intRange 0 16)
        let! seed = ArgGen.int
        let expected = Seq.scanBack (-) slice seed
        test Slice.scanBack ((-), slice, seed) ==> Assert.sequentialEqual expected
    }

    [<Fact>]
    let Empty () = testing {
        let! seed = ArgGen.int
        test Slice.scanBack ((-), Slice.empty, seed) ==> Assert.sequentialEqual [| seed |]
    }


module mapFold =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 0 16)
        let! state = ArgGen.int
        let expectedMapped, expecdexFolded = Seq.mapFold (fun s x -> uint16 x, s + int x) state slice
        let! mapped, folded = test Slice.mapFold ((fun s x -> uint16 x, s + int x), state, slice)
        Assert.equal expecdexFolded folded
        Assert.sequentialEqual expectedMapped mapped
    }

    [<Fact>]
    let Empty () = testing {
        let! state = ArgGen.int
        let! mapped, folded = test Slice.mapFold ((fun s x -> uint16 x, s + int x), state, Slice.empty)
        Assert.equal state folded
        Assert.sequentialEqual Slice.empty mapped
    }


module vMapFold =
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
        let! mapped, folded = test Slice.mapFoldBack ((fun x s -> uint16 x, s + int x), slice, state)
        Assert.equal expecdexFolded folded
        Assert.sequentialEqual expectedMapped mapped
    }

    [<Fact>]
    let Empty () = testing {
        let! state = ArgGen.int
        let! mapped, folded = test Slice.mapFoldBack ((fun s x -> uint16 x, s + int x), Slice.empty, state)
        Assert.equal state folded
        Assert.sequentialEqual Slice.empty mapped
    }


module vMapFoldBack =
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
