namespace Minori.Slices.Test.Slice

open System
open Testexp
open Minori.Slices
open Minori.Slices.Test


module ofArray =
    [<Fact>]
    let 正常 () = testing {
        let! array = ArgGen.array ArgGen.asciiChar (ArgGen.intRange 0 16)
        test Slice.ofArray<char> array ==> Assert.sequentialEqual array
    }


module ofSpan =
    let testFunc (array: char[]) = Slice.ofSpan (ReadOnlySpan(array))

    [<Fact>]
    let 正常 () = testing {
        let! array = ArgGen.array ArgGen.asciiChar (ArgGen.intRange 0 16)
        test testFunc array ==> Assert.sequentialEqual array
    }


module ofSeq =
    [<Fact>]
    let 正常 () = testing {
        let! array = ArgGen.array ArgGen.asciiChar (ArgGen.intRange 0 16)
        let seq = seq { for item in array -> item }
        test Slice.ofSeq<char> seq ==> Assert.sequentialEqual array
    }


module toArray =
    [<Fact>]
    let 正常 () = testing {
        let! array = ArgGen.array ArgGen.asciiChar (ArgGen.intRange 0 16)
        let slice = Slice.ofArray array
        test Slice.toArray<char> slice ==> Assert.sequentialEqual array
    }


module toSeq =
    [<Fact>]
    let 正常 () = testing {
        let! slice = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 0 16)
        test Slice.toSeq<char> slice ==> Assert.sequentialEqual slice
    }
