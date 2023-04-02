namespace Minori.Slices.Test.SliceBuilder

open System
open Testexp
open Minori.Slices
open Minori.Slices.Test


module Yield =
    let testFunc (x: 'T) = slice { x }

    [<Fact>]
    let 正常 () = testing {
        let! c = ArgGen.asciiChar
        test testFunc c ==> Assert.equal (Slice.singleton c)
    }


module YieldFrom =
    let testFunc1 (xs: 'T slice) = slice { yield! xs }
    let testFunc2 (xs: seq<'T>) = slice { yield! xs }

    [<Fact>]
    let 正常_Slice () = testing {
        let! s = ArgGen.slice ArgGen.asciiChar (ArgGen.intRange 0 16)
        test testFunc1 s ==> Assert.equal s
    }

    [<Fact>]
    let 正常_Seq () = testing {
        let! s = ArgGen.array ArgGen.asciiChar (ArgGen.intRange 0 16)
        test testFunc2 s ==> Assert.sequentialEqual s
    }


module Zero =
    let testFunc cond (x: char) = slice {
        if cond then
            x
    }

    [<Fact>]
    let 正常_True () = testing {
        let! c = ArgGen.asciiChar
        test testFunc (true, c) ==> Assert.equal (Slice.singleton c)
    }

    [<Fact>]
    let 正常_False () = testing {
        let! c = ArgGen.asciiChar
        test testFunc (false, c) ==> Assert.equal (Slice.empty)
    }


module Combine =
    let testFunc (x: char) (y: char) = slice {
        x
        y
    }

    [<Fact>]
    let 正常 () = testing {
        let! x = ArgGen.asciiChar
        let! y = ArgGen.asciiChar
        test testFunc (x, y) ==> Assert.sequentialEqual [| x; y |]
    }


module For =
    let testFunc1 (xs: int slice) = slice { for x in xs -> x * 2 }
    let testFunc2 (xs: seq<int>) = slice { for x in xs -> x * 2 }

    [<Fact>]
    let 正常_Slice () = testing {
        let! xs = ArgGen.slice ArgGen.int (ArgGen.intRange 0 16)
        test testFunc1 xs ==> Assert.sequentialEqual [| for x in xs -> x * 2 |]
    }

    [<Fact>]
    let 正常_Seq () = testing {
        let! xs = ArgGen.array ArgGen.int (ArgGen.intRange 0 16)
        test testFunc2 xs ==> Assert.sequentialEqual [| for x in xs -> x * 2 |]
    }


module While =
    let testFunc count = slice {
        let mutable i = 0
        while i < count do
            i * 2
            i <- i + 1
    }

    let expected count = [|
        let mutable i = 0
        while i < count do
            i * 2
            i <- i + 1
    |]

    [<Fact>]
    let 正常 () = testing {
        let! count = ArgGen.intRange 0 16
        test testFunc count ==> Assert.sequentialEqual (expected count)
    }


module Using =
    let testFunc1 resource = slice {
        use _ = resource
        1
        2
    }

    let testFunc2 resource =
        try
            slice {
                use _ = resource
                1
                raise (Exception())
            }
        with _ -> slice { 3 }

    [<Fact>]
    let 正常 () = testing {
        let disposable = Mock.disposable ()
        let! result = test testFunc1 disposable
        Assert.sequentialEqual [| 1; 2 |] result
        Assert.disposed disposable
    }

    [<Fact>]
    let 正常_例外発生時 () = testing {
        let disposable = Mock.disposable ()
        let! result = test testFunc2 disposable
        Assert.sequentialEqual [| 3 |] result
        Assert.disposed disposable
    }


module TryWith =
    let testFunc () = slice {
        try
            1
            2
            raise (Exception())
        with ex ->
            3
            4
    }

    [<Fact>]
    let 正常 () = test testFunc () ==> Assert.sequentialEqual [| 3; 4 |]


module TryFinally =
    let testFunc1 (resource: IDisposable) = slice {
        try
            1
            2
        finally
            resource.Dispose()
    }

    let testFunc2 (resource: IDisposable) =
        try
            slice {
                try
                    1
                    2
                    raise (Exception())
                finally
                    resource.Dispose()
            }
        with _ -> slice { 3 }

    [<Fact>]
    let 正常 () = testing {
        let disposable = Mock.disposable ()
        let! result = test testFunc1 disposable
        Assert.sequentialEqual [| 1; 2 |] result
        Assert.disposed disposable
    }

    [<Fact>]
    let 正常_例外発生時 () = testing {
        let disposable = Mock.disposable ()
        let! result = test testFunc2 disposable
        Assert.sequentialEqual [| 3 |] result
        Assert.disposed disposable

    }
