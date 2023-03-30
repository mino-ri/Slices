namespace Minori.Slices.Test

open System

type ICallObserver =
    abstract member IsCalled: bool


type DisposeObserver() =
    let mutable disposed = false
    member _.IsDisposed = disposed
    member _.Dispose() = disposed <- true
    interface IDisposable with
        member this.Dispose() = this.Dispose()
    interface ICallObserver with
        member _.IsCalled = disposed


type CallObserver<'T when 'T: equality>(expectedArgs: 'T) =
    let mutable called = false
    member _.Call(args: 'T) =
        if args = expectedArgs then
            called <- true
    member _.IsCalled = called

    interface ICallObserver with
        member _.IsCalled = called


type ArgumentRecorder<'T, 'R>(returns: 'R) =
    let mutable arguments = ValueNone
    member _.Call(args: 'T) =
        arguments <- ValueSome(args)
        returns
    member _.ActualArgs = arguments

    interface ICallObserver with
        member _.IsCalled = arguments.IsSome


module Mock =
    let call expectedArgs = CallObserver(expectedArgs)

    let getArgs returnValue = ArgumentRecorder(returnValue)

    let disposable () = new DisposeObserver()

    let trueWhen<'T> count =
        let mutable calledCount = -1
        fun (_: 'T) ->
            calledCount <- calledCount + 1
            calledCount = count

    let trueWhen2<'T1, 'T2> count =
        let mutable calledCount = -1
        fun (_: 'T1) (_: 'T2) ->
            calledCount <- calledCount + 1
            calledCount = count

    let falseWhen<'T> count =
        let mutable calledCount = -1
        fun (_: 'T) ->
            calledCount <- calledCount + 1
            calledCount <> count

    let falseWhen2<'T1, 'T2> count =
        let mutable calledCount = -1
        fun (_: 'T1) (_: 'T2) ->
            calledCount <- calledCount + 1
            calledCount <> count
