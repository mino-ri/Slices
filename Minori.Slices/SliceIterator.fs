namespace Minori.Slices

open System.Collections.Generic
open System.Diagnostics.CodeAnalysis

[<Struct; ExcludeFromCodeCoverage>]
type SliceIterator<'T> =
    val private Back: 'T[]
    val private Start: int
    val private Length: int
    val mutable private Index: int

    internal new(back, start, length) = { Back = back; Start = start; Length = length; Index = -1 }

    member this.MoveNext() =
        this.Index <- this.Index + 1
        this.Index < this.Length

    member this.Current = this.Back[this.Start + this.Index]

    interface IEnumerator<'T> with
        member this.Current = box this.Current
        member this.Current = this.Current
        member this.MoveNext() = this.MoveNext()
        member _.Dispose() = ()
        member _.Reset() = ()


[<ExcludeFromCodeCoverage>]
type internal SliceIteratorRef<'T>(inner: SliceIterator<'T>) =
    let mutable inner = inner
    interface IEnumerator<'T> with
        member _.Current = box inner.Current
        member _.Current = inner.Current
        member _.MoveNext() = inner.MoveNext()
        member _.Dispose() = ()
        member _.Reset() = ()
