namespace Minori.Slices

#nowarn "1204"

open System
open System.Collections.Generic
open System.Diagnostics.CodeAnalysis
open System.Collections

[<Struct; CustomEquality; NoComparison>]
type Slice<'T> =
    val internal Back: SliceBack<'T>
    val internal Start: int
    val Length: int

    internal new(back: SliceBack<'T>, start, length) =
        assert (start + length <= back.Field.Length)
        { Back = back; Start = start; Length = length }

    member this.Capacity =
        match this.Back with
        | null -> 0
        | _ -> this.Back.Field.Length - this.Start

    member this.Item
        with get index: inref<'T> =
            if uint index >= uint this.Length then
                raise (ArgumentOutOfRangeException(nameof index))
            &this.Back.Field[this.Start + index]

    [<ExcludeFromCodeCoverage>]
    member internal this.FieldOrEmpty =
        match this.Back with
        | null -> array.Empty()
        | _ -> this.Back.Field

    [<ExcludeFromCodeCoverage>]
    member inline internal this.ForSet(index) : byref<'T> =
        assert (uint index < uint this.Length)
        &this.Back.Field[this.Start + index]

    member this.AsSpan() =
        if this.Length = 0 then
            ReadOnlySpan.Empty
        else
            ReadOnlySpan(this.Back.Field, this.Start, this.Length)

    member this.AsMemory() =
        if this.Length = 0 then
            ReadOnlyMemory.Empty
        else
            ReadOnlyMemory(this.Back.Field, this.Start, this.Length)

    member internal this.AsMutableSpan() =
        if this.Length = 0 then
            Span.Empty
        else
            Span(this.Back.Field, this.Start, this.Length)

    member this.Slice(start) =
        match this.Back with
        | null -> Slice()
        | _ ->
            let start = Math.Clamp(start, 0, this.Length)
            Slice(this.Back, this.Start + start, this.Length - start)

    member this.Slice(start, length) =
        match this.Back with
        | null -> Slice()
        | _ ->
            let start = Math.Clamp(start, 0, this.Length)
            let length = Math.Clamp(length, 0, this.Length - start)
            Slice(this.Back, this.Start + start, length)

    member this.GetSlice(startIndex, endIndex) =
        match this.Back with
        | null -> Slice()
        | _ ->
            let startIndex = Math.Clamp(defaultArg startIndex 0, 0, this.Length)
            let endIndex = Math.Clamp(defaultArg endIndex this.Length, startIndex, this.Length)
            Slice(this.Back, this.Start + startIndex, endIndex - startIndex)

    [<ExcludeFromCodeCoverage>]
    member this.GetPinnableReference() = this.AsSpan().GetPinnableReference()

    member this.GetEnumerator() = new SliceIterator<'T>(this.FieldOrEmpty, this.Start, this.Length)

    member this.Equals(other: Slice<'T>) =
        if this.Length <> other.Length then
            false
        else
            let span = this.AsSpan()
            let otherSpan = other.AsSpan()
            let mutable equals = true
            let mutable i = 0
            while equals && i < span.Length do
                equals <- LanguagePrimitives.HashCompare.GenericEqualityERIntrinsic span[i] otherSpan[i]
                i <- i + 1
            equals

    // member private this.Equals(s: Slice<'T>, comparer: IEqualityComparer) =
    //     if s.Length <> this.Length then
    //         false
    //     else
    //         let span = this.AsSpan()
    //         let otherSpan = s.AsSpan()
    //         let mutable equals = true
    //         let mutable i = 0
    //         while equals && i < span.Length do
    //             equals <- LanguagePrimitives.HashCompare.GenericEqualityWithComparerIntrinsic comparer span[i] otherSpan[i]
    //             i <- i + 1
    //         equals

    override this.Equals(other: obj) =
        match other with
        | :? Slice<'T> as s -> this.Equals(s)
        | _ -> false

    [<ExcludeFromCodeCoverage>]
    member private this.GetHashCode(comparer: IEqualityComparer) =
        let mutable hashCode = 0
        for x in this do
            hashCode <-
                -1640531527
                + LanguagePrimitives.HashCompare.GenericHashWithComparerIntrinsic comparer x
                + (hashCode <<< 6)
                + (hashCode >>> 2)
        hashCode

    [<ExcludeFromCodeCoverage>]
    override this.GetHashCode() = this.GetHashCode(LanguagePrimitives.GenericEqualityComparer)

    interface IEquatable<Slice<'T>> with
        [<ExcludeFromCodeCoverage>]
        member this.Equals(other) = this.Equals(other)

    // interface IStructuralEquatable with
    //     member this.Equals(other: obj, comparer: IEqualityComparer) =
    //         match other with
    //         | :? Slice<'T> as s -> this.Equals(s, comparer)
    //         | _ -> false
    //
    //     [<ExcludeFromCodeCoverage>]
    //     member this.GetHashCode(comparer) = this.GetHashCode(comparer)

    interface IReadOnlyList<'T> with
        member this.Count = this.Length
        member this.Item
            with get index = this[index]
        member this.GetEnumerator() : Collections.IEnumerator = new SliceIteratorRef<'T>(this.GetEnumerator())
        member this.GetEnumerator() : IEnumerator<'T> = new SliceIteratorRef<'T>(this.GetEnumerator())


type 'T slice = Slice<'T>
