namespace Minori.Slices

#nowarn "1204"

open System
open System.Collections.Generic
open System.Diagnostics.CodeAnalysis
open System.Collections
#if NET5_0_OR_GREATER
open System.Text.Json
open System.Text.Json.Serialization
#endif

/// Represents immutable memory block.
#if NET5_0_OR_GREATER
[<JsonConverter(typeof<SliceJsonConverterFactory>)>]
#endif
[<Struct; CustomEquality; NoComparison>]
type Slice<'T> =
    val internal Back: SliceBack<'T>
    val internal Start: int
    /// Gets the length of the slice.
    val Length: int

    internal new(back: SliceBack<'T>, start, length) =
        let x: int list = []
        assert (start + length <= back.Field.Length)
        { Back = back; Start = start; Length = length }

    /// Gets the number of elements for which there is space allocated in the underlying array.
    member this.Capacity =
        match this.Back with
        | null -> 0
        | _ -> this.Back.Field.Length - this.Start

    /// Indexes into the slice. The first element has index 0.
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
    static member internal WrapArray(source: 'T[]) =
        if source.Length = 0 then
            Slice()
        else
            Slice(SliceBack(source, source.Length), 0, source.Length)

    [<ExcludeFromCodeCoverage>]
    member inline internal this.ForSet(index) : byref<'T> =
        assert (uint index < uint this.Length)
        &this.Back.Field[this.Start + index]

    /// Returns the Span that references the same range as this slice.
    member this.AsSpan() =
        if this.Length = 0 then
            ReadOnlySpan.Empty
        else
            ReadOnlySpan(this.Back.Field, this.Start, this.Length)

    /// Returns the Memory that references the same range as this slice.
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

    /// Returns the sub-slice. This method is for C# slicing syntax.
    member this.Slice(start) =
        match this.Back with
        | null -> Slice()
        | _ ->
            let start = Math.Clamp(start, 0, this.Length)
            Slice(this.Back, this.Start + start, this.Length - start)

    /// Returns the sub-slice. This method is for C# slicing syntax.
    member this.Slice(start, length) =
        match this.Back with
        | null -> Slice()
        | _ ->
            let start = Math.Clamp(start, 0, this.Length)
            let length = Math.Clamp(length, 0, this.Length - start)
            Slice(this.Back, this.Start + start, length)

    /// Returns the sub-slice. This method is for F# slicing syntax.
    member this.GetSlice(startIndex, endIndex) =
        match this.Back with
        | null -> Slice()
        | _ ->
            let startIndex = Math.Clamp(defaultArg startIndex 0, 0, this.Length)
            let endIndex = Math.Clamp(defaultArg endIndex this.Length, startIndex, this.Length)
            Slice(this.Back, this.Start + startIndex, endIndex - startIndex)

    /// Returns a read-only reference to an object of type T that can be used for pinning.
    [<ExcludeFromCodeCoverage>]
    member this.GetPinnableReference() = this.AsSpan().GetPinnableReference()

    /// Returns an enumerator.
    member this.GetEnumerator() = new SliceIterator<'T>(this.FieldOrEmpty, this.Start, this.Length)

    /// Determines whether the specified slice is equal to this slice.
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

    /// Determines whether the specified object is equal to this slice.
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

    /// Gets the hash code for the slice.
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

#if NET5_0_OR_GREATER
and internal SliceJsonConverter<'T>() =
    inherit JsonConverter<Slice<'T>>()

    override _.Read(reader, _, options) = Slice.WrapArray(JsonSerializer.Deserialize<'T[]>(&reader, options))

    override _.Write(writer, value, options) =
        writer.WriteStartArray()
        for x in value do
            JsonSerializer.Serialize(writer, x, options)
        writer.WriteEndArray()


and internal ByteSliceJsonConverter() =
    inherit JsonConverter<Slice<byte>>()
    override _.Read(reader, _, _) = Slice.WrapArray(reader.GetBytesFromBase64())

    override _.Write(writer, value, _) = writer.WriteBase64StringValue(value.AsSpan())


/// Provides JsonConverter for slices.
and SliceJsonConverterFactory() =
    inherit JsonConverterFactory()
    let genericType = typedefof<Slice<_>>
    let byteSliceType = typeof<Slice<byte>>
    let converterType = typedefof<SliceJsonConverter<_>>

    /// <inheritdoc />
    override _.CanConvert(typeToConvet) =
        typeToConvet.IsGenericType && typeToConvet.GetGenericTypeDefinition() = genericType

    /// <inheritdoc />
    override _.CreateConverter(typeToConvet, _) =
        if typeToConvet = byteSliceType then
            ByteSliceJsonConverter()
        else
            let elementType = typeToConvet.GetGenericArguments()[0]
            Activator.CreateInstance(converterType.MakeGenericType(elementType)) :?> JsonConverter
#endif


/// Represents immutable memory block.
type 'T slice = Slice<'T>
