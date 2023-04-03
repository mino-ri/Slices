/// Contains operations for working with values of type slice.
[<RequireQualifiedAccess>]
module Minori.Slices.Slice

open System
open System.Collections.Generic
open System.Diagnostics.CodeAnalysis

// ============================== exception utility ==============================

[<ExcludeFromCodeCoverage>]
let private raiseNotFound () =
    raise (KeyNotFoundException("An value satisfying the predicate was not found in the slice."))

[<ExcludeFromCodeCoverage>]
let private raiseEmpty (paramName: string) = invalidArg paramName "The input sequence was empty."

[<ExcludeFromCodeCoverage>]
let private raiseNeedMoreElement (paramName: string) =
    invalidArg paramName "The input sequence must contain two or more elements."

[<ExcludeFromCodeCoverage>]
let private raiseMoreThanOneElement (paramName: string) =
    invalidArg paramName "The input sequence contains more than one element."

[<ExcludeFromCodeCoverage>]
let private raiseIndexOutOfRange (paramName: string) =
    invalidArg paramName "The index is below 0 or greater than length of the slice."

// ============================== internal utility ==============================

let private isOwner (slice: 'T slice) =
    assert (slice.Start + slice.Length <= slice.Back.Written)
    slice.Start + slice.Length = slice.Back.Written

let private extendCap current required =
    if current >= 1024 then
        max (current + required) (current + current / 4)
    else
        max (current + required) (current * 2)

let private createDefaultCap<'T> capacity =
    let capacity = min 8 capacity
    Slice<'T>(SliceBack.alloc capacity, 0, 0)

let private copyCore length capacity (slice: 'T slice) =
    let back = SliceBack.alloc (max length capacity)
    back.Written <- length
    let newSlice = Slice(back, 0, length)
    slice.AsSpan().CopyTo(newSlice.AsMutableSpan())
    newSlice

let private extend required (slice: 'T slice) =
    if slice.Capacity < slice.Length + required then
        copyCore (slice.Length + required) (extendCap slice.Capacity required) slice
    elif not (isOwner slice) then
        copyCore (slice.Length + required) (extendCap slice.Length required) slice
    else
        slice.Back.Written <- slice.Start + slice.Length + required
        Slice(slice.Back, slice.Start, slice.Length + required)

// ============================== basic operations ==============================

/// Contains dangerous operations for working with values of type slice.
module Unchecked =
    /// Returns a slice that references given array.
    /// When an element of the array is changed, the element of slice is also changed.
    /// This method breaks immutability of slice.
    let wrapArray<'T> (array: 'T[]) : 'T slice = Slice.WrapArray(array)

/// Gets the length of the slice.
[<ExcludeFromCodeCoverage>]
let inline length (slice: 'T slice) = slice.Length

/// Gets the number of elements for which there is space allocated in the underlying array.
[<ExcludeFromCodeCoverage>]
let inline capacity (slice: 'T slice) = slice.Capacity

/// Returns true if the slice contains no elements, false otherwise.
[<ExcludeFromCodeCoverage>]
let inline isEmpty (slice: 'T slice) = slice.Length = 0

/// Creates copy of the slice. The capacity of the new slice is specified value.
let copyWithCap<'T> capacity (slice: 'T slice) : 'T slice = copyCore slice.Length capacity slice

/// Creates copy of the slice.
let copy<'T> (slice: 'T slice) : 'T slice = copyCore slice.Length slice.Length slice

/// Return a new slice with a new item added.
let add<'T> item (slice: 'T slice) : 'T slice =
    let newSlice = extend 1 slice
    newSlice.ForSet(newSlice.Length - 1) <- item
    newSlice

/// Return a new slice with a new items added.
let addSpan<'T> (items: ReadOnlySpan<'T>) (slice: 'T slice) : 'T slice =
    if items.Length = 0 then
        slice
    else
        let newSlice = extend items.Length slice
        assert (newSlice.Length = items.Length + slice.Length)
        items.CopyTo(newSlice.AsMutableSpan().Slice(slice.Length))
        newSlice

/// Return a new slice with a new items added.
[<ExcludeFromCodeCoverage>]
let addSlice<'T> (items: 'T slice) (slice: 'T slice) = addSpan (items.AsSpan()) slice

/// Return a new slice with a new items added.
let addRange<'T> (items: seq<'T>) (slice: 'T slice) : 'T slice =
    match items with
    | null -> slice
    | :? ('T[]) as array -> addSpan (ReadOnlySpan(array)) slice
    | :? ('T slice) as s -> addSpan (s.AsSpan()) slice
    | :? ICollection<'T> as collection ->
        if collection.Count = 0 then
            slice
        else
            let newSlice = extend collection.Count slice
            collection.CopyTo(newSlice.Back.Field, newSlice.Start + slice.Length)
            newSlice
    | :? IReadOnlyCollection<'T> as collection ->
        if collection.Count = 0 then
            slice
        else
            let newSlice = extend collection.Count slice
            let mutable index = slice.Length
            for x in collection do
                newSlice.ForSet(index) <- x
                index <- index + 1
            newSlice
    | _ ->
        let mutable slice = slice
        for x in items do
            slice <- add x slice
        slice

/// Applies the given function to each element of the slice.
[<ExcludeFromCodeCoverage>]
let inline iter ([<InlineIfLambda>] action: 'T -> unit) (slice: 'T slice) =
    for x in slice do
        action x

/// Applies the given function to each element of the slice.
/// The integer passed to the function indicates the index of element.
[<ExcludeFromCodeCoverage>]
let inline iteri ([<InlineIfLambda>] action: int -> 'T -> unit) (slice: 'T slice) =
    let mutable index = 0
    for x in slice do
        action index x
        index <- index + 1

/// Applies the given function to two slices simultaneously.
/// If one sequence is shorter than the other then the remaining elements of the longer sequence are ignored.
let iter2 (action: 'T1 -> 'T2 -> unit) (slice1: 'T1 slice) (slice2: 'T2 slice) =
    if isEmpty slice1 || isEmpty slice2 then
        ()
    else
        let f = OptimizedClosures.FSharpFunc<_, _, _>.Adapt(action)
        let span1 = slice1.AsSpan()
        let span2 = slice2.AsSpan()
        let length = Operators.min span1.Length span2.Length
        for i in 0 .. length - 1 do
            f.Invoke(span1[i], span2[i])

/// Applies the given function to two slices simultaneously.
/// If one slice is shorter than the other then the remaining elements of the longer slice are ignored.
/// The integer passed to the function indicates the index of element.
let iteri2 (action: int -> 'T1 -> 'T2 -> unit) (slice1: 'T1 slice) (slice2: 'T2 slice) =
    if isEmpty slice1 || isEmpty slice2 then
        ()
    else
        let f = OptimizedClosures.FSharpFunc<_, _, _, _>.Adapt(action)
        let span1 = slice1.AsSpan()
        let span2 = slice2.AsSpan()
        let length = Operators.min span1.Length span2.Length
        for i in 0 .. length - 1 do
            f.Invoke(i, span1[i], span2[i])

/// Matches slice of length greater than or equal to 1, and breaks down into the first element and remaining slice.
[<return: Struct>]
let (|Cons|_|) (slice: 'T1 slice) =
    if slice.Length < 1 then
        ValueNone
    else
        ValueSome struct (slice[0], slice[1..])

/// Matches slice of length greater than or equal to 2, and breaks down into the first two elements and remaining slice.
[<return: Struct>]
let (|Cons2|_|) (slice: 'T1 slice) =
    if slice.Length < 2 then
        ValueNone
    else
        ValueSome struct (slice[0], slice[1], slice[2..])

/// Matches slice of length greater than or equal to 3, and breaks down into the first three elements and remaining slice.
[<return: Struct>]
let (|Cons3|_|) (slice: 'T1 slice) =
    if slice.Length < 3 then
        ValueNone
    else
        ValueSome struct (slice[0], slice[1], slice[2], slice[3..])

/// Matches slice of length greater than or equal to 1, and breaks down into the last element and remaining slice.
[<return: Struct>]
let (|ConsBack|_|) (slice: 'T1 slice) =
    if slice.Length < 1 then
        ValueNone
    else
        ValueSome struct (slice[.. slice.Length - 1], slice[slice.Length - 1])

/// Matches slice of length greater than or equal to 2, and breaks down into the last two elements and remaining slice.
[<return: Struct>]
let (|ConsBack2|_|) (slice: 'T1 slice) =
    if slice.Length < 2 then
        ValueNone
    else
        ValueSome struct (slice[.. slice.Length - 2], slice[slice.Length - 2], slice[slice.Length - 1])

/// Matches slice of length greater than or equal to 2, and breaks down into the last three elements and remaining slice.
[<return: Struct>]
let (|ConsBack3|_|) (slice: 'T1 slice) =
    if slice.Length < 3 then
        ValueNone
    else
        ValueSome
            struct (slice[.. slice.Length - 3],
                    slice[slice.Length - 3],
                    slice[slice.Length - 2],
                    slice[slice.Length - 1])

// ============================== creators & initializers ==============================

let private zeroCreate<'T> length =
    let back = SliceBack.alloc length
    back.Written <- length
    Slice<'T>(back, 0, length)

[<ExcludeFromCodeCoverage>]
let inline private initInline length ([<InlineIfLambda>] initializer: int -> 'T) : 'T slice =
    let result = zeroCreate length
    let resultSpan = result.AsMutableSpan()
    for i in 0 .. length - 1 do
        resultSpan[i] <- initializer i
    result

/// Creates a slice by calling the given generator on each index.
let init length (initializer: int -> 'T) : 'T slice =
    let result = zeroCreate length
    let resultSpan = result.AsMutableSpan()
    for i in 0 .. length - 1 do
        resultSpan[i] <- initializer i
    result

/// Returns an empty slice.
[<ExcludeFromCodeCoverage>]
let inline empty<'T> : 'T slice = Slice<'T>()

/// Creates a zero-length slice with the given capacity.
let create<'T> capacity : 'T slice =
    if capacity <= 0 then
        empty
    else
        Slice<'T>(SliceBack.alloc capacity, 0, 0)

/// Creates a slice than contains one item only.
let singleton value : 'T slice =
    let result = zeroCreate 1
    result.ForSet(0) <- value
    result

/// Creates a slice by replicating the given initial value.
let replicate count (initial: 'T) : 'T slice =
    if count < 0 then
        invalidArg (nameof count) "count must be positive."
    initInline count (fun _ -> initial)

/// Returns a slice that contains the elements generated by the given computation.
/// The generator is repeatedly called to build the slice until it returns None.
/// The given initial state argument is passed to the element generator.
let unfold (generator: 'State -> ('T * 'State) option) (state: 'State) : 'T slice =
    let mutable result = empty
    let mutable loop = true
    let mutable state = state
    while loop do
        match generator state with
        | None -> loop <- false
        | Some(x, s) ->
            state <- s
            result <- add x result
    result

/// Returns a slice that contains the elements generated by the given computation.
/// The generator is repeatedly called to build the slice until it returns None.
/// The given initial state argument is passed to the element generator.
let vUnfold (generator: 'State -> struct ('T * 'State) voption) (state: 'State) : 'T slice =
    let mutable result = empty
    let mutable loop = true
    let mutable state = state
    while loop do
        match generator state with
        | ValueNone -> loop <- false
        | ValueSome(x, s) ->
            state <- s
            result <- add x result
    result

// ============================== converters ==============================

[<ExcludeFromCodeCoverage>]
let inline private ofDict<'Key, 'Value> (dict: Dictionary<'Key, 'Value>) =
    let result = zeroCreate dict.Count
    let resultSpan = result.AsMutableSpan()
    let mutable i = 0
    for KeyValue(key, value) in dict do
        resultSpan[i] <- key, value
        i <- i + 1
    result

[<ExcludeFromCodeCoverage>]
let inline private vOfDict<'Key, 'Value> (dict: Dictionary<'Key, 'Value>) =
    let result = zeroCreate dict.Count
    let resultSpan = result.AsMutableSpan()
    let mutable i = 0
    for KeyValue(key, value) in dict do
        resultSpan[i] <- struct (key, value)
        i <- i + 1
    result

/// Builds a new slice from the given span.
[<ExcludeFromCodeCoverage>]
let ofSpan<'T> (span: ReadOnlySpan<'T>) : 'T slice = addSpan span (create span.Length)

/// Builds a new slice from the given array.
[<ExcludeFromCodeCoverage>]
let ofArray<'T> (array: 'T[]) : 'T slice = create array.Length |> addSpan (ReadOnlySpan(array))

/// Builds a new slice from the given list.
[<ExcludeFromCodeCoverage>]
let ofList<'T> (list: 'T list) : 'T slice = create list.Length |> addRange list

/// Builds a new slice from the given enumerable object.
[<ExcludeFromCodeCoverage>]
let ofSeq<'T> (items: seq<'T>) = addRange items empty

/// Builds a new array from the given slice.
[<ExcludeFromCodeCoverage>]
let inline toArray<'T> (slice: 'T slice) = slice.AsSpan().ToArray()

/// Builds a new list from the given slice.
[<ExcludeFromCodeCoverage>]
let toList<'T> (slice: 'T slice) = List.ofSeq slice

/// Views the given slice as a sequence.
[<ExcludeFromCodeCoverage>]
let inline toSeq<'T> (slice: 'T slice) = Seq.readonly slice

/// Returns the Span that references the same range as given slice.
[<ExcludeFromCodeCoverage>]
let inline asSpan<'T> (slice: 'T slice) = slice.AsSpan()

/// Returns the Memory that references the same range as given slice.
[<ExcludeFromCodeCoverage>]
let inline asMemory<'T> (slice: 'T slice) = slice.AsMemory()

// ============================== mappings ==============================

type private FreeMapping<'T, 'U> = delegate of int * ReadOnlySpan<'T> -> 'U

[<ExcludeFromCodeCoverage>]
let inline private freeMap length (source: 'T slice) ([<InlineIfLambda>] mapping: FreeMapping<'T, 'U>) =
    let result = zeroCreate<'U> length
    let span = source.AsSpan()
    let resultSpan = result.AsMutableSpan()
    for i in 0 .. length - 1 do
        resultSpan[i] <- mapping.Invoke(i, span)
    result

type private FreeMapping2<'T1, 'T2, 'U> = delegate of int * ReadOnlySpan<'T1> * ReadOnlySpan<'T2> -> 'U

[<ExcludeFromCodeCoverage>]
let inline private freeMap2
    length
    (source1: 'T1 slice)
    (source2: 'T2 slice)
    ([<InlineIfLambda>] mapping: FreeMapping2<'T1, 'T2, 'U>)
    =
    let result = zeroCreate<'U> length
    let span1 = source1.AsSpan()
    let span2 = source2.AsSpan()
    let resultSpan = result.AsMutableSpan()
    for i in 0 .. length - 1 do
        resultSpan[i] <- mapping.Invoke(i, span1, span2)
    result

type private FreeMapping3<'T1, 'T2, 'T3, 'U> =
    delegate of int * ReadOnlySpan<'T1> * ReadOnlySpan<'T2> * ReadOnlySpan<'T3> -> 'U

[<ExcludeFromCodeCoverage>]
let inline private freeMap3
    length
    (source1: 'T1 slice)
    (source2: 'T2 slice)
    (source3: 'T3 slice)
    ([<InlineIfLambda>] mapping: FreeMapping3<'T1, 'T2, 'T3, 'U>)
    =
    let result = zeroCreate<'U> length
    let span1 = source1.AsSpan()
    let span2 = source2.AsSpan()
    let span3 = source3.AsSpan()
    let resultSpan = result.AsMutableSpan()
    for i in 0 .. length - 1 do
        resultSpan[i] <- mapping.Invoke(i, span1, span2, span3)
    result

[<ExcludeFromCodeCoverage>]
let inline private bind2Core ([<InlineIfLambda>] mapping: 'T1 -> 'T2 -> 'U) (slice1: 'T1 slice) (slice2: 'T2 slice) =
    let mutable result = create (slice1.Length * slice2.Length)
    for x1 in slice1 do
        for x2 in slice2 do
            result <- add (mapping x1 x2) result
    result

/// Builds a new slice whose elements are the results of applying the given function to each of the elements of the slice.
let map (mapping: 'T -> 'U) (slice: 'T slice) : 'U slice =
    if isEmpty slice then
        empty
    else
        freeMap slice.Length slice (FreeMapping(fun i span -> mapping span[i]))

/// Builds a new slice whose elements are the results of applying the given function to each of the elements of the slice.
/// The integer index passed to the function indicates the index (from 0) of element being transformed.
let mapi (mapping: int -> 'T -> 'U) (slice: 'T slice) : 'U slice =
    if isEmpty slice then
        empty
    else
        let f = OptimizedClosures.FSharpFunc<_, _, _>.Adapt(mapping)
        freeMap slice.Length slice (FreeMapping(fun i span -> f.Invoke(i, span[i])))

/// Builds a new slice whose elements are the results of applying the given function to the corresponding elements of the two slices pairwise.
let map2 (mapping: 'T1 -> 'T2 -> 'U) (slice1: 'T1 slice) (slice2: 'T2 slice) : 'U slice =
    if isEmpty slice1 || isEmpty slice2 then
        empty
    else
        let f = OptimizedClosures.FSharpFunc<_, _, _>.Adapt(mapping)
        let length = min slice1.Length slice2.Length
        freeMap2 length slice1 slice2 (FreeMapping2(fun i span1 span2 -> f.Invoke(span1[i], span2[i])))

/// Builds a new slice whose elements are the results of applying the given function to the corresponding elements of the two slices pairwise.
/// The integer index passed to the function indicates the index (from 0) of element being transformed.
let mapi2 (mapping: int -> 'T1 -> 'T2 -> 'U) (slice1: 'T1 slice) (slice2: 'T2 slice) : 'U slice =
    if isEmpty slice1 || isEmpty slice2 then
        empty
    else
        let f = OptimizedClosures.FSharpFunc<_, _, _, _>.Adapt(mapping)
        let length = min slice1.Length slice2.Length
        freeMap2 length slice1 slice2 (FreeMapping2(fun i span1 span2 -> f.Invoke(i, span1[i], span2[i])))

/// Returns a new slice that contains all pairings of elements from two slices.
let allPairs (slice1: 'T1 slice) (slice2: 'T2 slice) : ('T1 * 'T2) slice =
    if isEmpty slice1 || isEmpty slice2 then
        empty
    else
        bind2Core (fun a b -> (a, b)) slice1 slice2

/// Returns a new slice that contains all pairings of elements from two slices.
let vAllPairs (slice1: 'T1 slice) (slice2: 'T2 slice) : struct ('T1 * 'T2) slice =
    if isEmpty slice1 || isEmpty slice2 then
        empty
    else
        bind2Core (fun a b -> (a, b)) slice1 slice2

/// Returns a new slice whose elements are the corresponding elements of the input slice paired with the index (from 0) of each element.
let indexed (slice: 'T slice) : (int * 'T) slice = freeMap slice.Length slice (FreeMapping(fun i span -> (i, span[i])))

/// Returns a new slice whose elements are the corresponding elements of the input slice paired with the index (from 0) of each element.
let vIndexed (slice: 'T slice) : struct (int * 'T) slice =
    freeMap slice.Length slice (FreeMapping(fun i span -> (i, span[i])))

/// Splits a slice of pairs into two slice.
let unzip (slice: ('T1 * 'T2) slice) : 'T1 slice * 'T2 slice =
    let result1 = zeroCreate<'T1> slice.Length
    let result2 = zeroCreate<'T2> slice.Length
    let resultSpan1 = result1.AsMutableSpan()
    let resultSpan2 = result2.AsMutableSpan()
    let span = slice.AsSpan()
    for i = 0 to span.Length - 1 do
        let x, y = span[i]
        resultSpan1[i] <- x
        resultSpan2[i] <- y
    result1, result2

/// Splits a slice of pairs into two slice.
let vUnzip (slice: struct ('T1 * 'T2) slice) : struct ('T1 slice * 'T2 slice) =
    let result1 = zeroCreate<'T1> slice.Length
    let result2 = zeroCreate<'T2> slice.Length
    let resultSpan1 = result1.AsMutableSpan()
    let resultSpan2 = result2.AsMutableSpan()
    let span = slice.AsSpan()
    for i = 0 to span.Length - 1 do
        let struct (x, y) = span[i]
        resultSpan1[i] <- x
        resultSpan2[i] <- y
    result1, result2

/// Splits a slice of triples into three slice.
let unzip3 (slice: ('T1 * 'T2 * 'T3) slice) : 'T1 slice * 'T2 slice * 'T3 slice =
    let result1 = zeroCreate<'T1> slice.Length
    let result2 = zeroCreate<'T2> slice.Length
    let result3 = zeroCreate<'T3> slice.Length
    let resultSpan1 = result1.AsMutableSpan()
    let resultSpan2 = result2.AsMutableSpan()
    let resultSpan3 = result3.AsMutableSpan()
    let span = slice.AsSpan()
    for i = 0 to span.Length - 1 do
        let x, y, z = span[i]
        resultSpan1[i] <- x
        resultSpan2[i] <- y
        resultSpan3[i] <- z
    result1, result2, result3

/// Splits a slice of triples into three slice.
let vUnzip3 (slice: struct ('T1 * 'T2 * 'T3) slice) : struct ('T1 slice * 'T2 slice * 'T3 slice) =
    let result1 = zeroCreate<'T1> slice.Length
    let result2 = zeroCreate<'T2> slice.Length
    let result3 = zeroCreate<'T3> slice.Length
    let resultSpan1 = result1.AsMutableSpan()
    let resultSpan2 = result2.AsMutableSpan()
    let resultSpan3 = result3.AsMutableSpan()
    let span = slice.AsSpan()
    for i = 0 to span.Length - 1 do
        let struct (x, y, z) = span[i]
        resultSpan1[i] <- x
        resultSpan2[i] <- y
        resultSpan3[i] <- z
    result1, result2, result3

/// Combines the two slices into a slice of pairs.
/// If one slice is shorter than the other then the remaining elements of the longer slice are ignored.
let zip (slice1: 'T1 slice) (slice2: 'T2 slice) : ('T1 * 'T2) slice =
    if isEmpty slice1 || isEmpty slice2 then
        empty
    else
        let length = Operators.min slice1.Length slice2.Length
        freeMap2 length slice1 slice2 (FreeMapping2(fun i span1 span2 -> span1[i], span2[i]))

/// Combines the two slices into a slice of pairs.
/// If one slice is shorter than the other then the remaining elements of the longer slice are ignored.
let vZip (slice1: 'T1 slice) (slice2: 'T2 slice) : struct ('T1 * 'T2) slice =
    if isEmpty slice1 || isEmpty slice2 then
        empty
    else
        let length = Operators.min slice1.Length slice2.Length
        freeMap2 length slice1 slice2 (FreeMapping2(fun i span1 span2 -> span1[i], span2[i]))

/// Combines the three slices into a slice of triples.
/// If one slice is shorter than the others then the remaining elements of the longer slices are ignored.
let zip3 (slice1: 'T1 slice) (slice2: 'T2 slice) (slice3: 'T3 slice) : ('T1 * 'T2 * 'T3) slice =
    if isEmpty slice1 || isEmpty slice2 || isEmpty slice3 then
        empty
    else
        let length = Operators.min slice1.Length slice2.Length |> Operators.min slice3.Length
        freeMap3 length slice1 slice2 slice3 (FreeMapping3(fun i span1 span2 span3 -> span1[i], span2[i], span3[i]))

/// Combines the three slices into a slice of triples.
/// If one slice is shorter than the others then the remaining elements of the longer slices are ignored.
let vZip3 (slice1: 'T1 slice) (slice2: 'T2 slice) (slice3: 'T3 slice) : struct ('T1 * 'T2 * 'T3) slice =
    if isEmpty slice1 || isEmpty slice2 || isEmpty slice3 then
        empty
    else
        let length = Operators.min slice1.Length slice2.Length |> Operators.min slice3.Length
        freeMap3 length slice1 slice2 slice3 (FreeMapping3(fun i span1 span2 span3 -> span1[i], span2[i], span3[i]))

// ============================== filterings ==============================

/// Returns a new slice containing only the elements of the slice for which the given predicate returns "true"
let filter (predicate: 'T -> bool) (slice: 'T slice) : 'T slice =
    if isEmpty slice then
        empty
    else
        let mutable result = createDefaultCap<'T> slice.Length
        for x in slice do
            if predicate x then
                result <- add x result
        result

/// Returns a new slice containing only the elements of the slice for which the given predicate returns "true"
[<ExcludeFromCodeCoverage>]
let inline where predicate slice : 'T slice = filter predicate slice

/// Applies a function to each element in a slice and then returns a slice of values v where the applied function returned Some(v).
/// Returns an empty slice when the input slice is empty or when the applied chooser function returns None for all elements.
let choose (chooser: 'T -> 'U option) (slice: 'T slice) : 'U slice =
    let mutable result = create<'U> slice.Length
    for x in slice do
        match chooser x with
        | Some(v) -> result <- add v result
        | None -> ()
    result

/// Applies a function to each element in a slice and then returns a slice of values v where the applied function returned ValueSome(v).
/// Returns an empty slice when the input slice is empty or when the applied chooser function returns ValueNone for all elements.
let vChoose (chooser: 'T -> 'U voption) (slice: 'T slice) : 'U slice =
    let mutable result = create<'U> slice.Length
    for x in slice do
        match chooser x with
        | ValueSome(v) -> result <- add v result
        | ValueNone -> ()
    result

/// Returns the slice after removing the first N elements. Returns an empty slice when the input slice is shorter than N.
[<ExcludeFromCodeCoverage>]
let inline skip count (slice: 'T slice) : 'T slice = slice[count..]

/// Returns the first N elements of the slice. Returns an empty slice when the input slice is shorter than N.
[<ExcludeFromCodeCoverage>]
let inline take count (slice: 'T slice) : 'T slice = slice[..count]

/// Returns the first N elements of the slice. Returns an empty slice when the input slice is shorter than N.
[<ExcludeFromCodeCoverage>]
let inline truncate count (slice: 'T slice) : 'T slice = slice[..count]

/// Returns a slice that contains all elements of the original slice while the given predicate returns True, and then returns no further elements.
let takeWhile (predicate: 'T -> bool) (slice: 'T slice) : 'T slice =
    let mutable index = 0
    let span = slice.AsSpan()
    while index < span.Length && predicate span[index] do
        index <- index + 1
    slice[..index]

/// Bypasses elements in a slice while the given predicate returns True, and then returns the remaining elements of the slice.
let skipWhile (predicate: 'T -> bool) (slice: 'T slice) : 'T slice =
    let mutable index = 0
    let span = slice.AsSpan()
    while index < span.Length && predicate span[index] do
        index <- index + 1
    slice[index..]

/// Returns the slice after removing the first element.
let tail (slice: 'T slice) : 'T slice =
    if isEmpty slice then
        raiseEmpty (nameof slice)
    else
        slice[1..]

/// Splits the slice into two slices, containing the elements for which the given predicate returns True and False respectively.
/// Element order is preserved in both of the created slices.
let vPartition (predicate: 'T -> bool) (slice: 'T slice) : struct ('T slice * 'T slice) =
    if isEmpty slice then
        empty, empty
    else
        let mutable result1 = create<'T> slice.Length
        let mutable result2 = create<'T> slice.Length
        for x in slice do
            if predicate x then
                result1 <- add x result1
            else
                result2 <- add x result2
        result1, result2

/// Splits the slice into two slices, containing the elements for which the given predicate returns True and False respectively.
/// Element order is preserved in both of the created slices.
let partition (predicate: 'T -> bool) (slice: 'T slice) : 'T slice * 'T slice =
    let struct (x, y) = vPartition predicate slice
    x, y

/// Splits a slice into two slices, at the given index.
let splitAt index (slice: 'T slice) : 'T slice * 'T slice = slice[..index], slice[index..]

/// Splits a slice into two slices, at the given index.
let vSplitAt index (slice: 'T slice) : struct ('T slice * 'T slice) = slice[..index], slice[index..]

// ============================== concats ==============================

/// Returns a new slice that contains the elements of the first slice followed by elements of the second slice.
[<ExcludeFromCodeCoverage>]
let inline append (slice1: 'T slice) (slice2: 'T slice) : 'T slice = addSpan (slice2.AsSpan()) slice1

/// For each element of the slice, applies the given function. Concatenates all the results and return the combined slice.
let collect (mapping: 'T -> 'U slice) (slice: 'T slice) : 'U slice =
    let mutable result = createDefaultCap<'U> slice.Length
    for x in slice do
        result <- append result (mapping x)
    result

/// Returns a new slice that contains the elements of each the slices in order.
let concat (slices: seq<'T slice>) : 'T slice =
    if isNull slices then
        empty
    else
        let mutable result = empty<'T>
        for slice in slices do
            result <- append result slice
        result

// ============================== foldings ==============================

/// Applies a function to each element of the slice, threading an accumulator argument through the computation.
/// Take the second argument, and apply the function to it and the first element of the slice.
/// Then feed this result into the function along with the second element and so on.
/// Return the final result. If the input function is f and the elements are i0...iN then computes f (... (f s i0) i1 ...) iN.
let fold (folder: 'State -> 'T -> 'State) (state: 'State) (slice: 'T slice) =
    if isEmpty slice then
        state
    else
        let f = OptimizedClosures.FSharpFunc<_, _, _>.Adapt(folder)
        let mutable acc = state
        for x in slice do
            acc <- f.Invoke(acc, x)
        acc

/// Applies a function to corresponding elements of two slices, threading an accumulator argument through the computation.
/// If one slice is shorter than the other then the remaining elements of the longer slice are ignored.
/// If the input function is f and the elements are i0...iN and j0...jN then computes f (... (f s i0 j0)...) iN jN.
let fold2 (folder: 'State -> 'T1 -> 'T2 -> 'State) (state: 'State) (slice1: 'T1 slice) (slice2: 'T2 slice) =
    if isEmpty slice1 || isEmpty slice2 then
        state
    else
        let f = OptimizedClosures.FSharpFunc<_, _, _, _>.Adapt(folder)
        let mutable acc = state
        let length = min slice1.Length slice2.Length
        let slice1Span = slice1.AsSpan()
        let slice2Span = slice2.AsSpan()
        for i in 0 .. length - 1 do
            acc <- f.Invoke(acc, slice1Span[i], slice2Span[i])
        acc

/// Applies a function to each element of the slice, starting from the end, threading an accumulator argument through the computation.
/// If the input function is f and the elements are i0...iN then computes f i0 (...(f iN s)).
let foldBack (folder: 'T -> 'State -> 'State) (slice: 'T slice) (state: 'State) =
    if isEmpty slice then
        state
    else
        let f = OptimizedClosures.FSharpFunc<_, _, _>.Adapt(folder)
        let mutable acc = state
        let sliceSpan = slice.AsSpan()
        for i = sliceSpan.Length - 1 downto 0 do
            acc <- f.Invoke(sliceSpan[i], acc)
        acc

/// Applies a function to corresponding elements of two slices, threading an accumulator argument through the computation.
/// If one slice is shorter than the other then the remaining elements of the longer slice are ignored.
/// If the input function is f and the elements are i0...iN and j0...jN then computes f i0 j0 (...(f iN jN s)).
let foldBack2 (folder: 'T1 -> 'T2 -> 'State -> 'State) (slice1: 'T1 slice) (slice2: 'T2 slice) (state: 'State) =
    if isEmpty slice1 || isEmpty slice2 then
        state
    else
        let f = OptimizedClosures.FSharpFunc<_, _, _, _>.Adapt(folder)
        let mutable acc = state
        let length = min slice1.Length slice2.Length
        let span1 = slice1.AsSpan()
        let span2 = slice2.AsSpan()
        for i = length - 1 downto 0 do
            acc <- f.Invoke(span1[i], span2[i], acc)
        acc

/// Applies a function to each element of the slice, threading an accumulator argument through the computation.
/// Take the second argument, and apply the function to it and the first element of the slice.
/// Then feed this result into the function along with the second element and so on. Returns the slice of intermediate results and the final result.
let scan (folder: 'State -> 'T -> 'State) (state: 'State) (slice: 'T slice) : 'State slice =
    if isEmpty slice then
        empty
    else
        let f = OptimizedClosures.FSharpFunc<_, _, _>.Adapt(folder)
        let mutable acc = state
        freeMap
            (slice.Length + 1)
            slice
            (FreeMapping(fun i span ->
                if i > 0 then
                    acc <- f.Invoke(acc, span[i - 1])
                acc
            ))

/// Like foldBack, but returns both the intermediary and final results.
let scanBack (folder: 'T -> 'State -> 'State) (slice: 'T slice) (state: 'State) : 'State slice =
    if isEmpty slice then
        empty
    else
        let f = OptimizedClosures.FSharpFunc<_, _, _>.Adapt(folder)
        let mutable acc = state
        let result = zeroCreate slice.Length
        let span = slice.AsSpan()
        let resultSpan = result.AsMutableSpan()
        for i = span.Length - 1 downto 0 do
            acc <- f.Invoke(span[i], acc)
            resultSpan[i] <- acc
        result

/// Combines map and fold.
/// Builds a new slice whose elements are the results of applying the given function to each of the elements of the input slice.
/// The function is also used to accumulate a final value.
let mapFold (folder: 'State -> 'T -> 'Result * 'State) (state: 'State) (slice: 'T slice) : 'Result slice * 'State =
    if isEmpty slice then
        empty, state
    else
        let f = OptimizedClosures.FSharpFunc<_, _, _>.Adapt(folder)
        let mutable acc = state
        let span = slice.AsSpan()
        let result = zeroCreate<'Result> slice.Length
        let resultSpan = result.AsMutableSpan()
        for i in 0 .. span.Length - 1 do
            let item, a = f.Invoke(acc, span[i])
            resultSpan[i] <- item
            acc <- a
        result, acc

/// Combines map and fold.
/// Builds a new slice whose elements are the results of applying the given function to each of the elements of the input slice.
/// The function is also used to accumulate a final value.
let vMapFold
    (folder: 'State -> 'T -> struct ('Result * 'State))
    (state: 'State)
    (slice: 'T slice)
    : struct ('Result slice * 'State) =
    if isEmpty slice then
        empty, state
    else
        let f = OptimizedClosures.FSharpFunc<_, _, _>.Adapt(folder)
        let mutable acc = state
        let span = slice.AsSpan()
        let result = zeroCreate<'Result> slice.Length
        let resultSpan = result.AsMutableSpan()
        for i in 0 .. span.Length - 1 do
            let struct (item, a) = f.Invoke(acc, span[i])
            resultSpan[i] <- item
            acc <- a
        result, acc

/// Combines map and foldBack.
/// Builds a new slice whose elements are the results of applying the given function to each of the elements of the input slice.
/// The function is also used to accumulate a final value.
let mapFoldBack (folder: 'T -> 'State -> 'Result * 'State) (slice: 'T slice) (state: 'State) : 'Result slice * 'State =
    if isEmpty slice then
        empty, state
    else
        let f = OptimizedClosures.FSharpFunc<_, _, _>.Adapt(folder)
        let mutable acc = state
        let span = slice.AsSpan()
        let result = zeroCreate<'Result> slice.Length
        let resultSpan = result.AsMutableSpan()
        for i = span.Length - 1 downto 0 do
            let item, a = f.Invoke(span[i], acc)
            resultSpan[i] <- item
            acc <- a
        result, acc

/// Combines map and foldBack.
/// Builds a new slice whose elements are the results of applying the given function to each of the elements of the input slice.
/// The function is also used to accumulate a final value.
let vMapFoldBack
    (folder: 'T -> 'State -> struct ('Result * 'State))
    (slice: 'T slice)
    (state: 'State)
    : struct ('Result slice * 'State) =
    if isEmpty slice then
        empty, state
    else
        let f = OptimizedClosures.FSharpFunc<_, _, _>.Adapt(folder)
        let mutable acc = state
        let span = slice.AsSpan()
        let result = zeroCreate<'Result> slice.Length
        let resultSpan = result.AsMutableSpan()
        for i = span.Length - 1 downto 0 do
            let struct (item, a) = f.Invoke(span[i], acc)
            resultSpan[i] <- item
            acc <- a
        result, acc

/// Apply a function to each element of the slice, threading an accumulator argument through the computation.
/// Apply the function to the first two elements of the slice.
/// Then feed this result into the function along with the third element and so on.
/// Return the final result.
/// If the input function is f and the elements are i0...iN then computes f (... (f i0 i1) i2 ...) iN.
let reduce reduction (slice: 'T slice) =
    if isEmpty slice then
        raiseEmpty (nameof slice)
    else
        fold reduction slice[0] slice[1..]

/// Applies a function to each element of the slice, starting from the end, threading an accumulator argument through the computation.
/// If the input function is f and the elements are i0...iN then computes f i0 (...(f iN-1 iN)).
let reduceBack reduction (slice: 'T slice) =
    if isEmpty slice then
        raiseEmpty (nameof slice)
    else
        foldBack reduction slice[.. slice.Length - 1] slice[slice.Length - 1]

// ============================== aggregations ==============================

/// Abstraction of min and max.
let top (isRightChosen: 'T -> 'T -> bool) (slice: 'T slice) =
    match slice.Length with
    | 0 -> raiseEmpty (nameof slice)
    | 1 -> slice[0]
    | _ ->
        let f = OptimizedClosures.FSharpFunc<_, _, _>.Adapt(isRightChosen)
        let mutable acc = slice[0]
        for x in slice[1..] do
            if f.Invoke(acc, x) then
                acc <- x
        acc

/// Abstraction of minBy and maxBy.
let topBy (projection: 'T -> 'U) (isRightChosen: 'U -> 'U -> bool) (slice: 'T slice) =
    match slice.Length with
    | 0 -> raiseEmpty (nameof slice)
    | 1 -> slice[0]
    | _ ->
        let f = OptimizedClosures.FSharpFunc<_, _, _>.Adapt(isRightChosen)
        let mutable accKey = projection slice[0]
        let mutable accValue = slice[0]
        for x in slice[1..] do
            let key = projection x
            if f.Invoke(accKey, key) then
                accKey <- key
                accValue <- x
        accValue

/// Return the greatest of all elements of the non-empty slice, compared via Operators.max.
[<ExcludeFromCodeCoverage>]
let inline max (slice: 'T slice) = top (<) slice

/// Returns the greatest of all elements of the non-empty slice, compared via Operators.max on the function result.
[<ExcludeFromCodeCoverage>]
let inline maxBy (projection: 'T -> 'U) (slice: 'T slice) = topBy projection (<) slice

/// Return the lowest of all elements of the non-empty slice, compared via Operators.min.
[<ExcludeFromCodeCoverage>]
let inline min (slice: 'T slice) = top (>) slice

/// Returns the lowest of all elements of the non-empty slice, compared via Operators.min on the function result.
[<ExcludeFromCodeCoverage>]
let inline minBy (projection: 'T -> 'U) (slice: 'T slice) = topBy projection (>) slice

/// Returns the sum of the elements in the slice.
[<ExcludeFromCodeCoverage>]
let inline sum (slice: 'T slice) = fold Checked.(+) LanguagePrimitives.GenericZero<'T> slice

/// Returns the sum of the results generated by applying the function to each element of the slice.
[<ExcludeFromCodeCoverage>]
let inline sumBy (projection: 'T -> 'U) (slice: 'T slice) =
    fold (Checked.(+) >> (>>) projection) LanguagePrimitives.GenericZero<'U> slice

/// Returns the average of the values in a non-empty slice.
[<ExcludeFromCodeCoverage>]
let inline average (slice: 'T slice) =
    if isEmpty slice then
        invalidArg (nameof slice) "The input sequence was empty."
    else
        let sum = sum slice
        LanguagePrimitives.DivideByInt sum slice.Length

/// Returns the average of values in a slice generated by applying a function to each element of the slice.
[<ExcludeFromCodeCoverage>]
let inline averageBy (projection: 'T -> 'U) (slice: 'T slice) =
    if isEmpty slice then
        invalidArg (nameof slice) "The input sequence was empty."
    else
        let sum = sumBy projection slice
        LanguagePrimitives.DivideByInt sum slice.Length

// ============================== item fetchings ==============================

/// Returns the only element of the slice.
let exactlyOne (slice: 'T slice) =
    match slice.Length with
    | 1 -> slice[0]
    | 0 -> raiseEmpty (nameof slice)
    | _ -> raiseMoreThanOneElement (nameof slice)

/// Returns the only element of the slice or None if it is empty or contains more than one element.
let tryExactlyOne (slice: 'T slice) =
    match slice.Length with
    | 1 -> Some(slice[0])
    | _ -> None

/// Returns the only element of the slice or ValueNone if it is empty or contains more than one element.
let vTryExactlyOne (slice: 'T slice) =
    match slice.Length with
    | 1 -> ValueSome(slice[0])
    | _ -> ValueNone

/// Returns the first element for which the given function returns True. Raises KeyNotFoundException if no such element exists.
let find (predicate: 'T -> bool) (slice: 'T slice) =
    let rec loop index =
        if index >= slice.Length then raiseNotFound ()
        elif predicate slice[index] then slice[index]
        else loop (index + 1)
    loop 0

/// Returns the first element for which the given function returns True. Return None if no such element exists.
let tryFind (predicate: 'T -> bool) (slice: 'T slice) =
    let rec loop index =
        if index >= slice.Length then None
        elif predicate slice[index] then Some(slice[index])
        else loop (index + 1)
    loop 0

/// Returns the first element for which the given function returns True. Return ValueNone if no such element exists.
let vTryFind (predicate: 'T -> bool) (slice: 'T slice) =
    let rec loop index =
        if index >= slice.Length then ValueNone
        elif predicate slice[index] then ValueSome(slice[index])
        else loop (index + 1)
    loop 0

/// Returns the last element for which the given function returns True. Raises KeyNotFoundException if no such element exists.
let findBack (predicate: 'T -> bool) (slice: 'T slice) =
    let rec loop index =
        if index < 0 then raiseNotFound ()
        elif predicate slice[index] then slice[index]
        else loop (index - 1)
    loop (slice.Length - 1)

/// Returns the last element for which the given function returns True. Return None if no such element exists.
let tryFindBack (predicate: 'T -> bool) (slice: 'T slice) =
    let rec loop index =
        if index < 0 then None
        elif predicate slice[index] then Some(slice[index])
        else loop (index - 1)
    loop (slice.Length - 1)

/// Returns the last element for which the given function returns True. Return ValueNone if no such element exists.
let vTryFindBack (predicate: 'T -> bool) (slice: 'T slice) =
    let rec loop index =
        if index < 0 then ValueNone
        elif predicate slice[index] then ValueSome(slice[index])
        else loop (index - 1)
    loop (slice.Length - 1)

/// Returns the index of first element for which the given function returns True. Raises KeyNotFoundException if no such element exists.
let findIndex (predicate: 'T -> bool) (slice: 'T slice) =
    let rec loop index =
        if index >= slice.Length then raiseNotFound ()
        elif predicate slice[index] then index
        else loop (index + 1)
    loop 0

/// Returns the index of the first element for which the given function returns True. Return None if no such element exists.
let tryFindIndex (predicate: 'T -> bool) (slice: 'T slice) =
    let rec loop index =
        if index >= slice.Length then None
        elif predicate slice[index] then Some(index)
        else loop (index + 1)
    loop 0

/// Returns the index of the first element for which the given function returns True. Return ValueNone if no such element exists.
let vTryFindIndex (predicate: 'T -> bool) (slice: 'T slice) =
    let rec loop index =
        if index >= slice.Length then ValueNone
        elif predicate slice[index] then ValueSome(index)
        else loop (index + 1)
    loop 0

/// Returns the index of last element for which the given function returns True. Raises KeyNotFoundException if no such element exists.
let findIndexBack (predicate: 'T -> bool) (slice: 'T slice) =
    let rec loop index =
        if index < 0 then raiseNotFound ()
        elif predicate slice[index] then index
        else loop (index - 1)
    loop (slice.Length - 1)

/// Returns the index of the last element for which the given function returns True. Return None if no such element exists.
let tryFindIndexBack (predicate: 'T -> bool) (slice: 'T slice) =
    let rec loop index =
        if index < 0 then None
        elif predicate slice[index] then Some(index)
        else loop (index - 1)
    loop (slice.Length - 1)

/// Returns the index of the last element for which the given function returns True. Return ValueNone if no such element exists.
let vTryFindIndexBack (predicate: 'T -> bool) (slice: 'T slice) =
    let rec loop index =
        if index < 0 then ValueNone
        elif predicate slice[index] then ValueSome(index)
        else loop (index - 1)
    loop (slice.Length - 1)

/// Returns the first element of the non-empty slice.
let head (slice: 'T slice) =
    if isEmpty slice then
        raiseEmpty (nameof slice)
    else
        slice[0]

/// Returns the first element of the slice, or None if the slice is empty.
let tryHead (slice: 'T slice) = if isEmpty slice then None else Some(slice[0])

/// Returns the first element of the slice, or ValueNone if the slice is empty.
let vTryHead (slice: 'T slice) = if isEmpty slice then ValueNone else ValueSome(slice[0])

/// Returns the last element of the non-empty slice.
let last (slice: 'T slice) =
    if isEmpty slice then
        raiseEmpty (nameof slice)
    else
        slice[slice.Length - 1]

/// Returns the last element of the slice, or None if the slice is empty.
let tryLast (slice: 'T slice) =
    if isEmpty slice then
        None
    else
        Some(slice[slice.Length - 1])

/// Returns the last element of the slice, or ValueNone if the slice is empty.
let vTryLast (slice: 'T slice) =
    if isEmpty slice then
        ValueNone
    else
        ValueSome(slice[slice.Length - 1])

/// Indexes into the slice. The first element has index 0.
[<ExcludeFromCodeCoverage>]
let inline item index (slice: 'T slice) = slice[index]

/// Tries to find the nth element in the slice. Returns None if index is negative or the slice does not contain enough elements.
let tryItem index (slice: 'T slice) =
    if 0 <= index && index < slice.Length then
        Some(slice[index])
    else
        None

/// Tries to find the nth element in the slice. Returns ValueNone if index is negative or the slice does not contain enough elements.
let vTryItem index (slice: 'T slice) =
    if 0 <= index && index < slice.Length then
        ValueSome(slice[index])
    else
        ValueNone

/// Applies the given function to successive elements, returning the first result where function returns Some(x) for some x.
/// If no such element exists then raise KeyNotFoundException.
let pick (chooser: 'T -> 'U voption) (slice: 'T slice) =
    let rec loop index =
        if index >= slice.Length then
            raiseNotFound ()
        else
            match chooser slice[index] with
            | ValueSome(x) -> x
            | ValueNone -> loop (index + 1)
    loop 0

/// Applies the given function to successive elements, returning Some(x) the first result where function returns Some(x) for some x.
/// If no such element exists then return None.
let tryPick (chooser: 'T -> 'U option) (slice: 'T slice) =
    let rec loop index =
        if index >= slice.Length then
            None
        else
            match chooser slice[index] with
            | Some(x) -> Some(x)
            | None -> loop (index + 1)
    loop 0

/// Applies the given function to successive elements, returning ValueSome(x) the first result where function returns ValueSome(x) for some x.
/// If no such element exists then return ValueNone.
let vTryPick (chooser: 'T -> 'U voption) (slice: 'T slice) =
    let rec loop index =
        if index >= slice.Length then
            ValueNone
        else
            match chooser slice[index] with
            | ValueSome(x) -> ValueSome(x)
            | ValueNone -> loop (index + 1)
    loop 0

// ============================== check ==============================

/// Tests if the slice contains the specified element.
let contains (value: 'T) (source: 'T slice) =
    let rec loop index =
        if index >= source.Length then false
        elif source[index] = value then true
        else loop (index + 1)
    loop 0

/// Tests if any element of the slice satisfies the given predicate.
let exists (predicate: 'T -> bool) (slice: 'T slice) =
    let rec loop index =
        if index >= slice.Length then false
        elif predicate slice[index] then true
        else loop (index + 1)
    loop 0

/// Tests if any pair of corresponding elements of the slices satisfies the given predicate.
/// If one slice is shorter than the other then the remaining elements of the longer slice are ignored.
let exists2 (predicate: 'T1 -> 'T2 -> bool) (slice1: 'T1 slice) (slice2: 'T2 slice) =
    let length = Operators.min slice1.Length slice2.Length
    let f = OptimizedClosures.FSharpFunc<_, _, _>.Adapt(predicate)
    let rec loop index =
        if index >= length then false
        elif f.Invoke(slice1[index], slice2[index]) then true
        else loop (index + 1)
    loop 0

/// Tests if all elements of the slice satisfy the given predicate.
/// The predicate is applied to the elements of the input slice.
/// If any application returns false then the overall result is false and no further elements are tested.
/// Otherwise, true is returned.
let forall (predicate: 'T -> bool) (slice: 'T slice) =
    let rec loop index =
        if index >= slice.Length then true
        elif not (predicate slice[index]) then false
        else loop (index + 1)
    loop 0

/// Tests if all corresponding elements of the slice satisfy the given predicate pairwise.
/// The predicate is applied to matching elements in the two slices up to the lesser of the two lengths of the slices.
/// If any application returns false then the overall result is false and no further elements are tested. Otherwise, true is returned.
let forall2 (predicate: 'T1 -> 'T2 -> bool) (slice1: 'T1 slice) (slice2: 'T2 slice) =
    let length = Operators.min slice1.Length slice2.Length
    let f = OptimizedClosures.FSharpFunc<_, _, _>.Adapt(predicate)
    let rec loop index =
        if index >= length then true
        elif not (f.Invoke(slice1[index], slice2[index])) then false
        else loop (index + 1)
    loop 0

/// Compares two slices using the given comparison function, element by element.
let compareWith (comparer: 'T -> 'T -> int) (slice1: 'T slice) (slice2: 'T slice) =
    let f = OptimizedClosures.FSharpFunc<_, _, _>.Adapt(comparer)
    let rec loop (slice1: 'T slice) (slice2: 'T slice) index =
        if slice1.Length > index && slice2.Length > index then
            let c = f.Invoke(slice1[index], slice2[index])
            if c = 0 then loop slice1 slice2 (index + 1) else c
        elif slice1.Length > slice2.Length then
            1
        elif slice1.Length < slice2.Length then
            -1
        else
            0
    loop slice1 slice2 0

// ============================== sortings ==============================

/// Sorts the given slice using the given comparison function.
let sortWith (comparer: 'T -> 'T -> int) (slice: 'T slice) : 'T slice =
    let result = copy slice
    array.Sort(result.Back.Field, Comparison(comparer))
    result

/// Sorts the given slice using Operators.compare.
let sort (slice: 'T slice) : 'T slice = sortWith compare slice

/// Sorts the given slice using keys given by the given projection. Keys are compared using Operators.compare.
let sortBy (projection: 'T -> 'Key) (slice: 'T slice) : 'T slice =
    sortWith (fun x y -> compare (projection x) (projection y)) slice

/// Sorts the given slice in descending order using Operators.compare.
let sortDescending (slice: 'T slice) : 'T slice = sortWith (fun x y -> -compare x y) slice

/// Sorts the given slice in descending order using keys given by the given projection. Keys are compared using Operators.compare.
let sortByDescending (projection: 'T -> 'Key) (slice: 'T slice) : 'T slice =
    sortWith (fun x y -> -compare (projection x) (projection y)) slice

/// Returns a new slice with the elements in reverse order.
let rev (slice: 'T slice) : 'T slice = freeMap slice.Length slice (FreeMapping(fun i span -> span[span.Length - 1 - i]))

/// Returns a new slice with all elements permuted according to the specified permutation.
let permute (indexMap: int -> int) (slice: 'T slice) : 'T slice =
    let result = zeroCreate slice.Length
    let span = slice.AsSpan()
    let resultSpan = result.AsMutableSpan()
    for i = 0 to span.Length - 1 do
        resultSpan[indexMap i] <- span[i]
    result

// ============================== updatings ==============================

/// Return a new slice with a new item inserted before the given index.
let insertAt index (value: 'T) (source: 'T slice) : 'T slice =
    if uint source.Length < uint index then
        raiseIndexOutOfRange (nameof index)
    create<'T> (source.Length + 1)
    |> addSlice source[..index]
    |> add value
    |> addSlice source[index..]

/// Return a new slice with a new items inserted before the given index.
let insertSliceAt index (values: 'T slice) (source: 'T slice) : 'T slice =
    if uint source.Length < uint index then
        raiseIndexOutOfRange (nameof index)
    create<'T> (source.Length + values.Length)
    |> addSlice source[..index]
    |> addSlice values
    |> addSlice source[index..]

/// Return a new slice with a new items inserted before the given index.
let insertManyAt index (values: seq<'T>) (source: 'T slice) : 'T slice =
    if uint source.Length < uint index then
        raiseIndexOutOfRange (nameof index)
    create<'T> source.Length
    |> addSlice source[..index]
    |> addRange values
    |> addSlice source[index..]

/// Return a new slice with the item at a given index removed.
let removeAt index (source: 'T slice) : 'T slice =
    create<'T> source.Length |> addSlice source[..index] |> addSlice source[index + 1 ..]

/// Return a new slice with the item at a given index removed.
let removeManyAt index count (source: 'T slice) : 'T slice =
    create<'T> source.Length |> addSlice source[..index] |> addSlice source[index + count ..]

/// Return a new slice with the item at a given index set to the new value.
let updateAt index value (source: 'T slice) : 'T slice =
    if uint source.Length <= uint index then
        raiseIndexOutOfRange (nameof index)
    let result = copy source
    result.ForSet(index) <- value
    result

// ============================== others ==============================

/// Applies a key-generating function to each element of a slice and returns a slice yielding unique keys and their number of occurrences in the original slice.
let countBy (projection: 'T -> 'Key) (slice: 'T slice) : ('Key * int) slice =
    if isEmpty slice then
        empty
    else
        let dict = Dictionary(HashIdentity.Structural<'Key>)
        for x in slice do
            let key = projection x
            let ok, prev = dict.TryGetValue(key)
            dict[key] <- if ok then prev + 1 else 1
        ofDict dict

/// Applies a key-generating function to each element of a slice and returns a slice yielding unique keys and their number of occurrences in the original slice.
let vCountBy (projection: 'T -> 'Key) (slice: 'T slice) : struct ('Key * int) slice =
    if isEmpty slice then
        empty
    else
        let dict = Dictionary(HashIdentity.Structural<'Key>)
        for x in slice do
            let key = projection x
            let ok, prev = dict.TryGetValue(key)
            dict[key] <- if ok then prev + 1 else 1
        vOfDict dict

/// Applies a key-generating function to each element of a slice and yields a slice of unique keys.
/// Each unique key contains a slice of all elements that match to this key.
let groupBy (projection: 'T -> 'Key) (slice: 'T slice) : ('Key * 'T slice) slice =
    if isEmpty slice then
        empty
    else
        let dict = Dictionary(HashIdentity.Structural<'Key>)
        for x in slice do
            let key = projection x
            let _, items = dict.TryGetValue(key)
            dict[key] <- add x items
        ofDict dict

/// Applies a key-generating function to each element of a slice and yields a slice of unique keys.
/// Each unique key contains a slice of all elements that match to this key.
let vGroupBy (projection: 'T -> 'Key) (slice: 'T slice) : struct ('Key * 'T slice) slice =
    if isEmpty slice then
        empty
    else
        let dict = Dictionary(HashIdentity.Structural<'Key>)
        for x in slice do
            let key = projection x
            let _, items = dict.TryGetValue(key)
            dict[key] <- add x items
        vOfDict dict

/// Returns a slice that contains no duplicate entries according to generic hash and equality comparisons on the entries.
/// If an element occurs multiple times in the slice then the later occurrences are discarded.
let distinct (slice: 'T slice) : 'T slice =
    if isEmpty slice then
        empty
    else
        let hashSet = HashSet(HashIdentity.Structural<'T>)
        filter hashSet.Add slice

/// Returns a slice that contains no duplicate entries according to the generic hash and equality comparisons on the keys returned by the given key-generating function.
/// If an element occurs multiple times in the slice then the later occurrences are discarded.
let distinctBy (projection: 'T -> 'Key) (slice: 'T slice) : 'T slice =
    if isEmpty slice then
        empty
    else
        let hashSet = HashSet(HashIdentity.Structural<'Key>)
        let mutable result = create slice.Length
        for x in slice do
            if hashSet.Add(projection x) then
                result <- add x result
        result

/// Returns a new slice with the distinct elements of the input slice which do not appear in the itemsToExclude sequence,
/// using generic hash and equality comparisons to compare values.
let except (itemsToExclude: seq<'T>) (slice: 'T slice) : 'T slice =
    if isEmpty slice then
        empty
    else
        let cached = HashSet(itemsToExclude, HashIdentity.Structural)
        filter cached.Add slice

/// Returns a slice of each element in the input slice and its predecessor,
/// with the exception of the first element which is only returned as the predecessor of the second element.
let pairwise (slice: 'T slice) : ('T * 'T) slice =
    if slice.Length <= 1 then
        raiseNeedMoreElement (nameof slice)
    else
        freeMap (slice.Length - 1) slice (FreeMapping(fun i span -> (span[i], span[i + 1])))

/// Returns a slice of each element in the input slice and its predecessor,
/// with the exception of the first element which is only returned as the predecessor of the second element.
let vPairwise (slice: 'T slice) : struct ('T * 'T) slice =
    if slice.Length <= 1 then
        raiseNeedMoreElement (nameof slice)
    else
        freeMap (slice.Length - 1) slice (FreeMapping(fun i span -> (span[i], span[i + 1])))

/// Divides the input slice into slices (chunks) with at a positive number of at most chunkSize elements.
/// Returns a new slice containing the generated slices (chunks) as its elements. Returns an empty slice when the input slice is empty.
let chunkBySize chunkSize (slice: 'T slice) : 'T slice slice =
    if chunkSize <= 0 then
        invalidArg (nameof chunkSize) "chunkSize must be positive."
    if isEmpty slice then
        empty
    else
        let resultLength = (slice.Length + chunkSize - 1) / chunkSize
        initInline resultLength (fun i -> slice[i * chunkSize .. (i + 1) * chunkSize])

/// Returns the transpose of the given sequence of slices.
let transpose (slices: seq<'T slice>) : 'T slice slice =
    use enumerator = slices.GetEnumerator()
    if not (enumerator.MoveNext()) then
        empty
    else
        let first = enumerator.Current
        // zeroCreate によって empty が代入されるため、そのまま後続処理を行ってよい
        let result = zeroCreate<'T slice> first.Length
        let resultSpan = result.AsMutableSpan()
        let span = first.AsSpan()
        for i = 0 to span.Length - 1 do
            resultSpan[i] <- resultSpan[i] |> add span[i]
        while enumerator.MoveNext() do
            let current = enumerator.Current
            if current.Length <> first.Length then
                invalidArg (nameof slices) "The input slices must be of same length."
            let span = current.AsSpan()
            for i = 0 to span.Length - 1 do
                resultSpan[i] <- resultSpan[i] |> add span[i]
        result

/// Splits the input list into at most count chunks.
let splitInto count (slice: 'T slice) =
    if count <= 0 then
        invalidArg (nameof count) "The count must be positive."
    elif isEmpty slice then
        empty
    elif slice.Length <= count then
        initInline slice.Length (fun i -> slice[i .. i + 1])
    else
        let smallChunkSize = slice.Length / count
        let largeChunkSize = smallChunkSize + 1
        let largeChunkCount = slice.Length - smallChunkSize * count
        initInline
            count
            (fun i ->
                if i < largeChunkCount then
                    slice[i * largeChunkSize .. (i + 1) * largeChunkSize]
                else
                    slice[i * smallChunkSize + largeChunkCount .. (i + 1) * smallChunkSize + largeChunkCount]
            )

/// Returns a slice of sliding windows containing elements drawn from the input slice.
/// Each window is returned as a slice that references the original slice.
let windowed windowSize (slice: 'T slice) : 'T slice slice =
    if windowSize <= 0 then
        invalidArg (nameof windowSize) "The windowSize must be positive."
    elif isEmpty slice then
        empty
    elif slice.Length <= windowSize then
        singleton slice
    else
        initInline (slice.Length - windowSize) (fun i -> slice.Slice(i, windowSize))
