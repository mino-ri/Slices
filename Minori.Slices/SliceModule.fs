[<RequireQualifiedAccess>]
module Minori.Slices.Slice

open System
open System.Collections.Generic
open System.Diagnostics.CodeAnalysis

// ============================== exception utility ==============================

[<ExcludeFromCodeCoverage>]
let private raiseNotFound () = raise (KeyNotFoundException("An value satisfying the predicate was not found in the collection."))

[<ExcludeFromCodeCoverage>]
let private raiseEmpty (paramName: string) = invalidArg paramName "The input sequence was empty."

[<ExcludeFromCodeCoverage>]
let private raiseNeedMoreElement (paramName: string) = invalidArg paramName "The input sequence must contain two or more elements."

[<ExcludeFromCodeCoverage>]
let private raiseMoreThanOneElement (paramName: string) = invalidArg paramName "The input sequence contains more than one element."

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

[<ExcludeFromCodeCoverage>]
let inline length (slice: 'T slice) = slice.Length

[<ExcludeFromCodeCoverage>]
let inline isEmpty (slice: 'T slice) = slice.Length = 0

let copyWithCap<'T> capacity (slice: 'T slice) : 'T slice = copyCore slice.Length capacity slice

let copy<'T> (slice: 'T slice) : 'T slice = copyCore slice.Length slice.Length slice

let add<'T> item (slice: 'T slice) : 'T slice =
    let newSlice = extend 1 slice
    newSlice.ForSet(newSlice.Length - 1) <- item
    newSlice

let addSpan<'T> (items: ReadOnlySpan<'T>) (slice: 'T slice) : 'T slice =
    if items.Length = 0 then
        slice
    else
        let newSlice = extend items.Length slice
        assert (newSlice.Length = items.Length + slice.Length)
        items.CopyTo(newSlice.AsMutableSpan().Slice(slice.Length))
        newSlice

[<ExcludeFromCodeCoverage>]
let addSlice<'T> (items: 'T slice) (slice: 'T slice) = addSpan (items.AsSpan()) slice

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

[<ExcludeFromCodeCoverage>]
let inline iter ([<InlineIfLambda>] action: 'T -> unit) (slice: 'T slice) =
    for x in slice do
        action x

[<ExcludeFromCodeCoverage>]
let inline iteri ([<InlineIfLambda>] action: int -> 'T -> unit) (slice: 'T slice) =
    let mutable index = 0
    for x in slice do
        action index x
        index <- index + 1

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

let init length (initializer: int -> 'T) : 'T slice =
    let result = zeroCreate length
    let resultSpan = result.AsMutableSpan()
    for i in 0 .. length - 1 do
        resultSpan[i] <- initializer i
    result

[<ExcludeFromCodeCoverage>]
let inline empty<'T> : 'T slice = Slice<'T>()

let create<'T> capacity : 'T slice = Slice<'T>(SliceBack.alloc capacity, 0, 0)

let singleton value : 'T slice =
    let result = zeroCreate 1
    result.ForSet(0) <- value
    result

let replicate count (initial: 'T) : 'T slice =
    if count < 0 then
        invalidArg (nameof count) "count must be positive."
    initInline count (fun _ -> initial)

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

let ofSpan<'T> (span: ReadOnlySpan<'T>) : 'T slice = addSpan span (create span.Length)

let ofArray<'T> (array: 'T[]) : 'T slice = create array.Length |> addSpan (ReadOnlySpan(array))

// インライン化されるためカバレッジ対象外 (テストはある)
[<ExcludeFromCodeCoverage>]
let ofSeq<'T> (items: seq<'T>) = addRange items empty

[<ExcludeFromCodeCoverage>]
let inline toArray (slice: 'T slice) = slice.AsSpan().ToArray()

[<ExcludeFromCodeCoverage>]
let inline toSeq (slice: 'T slice) = Seq.readonly slice

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
let inline private freeMap2 length (source1: 'T1 slice) (source2: 'T2 slice) ([<InlineIfLambda>] mapping: FreeMapping2<'T1, 'T2, 'U>) =
    let result = zeroCreate<'U> length
    let span1 = source1.AsSpan()
    let span2 = source2.AsSpan()
    let resultSpan = result.AsMutableSpan()
    for i in 0 .. length - 1 do
        resultSpan[i] <- mapping.Invoke(i, span1, span2)
    result

type private FreeMapping3<'T1, 'T2, 'T3, 'U> = delegate of int * ReadOnlySpan<'T1> * ReadOnlySpan<'T2> * ReadOnlySpan<'T3> -> 'U

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

let map (mapping: 'T -> 'U) (slice: 'T slice) : 'U slice =
    if isEmpty slice then
        empty
    else
        freeMap slice.Length slice (FreeMapping(fun i span -> mapping span[i]))

let mapi (mapping: int -> 'T -> 'U) (slice: 'T slice) : 'U slice =
    if isEmpty slice then
        empty
    else
        let f = OptimizedClosures.FSharpFunc<_, _, _>.Adapt(mapping)
        freeMap slice.Length slice (FreeMapping(fun i span -> f.Invoke(i, span[i])))

let map2 (mapping: 'T1 -> 'T2 -> 'U) (slice1: 'T1 slice) (slice2: 'T2 slice) : 'U slice =
    if isEmpty slice1 || isEmpty slice2 then
        empty
    else
        let f = OptimizedClosures.FSharpFunc<_, _, _>.Adapt(mapping)
        let length = min slice1.Length slice2.Length
        freeMap2 length slice1 slice2 (FreeMapping2(fun i span1 span2 -> f.Invoke(span1[i], span2[i])))

let mapi2 (mapping: int -> 'T1 -> 'T2 -> 'U) (slice1: 'T1 slice) (slice2: 'T2 slice) : 'U slice =
    if isEmpty slice1 || isEmpty slice2 then
        empty
    else
        let f = OptimizedClosures.FSharpFunc<_, _, _, _>.Adapt(mapping)
        let length = min slice1.Length slice2.Length
        freeMap2 length slice1 slice2 (FreeMapping2(fun i span1 span2 -> f.Invoke(i, span1[i], span2[i])))

let allPairs (slice1: 'T1 slice) (slice2: 'T2 slice) : ('T1 * 'T2) slice =
    if isEmpty slice1 || isEmpty slice2 then
        empty
    else
        bind2Core (fun a b -> (a, b)) slice1 slice2

let vAllPairs (slice1: 'T1 slice) (slice2: 'T2 slice) : struct ('T1 * 'T2) slice =
    if isEmpty slice1 || isEmpty slice2 then
        empty
    else
        bind2Core (fun a b -> (a, b)) slice1 slice2

let indexed (slice: 'T slice) : (int * 'T) slice = freeMap slice.Length slice (FreeMapping(fun i span -> (i, span[i])))

let vIndexed (slice: 'T slice) : struct (int * 'T) slice = freeMap slice.Length slice (FreeMapping(fun i span -> (i, span[i])))

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

let zip (slice1: 'T1 slice) (slice2: 'T2 slice) : ('T1 * 'T2) slice =
    if isEmpty slice1 || isEmpty slice2 then
        empty
    else
        let length = Operators.min slice1.Length slice2.Length
        freeMap2 length slice1 slice2 (FreeMapping2(fun i span1 span2 -> span1[i], span2[i]))

let vZip (slice1: 'T1 slice) (slice2: 'T2 slice) : struct ('T1 * 'T2) slice =
    if isEmpty slice1 || isEmpty slice2 then
        empty
    else
        let length = Operators.min slice1.Length slice2.Length
        freeMap2 length slice1 slice2 (FreeMapping2(fun i span1 span2 -> span1[i], span2[i]))

let zip3 (slice1: 'T1 slice) (slice2: 'T2 slice) (slice3: 'T3 slice) : ('T1 * 'T2 * 'T3) slice =
    if isEmpty slice1 || isEmpty slice2 then
        empty
    else
        let length = Operators.min slice1.Length slice2.Length
        freeMap3 length slice1 slice2 slice3 (FreeMapping3(fun i span1 span2 span3 -> span1[i], span2[i], span3[i]))

let vZip3 (slice1: 'T1 slice) (slice2: 'T2 slice) (slice3: 'T3 slice) : struct ('T1 * 'T2 * 'T3) slice =
    if isEmpty slice1 || isEmpty slice2 then
        empty
    else
        let length = Operators.min slice1.Length slice2.Length
        freeMap3 length slice1 slice2 slice3 (FreeMapping3(fun i span1 span2 span3 -> span1[i], span2[i], span3[i]))

// ============================== filterings ==============================

let filter (predicate: 'T -> bool) (slice: 'T slice) : 'T slice =
    if isEmpty slice then
        empty
    else
        let mutable result = createDefaultCap<'T> slice.Length
        for x in slice do
            if predicate x then
                result <- add x result
        result

[<ExcludeFromCodeCoverage>]
let inline where predicate slice : 'T slice = filter predicate slice

let choose (chooser: 'T -> 'U option) (slice: 'T slice) : 'U slice =
    let mutable result = create<'U> slice.Length
    for x in slice do
        match chooser x with
        | Some(v) -> result <- add v result
        | None -> ()
    result

let vChoose (chooser: 'T -> 'U voption) (slice: 'T slice) : 'U slice =
    let mutable result = create<'U> slice.Length
    for x in slice do
        match chooser x with
        | ValueSome(v) -> result <- add v result
        | ValueNone -> ()
    result

[<ExcludeFromCodeCoverage>]
let inline skip count (slice: 'T slice) : 'T slice = slice[count..]

[<ExcludeFromCodeCoverage>]
let inline take count (slice: 'T slice) : 'T slice = slice[..count]

[<ExcludeFromCodeCoverage>]
let inline truncate count (slice: 'T slice) : 'T slice = slice[..count]

let takeWhihe (predicate: 'T -> bool) (slice: 'T slice) : 'T slice =
    let mutable index = 0
    let span = slice.AsSpan()
    while index < span.Length && predicate span[index] do
        index <- index + 1
    slice[..index]

let skipWhihe (predicate: 'T -> bool) (slice: 'T slice) : 'T slice =
    let mutable index = 0
    let span = slice.AsSpan()
    while index < span.Length && predicate span[index] do
        index <- index + 1
    slice[index..]

let tail (slice: 'T slice) : 'T slice =
    if isEmpty slice then
        raiseEmpty (nameof slice)
    else
        slice[1..]

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

let partition (predicate: 'T -> bool) (slice: 'T slice) : 'T slice * 'T slice =
    let struct (x, y) = vPartition predicate slice
    x, y

let splitAt index (slice: 'T slice) : 'T slice * 'T slice = slice[..index], slice[index..]

let vSplitAt index (slice: 'T slice) : struct ('T slice * 'T slice) = slice[..index], slice[index..]

// ============================== concats ==============================

[<ExcludeFromCodeCoverage>]
let append (slice1: 'T slice) (slice2: 'T slice) : 'T slice = addSpan (slice2.AsSpan()) slice1

let collect (mapping: 'T -> 'U slice) (slice: 'T slice) : 'U slice =
    let mutable result = createDefaultCap<'U> slice.Length
    for x in slice do
        result <- append result (mapping x)
    result

let concat (slices: seq<'T slice>) : 'T slice =
    if isNull slices then
        empty
    else
        let mutable result = empty<'T>
        for slice in slices do
            result <- append result slice
        result

// ============================== foldings ==============================

let fold (folder: 'State -> 'T -> 'State) (state: 'State) (slice: 'T slice) =
    if isEmpty slice then
        state
    else
        let f = OptimizedClosures.FSharpFunc<_, _, _>.Adapt(folder)
        let mutable acc = state
        for x in slice do
            acc <- f.Invoke(acc, x)
        acc

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

let scan (folder: 'State -> 'T -> 'State) (state: 'State) (slice: 'T slice) : 'State slice =
    if isEmpty slice then
        empty
    else
        let f = OptimizedClosures.FSharpFunc<_, _, _>.Adapt(folder)
        let mutable acc = state
        freeMap
            slice.Length
            slice
            (FreeMapping(fun i span ->
                acc <- f.Invoke(acc, span[i])
                acc
            ))

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

let vMapFold (folder: 'State -> 'T -> struct ('Result * 'State)) (state: 'State) (slice: 'T slice) : struct ('Result slice * 'State) =
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

let vMapFoldBack (folder: 'T -> 'State -> struct ('Result * 'State)) (slice: 'T slice) (state: 'State) : struct ('Result slice * 'State) =
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

let reduce reduction (slice: 'T slice) =
    if isEmpty slice then
        raiseEmpty (nameof slice)
    else
        fold reduction slice[0] slice[1..]

let reduceBack reduction (slice: 'T slice) =
    if isEmpty slice then
        raiseEmpty (nameof slice)
    else
        foldBack reduction slice[.. slice.Length - 1] slice[slice.Length - 1]

// ============================== aggregations ==============================

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

[<ExcludeFromCodeCoverage>]
let inline max (slice: 'T slice) = top (<) slice

[<ExcludeFromCodeCoverage>]
let inline maxBy (projection: 'T -> 'U) (slice: 'T slice) = topBy projection (<) slice

[<ExcludeFromCodeCoverage>]
let inline min (slice: 'T slice) = top (>) slice

[<ExcludeFromCodeCoverage>]
let inline minBy (projection: 'T -> 'U) (slice: 'T slice) = topBy projection (>) slice

[<ExcludeFromCodeCoverage>]
let inline sum (slice: 'T slice) = fold (Checked.(+)) LanguagePrimitives.GenericZero<'T> slice

[<ExcludeFromCodeCoverage>]
let inline sumBy (projection: 'T -> 'U) (slice: 'T slice) =
    fold (fun acc x -> Checked.(+) acc (projection x)) LanguagePrimitives.GenericZero<'U> slice

[<ExcludeFromCodeCoverage>]
let inline average (slice: 'T slice) =
    if isEmpty slice then
        invalidArg (nameof slice) "The input sequence was empty."
    else
        let sum = sum slice
        LanguagePrimitives.DivideByInt sum slice.Length

[<ExcludeFromCodeCoverage>]
let inline averageBy (projection: 'T -> 'U) (slice: 'T slice) =
    if isEmpty slice then
        invalidArg (nameof slice) "The input sequence was empty."
    else
        let sum = sumBy projection slice
        LanguagePrimitives.DivideByInt sum slice.Length

// ============================== item fetchings ==============================

let exactlyOne (slice: 'T slice) =
    match slice.Length with
    | 1 -> slice[0]
    | 0 -> raiseEmpty (nameof slice)
    | _ -> raiseMoreThanOneElement (nameof slice)

let tryExactlyOne (slice: 'T slice) =
    match slice.Length with
    | 1 -> Some(slice[0])
    | _ -> None

let vTryExactlyOne (slice: 'T slice) =
    match slice.Length with
    | 1 -> ValueSome(slice[0])
    | _ -> ValueNone

let find (predicate: 'T -> bool) (slice: 'T slice) =
    let rec loop index =
        if index >= slice.Length then raiseNotFound ()
        elif predicate slice[index] then slice[index]
        else loop (index + 1)
    loop 0

let tryFind (predicate: 'T -> bool) (slice: 'T slice) =
    let rec loop index =
        if index >= slice.Length then None
        elif predicate slice[index] then Some(slice[index])
        else loop (index + 1)
    loop 0

let vTryFind (predicate: 'T -> bool) (slice: 'T slice) =
    let rec loop index =
        if index >= slice.Length then ValueNone
        elif predicate slice[index] then ValueSome(slice[index])
        else loop (index + 1)
    loop 0

let findBack (predicate: 'T -> bool) (slice: 'T slice) =
    let rec loop index =
        if index < 0 then raiseNotFound ()
        elif predicate slice[index] then slice[index]
        else loop (index - 1)
    loop (slice.Length - 1)

let tryFindBack (predicate: 'T -> bool) (slice: 'T slice) =
    let rec loop index =
        if index < 0 then None
        elif predicate slice[index] then Some(slice[index])
        else loop (index - 1)
    loop (slice.Length - 1)

let vTryFindBack (predicate: 'T -> bool) (slice: 'T slice) =
    let rec loop index =
        if index < 0 then ValueNone
        elif predicate slice[index] then ValueSome(slice[index])
        else loop (index - 1)
    loop (slice.Length - 1)

let findIndex (predicate: 'T -> bool) (slice: 'T slice) =
    let rec loop index =
        if index >= slice.Length then raiseNotFound ()
        elif predicate slice[index] then index
        else loop (index + 1)
    loop 0

let tryFindIndex (predicate: 'T -> bool) (slice: 'T slice) =
    let rec loop index =
        if index >= slice.Length then None
        elif predicate slice[index] then Some(index)
        else loop (index + 1)
    loop 0

let vTryFindIndex (predicate: 'T -> bool) (slice: 'T slice) =
    let rec loop index =
        if index >= slice.Length then ValueNone
        elif predicate slice[index] then ValueSome(index)
        else loop (index + 1)
    loop 0

let findIndexBack (predicate: 'T -> bool) (slice: 'T slice) =
    let rec loop index =
        if index < 0 then raiseNotFound ()
        elif predicate slice[index] then index
        else loop (index - 1)
    loop (slice.Length - 1)

let tryFindIndexBack (predicate: 'T -> bool) (slice: 'T slice) =
    let rec loop index =
        if index < 0 then None
        elif predicate slice[index] then Some(index)
        else loop (index - 1)
    loop (slice.Length - 1)

let vTryFindIndexBack (predicate: 'T -> bool) (slice: 'T slice) =
    let rec loop index =
        if index < 0 then ValueNone
        elif predicate slice[index] then ValueSome(index)
        else loop (index - 1)
    loop (slice.Length - 1)

let head (slice: 'T slice) =
    if isEmpty slice then
        raiseEmpty (nameof slice)
    else
        slice[0]

let tryHead (slice: 'T slice) = if isEmpty slice then None else Some(slice[0])

let vTryHead (slice: 'T slice) = if isEmpty slice then ValueNone else ValueSome(slice[0])

let last (slice: 'T slice) =
    if isEmpty slice then
        raiseEmpty (nameof slice)
    else
        slice[slice.Length - 1]

let tryLast (slice: 'T slice) =
    if isEmpty slice then
        None
    else
        Some(slice[slice.Length - 1])

let vTryLast (slice: 'T slice) =
    if isEmpty slice then
        ValueNone
    else
        ValueSome(slice[slice.Length - 1])

[<ExcludeFromCodeCoverage>]
let inline item index (slice: 'T slice) = slice[index]

let tryItem index (slice: 'T slice) =
    if 0 <= index && index < slice.Length then
        Some(slice[index])
    else
        None

let vTryItem index (slice: 'T slice) =
    if 0 <= index && index < slice.Length then
        ValueSome(slice[index])
    else
        ValueNone

let pick (chooser: 'T -> 'U voption) (slice: 'T slice) =
    let rec loop index =
        if index >= slice.Length then
            raiseNotFound ()
        else
            match chooser slice[index] with
            | ValueSome(x) -> x
            | ValueNone -> loop (index + 1)
    loop 0

let tryPick (chooser: 'T -> 'U option) (slice: 'T slice) =
    let rec loop index =
        if index >= slice.Length then
            None
        else
            match chooser slice[index] with
            | Some(x) -> Some(x)
            | None -> loop (index + 1)
    loop 0

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

let contains (value: 'T) (source: 'T slice) =
    let rec loop index =
        if index >= source.Length then false
        elif source[index] = value then true
        else loop (index + 1)
    loop 0

let exists (predicate: 'T -> bool) (slice: 'T slice) =
    let rec loop index =
        if index >= slice.Length then false
        elif predicate slice[index] then true
        else loop (index + 1)
    loop 0

let exists2 (predicate: 'T1 -> 'T2 -> bool) (slice1: 'T1 slice) (slice2: 'T2 slice) =
    let length = Operators.min slice1.Length slice2.Length
    let f = OptimizedClosures.FSharpFunc<_, _, _>.Adapt(predicate)
    let rec loop index =
        if index >= length then false
        elif f.Invoke(slice1[index], slice2[index]) then true
        else loop (index + 1)
    loop 0

let forall (predicate: 'T -> bool) (slice: 'T slice) =
    let rec loop index =
        if index >= slice.Length then true
        elif not (predicate slice[index]) then false
        else loop (index + 1)
    loop 0

let forall2 (predicate: 'T1 -> 'T2 -> bool) (slice1: 'T1 slice) (slice2: 'T2 slice) =
    let length = Operators.min slice1.Length slice2.Length
    let f = OptimizedClosures.FSharpFunc<_, _, _>.Adapt(predicate)
    let rec loop index =
        if index >= length then true
        elif not (f.Invoke(slice1[index], slice2[index])) then false
        else loop (index + 1)
    loop 0

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

let sortWith (comparer: 'T -> 'T -> int) (slice: 'T slice) : 'T slice =
    let result = copy slice
    array.Sort(result.Back.Field, Comparison(comparer))
    result

let sort (slice: 'T slice) : 'T slice = sortWith compare slice

let sortBy (projection: 'T -> 'Key) (slice: 'T slice) : 'T slice = sortWith (fun x y -> compare (projection x) (projection y)) slice

let sortDescending (slice: 'T slice) : 'T slice = sortWith (fun x y -> -compare x y) slice

let sortByDescending (projection: 'T -> 'Key) (slice: 'T slice) : 'T slice =
    sortWith (fun x y -> -compare (projection x) (projection y)) slice

let rev (slice: 'T slice) : 'T slice = freeMap slice.Length slice (FreeMapping(fun i span -> span[span.Length - 1 - i]))

let permute (indexMap: int -> int) (slice: 'T slice) : 'T slice =
    let result = zeroCreate slice.Length
    let span = slice.AsSpan()
    let resultSpan = result.AsMutableSpan()
    for i = 0 to span.Length - 1 do
        resultSpan[indexMap i] <- span[i]
    result

// ============================== updatings ==============================

let insertAt index (value: 'T) (source: 'T slice) : 'T slice =
    create<'T> (source.Length + 1) |> addSlice source[..index] |> add value |> addSlice source[index..]

let insertSliceAt index (values: 'T slice) (source: 'T slice) : 'T slice =
    create<'T> (source.Length + values.Length)
    |> addSlice source[..index]
    |> addSlice values
    |> addSlice source[index..]

let insertManyAt index (values: seq<'T>) (source: 'T slice) : 'T slice =
    create<'T> source.Length |> addSlice source[..index] |> addRange values |> addSlice source[index..]

let removeAt index (source: 'T slice) : 'T slice = create<'T> source.Length |> addSlice source[..index] |> addSlice source[index + 1 ..]

let removeManyAt index count (source: 'T slice) : 'T slice =
    create<'T> source.Length |> addSlice source[..index] |> addSlice source[index + count ..]

let updateAt index value (source: 'T slice) : 'T slice =
    if uint source.Length <= uint index then
        invalidArg (nameof index) "The index is outside 0..source.Length - 1"
    let result = copy source
    result.ForSet(index) <- value
    result

// ============================== others ==============================

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

let distinct (slice: 'T slice) : 'T slice =
    if isEmpty slice then
        empty
    else
        let hashSet = HashSet(HashIdentity.Structural<'T>)
        filter hashSet.Add slice

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

let except (itemsToExclude: 'T slice) (slice: 'T slice) : 'T slice =
    if isEmpty slice then
        empty
    else
        let cached = HashSet(itemsToExclude, HashIdentity.Structural)
        filter cached.Add slice

let pairwise (slice: 'T slice) : ('T * 'T) slice =
    if slice.Length <= 1 then
        raiseNeedMoreElement (nameof slice)
    else
        freeMap (slice.Length - 1) slice (FreeMapping(fun i span -> (span[i], span[i + 1])))

let vPairwise (slice: 'T slice) : struct ('T * 'T) slice =
    if slice.Length <= 1 then
        raiseNeedMoreElement (nameof slice)
    else
        freeMap (slice.Length - 1) slice (FreeMapping(fun i span -> (span[i], span[i + 1])))

let chunkBySize chunkSize (slice: 'T slice) : 'T slice slice =
    if chunkSize <= 0 then
        invalidArg (nameof chunkSize) "chunkSize must be positive."
    if isEmpty slice then
        empty
    else
        let resultLength = (slice.Length + chunkSize - 1) / chunkSize
        initInline resultLength (fun i -> slice[i * chunkSize .. (i + 1) * chunkSize])

let transpose (slices: seq<'T slice>) : 'T slice slice =
    use enumerator = slices.GetEnumerator()
    if not (enumerator.MoveNext()) then
        empty
    else
        let first = enumerator.Current
        // zeroCreate によって empty が代入されるため、そのまま後続処理を行ってよい
        let result = zeroCreate<'T slice> first.Length
        let resultSpan = result.AsMutableSpan()
        while enumerator.MoveNext() do
            let current = enumerator.Current
            if current.Length <> first.Length then
                invalidArg (nameof slices) "The input slices must be of same length."
            let span = current.AsSpan()
            for i = 0 to span.Length - 1 do
                resultSpan[i] <- resultSpan[i] |> add span[i]
        result

let windowed windowSize (slice: 'T slice) : 'T slice slice =
    if windowSize <= 0 then
        invalidArg (nameof windowSize) "The windowsize must be positive."
    elif isEmpty slice then
        empty
    elif slice.Length <= windowSize then
        singleton slice
    else
        initInline (slice.Length - windowSize) (fun i -> slice.Slice(i, windowSize))
