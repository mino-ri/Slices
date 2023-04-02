namespace Minori.Slices

open System
open System.Diagnostics.CodeAnalysis


/// Contains the 'slice' computation expression builder.
[<ExcludeFromCodeCoverage>]
type SliceBuilder internal () =
    member inline _.Yield(x: 'T) = fun (s: 'T slice) -> Slice.add x s

    member inline _.YieldFrom(xs: 'T slice) = fun (s: 'T slice) -> Slice.addSlice xs s

    member inline _.YieldFrom(xs: seq<'T>) = fun (s: 'T slice) -> Slice.addRange xs s

    member inline _.Delay([<InlineIfLambda>] f: unit -> 'T slice -> 'T slice) = fun (s: 'T slice) -> f () s

    member inline _.Zero() = fun (s: 'T slice) -> s

    member inline _.Combine([<InlineIfLambda>] a: 'T slice -> 'T slice, [<InlineIfLambda>] b: 'T slice -> 'T slice) =
        fun (s: 'T slice) -> b (a s)


    member inline _.For(source: 'C slice, [<InlineIfLambda>] action: 'C -> 'T slice -> 'T slice) =
        fun (s: 'T slice) ->
            let mutable s = s
            for x in source do
                s <- action x s
            s

    member inline _.For(source: seq<'C>, [<InlineIfLambda>] action: 'C -> 'T slice -> 'T slice) =
        fun (s: 'T slice) ->
            let mutable s = s
            for x in source do
                s <- action x s
            s

    member inline _.While([<InlineIfLambda>] condition: unit -> bool, [<InlineIfLambda>] action: 'T slice -> 'T slice) =
        fun (s: 'T slice) ->
            let mutable s = s
            while condition () do
                s <- action s
            s

    member inline _.Using(resource: 'R :> IDisposable, [<InlineIfLambda>] action: 'R -> 'T slice -> 'T slice) =
        fun (s: 'T slice) ->
            try
                action resource s
            finally
                resource.Dispose()

    member inline _.TryWith
        (
            [<InlineIfLambda>] action: 'T slice -> 'T slice,
            [<InlineIfLambda>] catch: exn -> 'T slice -> 'T slice
        ) =
        fun (s: 'T slice) ->
            try
                action s
            with ex ->
                catch ex s

    member inline _.TryFinally
        (
            [<InlineIfLambda>] action: 'T slice -> 'T slice,
            [<InlineIfLambda>] finallyAction: unit -> unit
        ) =
        fun (s: 'T slice) ->
            try
                action s
            finally
                finallyAction ()

    member inline _.Run([<InlineIfLambda>] f: 'T slice -> 'T slice) = f Slice.empty


[<AutoOpen>]
module SliceOperators =
    /// Builds a slice using computation expression syntax.
    let slice = SliceBuilder()
