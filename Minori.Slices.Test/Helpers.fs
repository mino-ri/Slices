module Minori.Slices.Test.Helpers

open Minori.Slices

let toValueTuple (x, y) = struct (x, y)

let toValueTuple3 (x, y, z) = struct (x, y, z)

let toValueTupleSeq seq = Seq.map toValueTuple seq

let toValueTupleSeq3 seq = Seq.map toValueTuple3 seq

let toValueOption x =
    match x with
    | Some(v) -> ValueSome(v)
    | None -> ValueNone

let toSliceSlice seq = Seq.map Slice.ofSeq seq
