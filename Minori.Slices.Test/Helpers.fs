module Minori.Slices.Test.Helpers

let toValueTuple (x, y) = struct (x, y)

let toValueTupleSeq seq = Seq.map toValueTuple seq

let toValueOption x =
    match x with
    | Some(v) -> ValueSome(v)
    | None -> ValueNone
