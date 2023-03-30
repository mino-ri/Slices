namespace Minori.Slices

[<AllowNullLiteral>]
type internal SliceBack<'T> =
    val Field: 'T[]
    val mutable Written: int

    new(field: 'T[], written: int) = { Field = field; Written = written }


module internal SliceBack =
    let alloc capacity : SliceBack<'T> = SliceBack(Array.zeroCreate (max 1 capacity), 0)
