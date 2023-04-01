namespace Minori.Slices.Test

open Testexp
open Minori.Slices

type FactAttribute = Xunit.FactAttribute

module ArgGen =
    let vchoose (chooser: 'T -> 'U voption) (source: IArgumentGenerator<'T>) : IArgumentGenerator<'U> = argGen {
        let mutable result = ValueNone
        while result.IsNone do
            let! value = source
            result <- chooser value
        return result.Value
    }

    let different (source: IArgumentGenerator<'T>) =
        argGen {
            let! x = source
            and! y = source
            return if x <> y then ValueSome struct (x, y) else ValueNone
        }
        |> vchoose id

    let slice elementGenerator lengthGenerator =
        ArgGen.array elementGenerator lengthGenerator |> ArgGen.map Slice.ofArray

    let tuple (source: IArgumentGenerator<'T>) = argGen {
        let! x = source
        and! y = source
        return x, y
    }


    let tuple3 (source: IArgumentGenerator<'T>) = argGen {
        let! x = source
        and! y = source
        and! z = source
        return x, y, z
    }


    let vTuple (source: IArgumentGenerator<'T>) = argGen {
        let! x = source
        and! y = source
        return struct (x, y)
    }


    let vTuple3 (source: IArgumentGenerator<'T>) = argGen {
        let! x = source
        and! y = source
        and! z = source
        return struct (x, y, z)
    }
