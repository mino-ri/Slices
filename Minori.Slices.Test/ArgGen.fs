namespace Minori.Slices.Test

open Testexp
open Minori.Slices

type FactAttribute = Xunit.FactAttribute

module ArgGen =
    let slice elementGenerator lengthGenerator =
        ArgGen.array elementGenerator lengthGenerator |> ArgGen.map Slice.ofArray
