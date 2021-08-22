module Triton.Tests.PeCoffTests

open Xunit
open Swensen.Unquote

open Triton.PeCoffAllocate

[<Fact>]
let ``Align`` () =
    align 0 5 =! 5

    align 1 5 =! 6
    align 1 6 =! 6

    align 2 7 =! 8
    align 2 8 =! 8
    align 2 9 =! 12
    align 2 10 =! 12
