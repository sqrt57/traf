module Triton.Tests.MarkTypes

open Xunit
open Swensen.Unquote

open Triton.AstCreate
open Triton.MarkTypes

[<Fact>]
let ``Empty top level`` () =
    let input = topLevel []
    let expectedOutput = topLevel []
    test <@ expectedOutput = markTypes input @>