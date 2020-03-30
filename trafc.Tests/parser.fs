module Triton.Tests.ParserTests

open Xunit
open Swensen.Unquote

open Triton

[<Fact>]
let ``Parse empty module`` () =
    test <@ Ast.TopLevel [||] = Parser.parse [||]  @>
