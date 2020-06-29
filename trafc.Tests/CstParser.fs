module Triton.Tests.ParserTests

open Xunit
open Swensen.Unquote

open Triton

[<Fact>]
let ``Parse empty source`` () =
    test <@ Cst.TopLevel [] = CstParser.parse []  @>

[<Fact>]
let ``Parse empty module`` () =
    test <@ Cst.TopLevel [ { name = "X"; definitions = [] } ] =
        CstParser.parse [ Lexeme.Identifier "module"
                          Lexeme.Identifier "X"
                          Lexeme.LeftCurly
                          Lexeme.RightCurly ]
    @>
