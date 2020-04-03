module Triton.Tests.ParserTests

open Xunit
open Swensen.Unquote

open Triton

[<Fact>]
let ``Parse empty source`` () =
    test <@ Ast.TopLevel [||] = Parser.parse []  @>

[<Fact>]
let ``Parse empty module`` () =
    test <@ Ast.TopLevel [| { name = "X" } |] =
        Parser.parse [ Lexeme.Identifier "module"
                       Lexeme.Identifier "X"
                       Lexeme.LeftCurly
                       Lexeme.RightCurly ]
    @>
