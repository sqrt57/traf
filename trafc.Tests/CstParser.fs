module Triton.Tests.ParserTests

open Xunit
open Swensen.Unquote

open Triton

[<Fact>]
let ``Parse empty source`` () =
    test <@ Cst.TopLevel [] = CstParser.parse []  @>

[<Fact>]
let ``Parse empty module`` () =
    let expected = Cst.TopLevel [ { name = "X"; definitions = [] } ]
    let actual = CstParser.parse [ Lexeme.Identifier "module"
                                   Lexeme.Identifier "X"
                                   Lexeme.LeftCurly
                                   Lexeme.RightCurly ]
    test <@ expected = actual @>

[<Fact>]
let ``Parse two empty modules`` () =
    let expected = Cst.TopLevel [ { name = "X"; definitions = [] }
                                  { name = "Y"; definitions = [] } ]
    let actual = CstParser.parse [ Lexeme.Identifier "module"
                                   Lexeme.Identifier "X"
                                   Lexeme.LeftCurly
                                   Lexeme.RightCurly
                                   Lexeme.Identifier "module"
                                   Lexeme.Identifier "Y"
                                   Lexeme.LeftCurly
                                   Lexeme.RightCurly ]
    test <@ expected = actual @>


[<Fact>]
let ``Parse const definition stub`` () =
    let definitions = [ Cst.ConstDefinition { name = "C"; type_ = Cst.Builtin "int32"; value = Cst.IntVal 15L } ]
    let expected = Cst.TopLevel [ { name = "Mod"; definitions = definitions } ]
    let actual = CstParser.parse [ Lexeme.Identifier "module"
                                   Lexeme.Identifier "Mod"
                                   Lexeme.LeftCurly
                                   Lexeme.Identifier "const"
                                   Lexeme.Identifier "C"
                                   Lexeme.Operator ":"
                                   Lexeme.Identifier "int32"
                                   Lexeme.Operator ":="
                                   Lexeme.Int 15L
                                   Lexeme.Semicolon
                                   Lexeme.RightCurly ]
    test <@ expected = actual @>

