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
let ``Parse const definition with simple type`` () =
    let definitions = [ Cst.ConstDefinition { name = "C"; type_ = Cst.TypeName "int32"; value = Cst.IntVal 15L } ]
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

[<Fact>]
let ``Parse const definition with pointer type`` () =
    let definitions = [
        Cst.ConstDefinition {
            name = "C"
            type_ = Cst.Pointer <| Cst.TypeName "int32"
            value = Cst.IntVal 15L } ]
    let expected = Cst.TopLevel [ { name = "Mod"; definitions = definitions } ]
    let actual = CstParser.parse [ Lexeme.Identifier "module"
                                   Lexeme.Identifier "Mod"
                                   Lexeme.LeftCurly
                                   Lexeme.Identifier "const"
                                   Lexeme.Identifier "C"
                                   Lexeme.Operator ":"
                                   Lexeme.Caret
                                   Lexeme.Identifier "int32"
                                   Lexeme.Operator ":="
                                   Lexeme.Int 15L
                                   Lexeme.Semicolon
                                   Lexeme.RightCurly ]
    test <@ expected = actual @>

[<Fact>]
let ``Parse const definition with sized array type`` () =
    let definitions = [
        Cst.ConstDefinition {
            name = "C"
            type_ = Cst.Array {| type_ = Cst.TypeName "int32"; size = Some <| Cst.IntVal 20L; |}
            value = Cst.IntVal 15L } ]
    let expected = Cst.TopLevel [ { name = "Mod"; definitions = definitions } ]
    let actual = CstParser.parse [ Lexeme.Identifier "module"
                                   Lexeme.Identifier "Mod"
                                   Lexeme.LeftCurly
                                   Lexeme.Identifier "const"
                                   Lexeme.Identifier "C"
                                   Lexeme.Operator ":"
                                   Lexeme.LeftSquare
                                   Lexeme.Int 20L
                                   Lexeme.RightSquare
                                   Lexeme.Identifier "int32"
                                   Lexeme.Operator ":="
                                   Lexeme.Int 15L
                                   Lexeme.Semicolon
                                   Lexeme.RightCurly ]
    test <@ expected = actual @>

[<Fact>]
let ``Parse const definition with unsized array type`` () =
    let definitions = [
        Cst.ConstDefinition {
            name = "C"
            type_ = Cst.Array {| type_ = Cst.TypeName "int32"; size = None; |}
            value = Cst.IntVal 15L } ]
    let expected = Cst.TopLevel [ { name = "Mod"; definitions = definitions } ]
    let actual = CstParser.parse [ Lexeme.Identifier "module"
                                   Lexeme.Identifier "Mod"
                                   Lexeme.LeftCurly
                                   Lexeme.Identifier "const"
                                   Lexeme.Identifier "C"
                                   Lexeme.Operator ":"
                                   Lexeme.LeftSquare
                                   Lexeme.RightSquare
                                   Lexeme.Identifier "int32"
                                   Lexeme.Operator ":="
                                   Lexeme.Int 15L
                                   Lexeme.Semicolon
                                   Lexeme.RightCurly ]
    test <@ expected = actual @>

[<Fact>]
let ``Parse const definition simple type with brackets`` () =
    let definitions =
      [ Cst.ConstDefinition
          { name = "C"
            type_ = Cst.Type.Tuple <| Cst.TupleType [ {| name = None; type_ = Cst.TypeName "int32" |} ]
            value = Cst.IntVal 15L } ]
    let expected = Cst.TopLevel [ { name = "Mod"; definitions = definitions } ]
    let actual = CstParser.parse [ Lexeme.Identifier "module"
                                   Lexeme.Identifier "Mod"
                                   Lexeme.LeftCurly
                                   Lexeme.Identifier "const"
                                   Lexeme.Identifier "C"
                                   Lexeme.Operator ":"
                                   Lexeme.LeftBracket
                                   Lexeme.Identifier "int32"
                                   Lexeme.RightBracket
                                   Lexeme.Operator ":="
                                   Lexeme.Int 15L
                                   Lexeme.Semicolon
                                   Lexeme.RightCurly ]
    test <@ expected = actual @>

[<Fact>]
let ``Parse empty function definition`` () =
    let definitions =
      [ Cst.FunDefinition
            {|
                name = "F"
                type_ = { arguments = Cst.TupleType []; result = Cst.TupleType [] }
                body = Cst.FunBody []
                attributes = Cst.AttrLists []
            |} ]
    let expected = Cst.TopLevel [ { name = "Mod"; definitions = definitions } ]
    let actual = CstParser.parse [ Lexeme.Identifier "module"
                                   Lexeme.Identifier "Mod"
                                   Lexeme.LeftCurly
                                   Lexeme.Identifier "fun"
                                   Lexeme.Identifier "F"
                                   Lexeme.Operator ":"
                                   Lexeme.LeftBracket
                                   Lexeme.RightBracket
                                   Lexeme.Operator "->"
                                   Lexeme.LeftBracket
                                   Lexeme.RightBracket
                                   Lexeme.LeftCurly
                                   Lexeme.RightCurly
                                   Lexeme.RightCurly ]
    test <@ expected = actual @>

[<Fact>]
let ``Parse function definition with simple types`` () =
    let definitions =
      [ Cst.FunDefinition
            {|
                name = "F"
                type_ = { arguments = Cst.TupleType [ {| name = None; type_ = Cst.TypeName "int16" |} ]
                          result = Cst.TupleType [ {| name = None; type_ = Cst.TypeName "int32" |} ] }
                body = Cst.FunBody []
                attributes = Cst.AttrLists []
            |} ]
    let expected = Cst.TopLevel [ { name = "Mod"; definitions = definitions } ]
    let actual = CstParser.parse [ Lexeme.Identifier "module"
                                   Lexeme.Identifier "Mod"
                                   Lexeme.LeftCurly
                                   Lexeme.Identifier "fun"
                                   Lexeme.Identifier "F"
                                   Lexeme.Operator ":"
                                   Lexeme.Identifier "int16"
                                   Lexeme.Operator "->"
                                   Lexeme.Identifier "int32"
                                   Lexeme.LeftCurly
                                   Lexeme.RightCurly
                                   Lexeme.RightCurly ]
    test <@ expected = actual @>

[<Fact>]
let ``Parse function definition with simple types in brackets`` () =
    let definitions =
      [ Cst.FunDefinition
            {|
                name = "F"
                type_ = { arguments = Cst.TupleType [ {| name = None; type_ = Cst.TypeName "int16" |} ]
                          result = Cst.TupleType [ {| name = None; type_ = Cst.TypeName "int32" |} ] }
                body = Cst.FunBody []
                attributes = Cst.AttrLists []
            |} ]
    let expected = Cst.TopLevel [ { name = "Mod"; definitions = definitions } ]
    let actual = CstParser.parse [ Lexeme.Identifier "module"
                                   Lexeme.Identifier "Mod"
                                   Lexeme.LeftCurly
                                   Lexeme.Identifier "fun"
                                   Lexeme.Identifier "F"
                                   Lexeme.Operator ":"
                                   Lexeme.LeftBracket
                                   Lexeme.Identifier "int16"
                                   Lexeme.RightBracket
                                   Lexeme.Operator "->"
                                   Lexeme.LeftBracket
                                   Lexeme.Identifier "int32"
                                   Lexeme.RightBracket
                                   Lexeme.LeftCurly
                                   Lexeme.RightCurly
                                   Lexeme.RightCurly ]
    test <@ expected = actual @>

[<Fact>]
let ``Parse function definition with complex types`` () =
    let definitions =
      [ Cst.FunDefinition
            {|
                name = "F"
                type_ = { arguments = Cst.TupleType [ {| name = None; type_ = Cst.TypeName "int8" |}
                                                      {| name = None; type_ = Cst.TypeName "int16" |} ]
                          result = Cst.TupleType [ {| name = None; type_ = Cst.TypeName "int32" |} 
                                                   {| name = None; type_ = Cst.TypeName "int64" |} ] }
                body = Cst.FunBody []
                attributes = Cst.AttrLists []
            |} ]
    let expected = Cst.TopLevel [ { name = "Mod"; definitions = definitions } ]
    let actual = CstParser.parse [ Lexeme.Identifier "module"
                                   Lexeme.Identifier "Mod"
                                   Lexeme.LeftCurly
                                   Lexeme.Identifier "fun"
                                   Lexeme.Identifier "F"
                                   Lexeme.Operator ":"
                                   Lexeme.LeftBracket
                                   Lexeme.Identifier "int8"
                                   Lexeme.Comma
                                   Lexeme.Identifier "int16"
                                   Lexeme.RightBracket
                                   Lexeme.Operator "->"
                                   Lexeme.LeftBracket
                                   Lexeme.Identifier "int32"
                                   Lexeme.Comma
                                   Lexeme.Identifier "int64"
                                   Lexeme.RightBracket
                                   Lexeme.LeftCurly
                                   Lexeme.RightCurly
                                   Lexeme.RightCurly ]
    test <@ expected = actual @>
