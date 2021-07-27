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
let ``Do not parse closing bracket in module defintion`` () =
    let actual = CstParser.ParseModule.moduleBodyItem [ Lexeme.RightCurly ]
    test <@ ParserHelper.NoMatch = actual @>

[<Fact>]
let ``Parse const definition with simple type`` () =
    let expected = Cst.ConstDefinition { name = "C"; type_ = Cst.TypeName "int32"; value = Cst.IntVal 15L }
    let actual = CstParser.ParseModule.moduleBodyItem [
        Lexeme.Identifier "const"
        Lexeme.Identifier "C"
        Lexeme.Operator ":"
        Lexeme.Identifier "int32"
        Lexeme.Operator ":="
        Lexeme.Int 15L
        Lexeme.Semicolon ]
    test <@ ParserHelper.Match ([], expected) = actual @>

[<Fact>]
let ``Parse const definition with pointer type`` () =
    let expected =
        Cst.ConstDefinition
            { name = "C"
              type_ = Cst.Pointer <| Cst.TypeName "int32"
              value = Cst.IntVal 15L }
    let actual = CstParser.ParseModule.moduleBodyItem [
        Lexeme.Identifier "const"
        Lexeme.Identifier "C"
        Lexeme.Operator ":"
        Lexeme.Caret
        Lexeme.Identifier "int32"
        Lexeme.Operator ":="
        Lexeme.Int 15L
        Lexeme.Semicolon ]
    test <@ ParserHelper.Match ([], expected) = actual @>

[<Fact>]
let ``Parse const definition with sized array type`` () =
    let expected =
        Cst.ConstDefinition
            { name = "C"
              type_ = Cst.Array {| type_ = Cst.TypeName "int32"; size = Some <| Cst.IntVal 20L; |}
              value = Cst.IntVal 15L }
    let actual = CstParser.ParseModule.moduleBodyItem [
        Lexeme.Identifier "const"
        Lexeme.Identifier "C"
        Lexeme.Operator ":"
        Lexeme.LeftSquare
        Lexeme.Int 20L
        Lexeme.RightSquare
        Lexeme.Identifier "int32"
        Lexeme.Operator ":="
        Lexeme.Int 15L
        Lexeme.Semicolon ]
    test <@ ParserHelper.Match ([], expected) = actual @>

[<Fact>]
let ``Parse const definition with unsized array type`` () =
    let expected =
        Cst.ConstDefinition
            { name = "C"
              type_ = Cst.Array {| type_ = Cst.TypeName "int32"; size = None; |}
              value = Cst.IntVal 15L }
    let actual = CstParser.ParseModule.moduleBodyItem [
        Lexeme.Identifier "const"
        Lexeme.Identifier "C"
        Lexeme.Operator ":"
        Lexeme.LeftSquare
        Lexeme.RightSquare
        Lexeme.Identifier "int32"
        Lexeme.Operator ":="
        Lexeme.Int 15L
        Lexeme.Semicolon ]
    test <@ ParserHelper.Match ([], expected) = actual @>

[<Fact>]
let ``Parse const definition simple type with brackets`` () =
    let expected =
        Cst.ConstDefinition
            { name = "C"
              type_ = Cst.Type.Tuple <| Cst.TupleType [ {| name = None; type_ = Cst.TypeName "int32" |} ]
              value = Cst.IntVal 15L }
    let actual = CstParser.ParseModule.moduleBodyItem [
        Lexeme.Identifier "const"
        Lexeme.Identifier "C"
        Lexeme.Operator ":"
        Lexeme.LeftBracket
        Lexeme.Identifier "int32"
        Lexeme.RightBracket
        Lexeme.Operator ":="
        Lexeme.Int 15L
        Lexeme.Semicolon ]
    test <@ ParserHelper.Match ([], expected) = actual @>

[<Fact>]
let ``Parse function definition`` () =
    let expected =
        Cst.FunDefinition
            {| name = "F"
               type_ = { arguments = Cst.TupleType []; result = Cst.TupleType [] }
               body = Cst.FunBody []
               attributes = Cst.AttrLists [] |}
    let actual = CstParser.ParseModule.moduleBodyItem [
        Lexeme.Identifier "fun"
        Lexeme.Identifier "F"
        Lexeme.Operator ":"
        Lexeme.LeftBracket
        Lexeme.RightBracket
        Lexeme.Operator "->"
        Lexeme.LeftBracket
        Lexeme.RightBracket
        Lexeme.LeftCurly
        Lexeme.RightCurly ]
    test <@ ParserHelper.Match ([], expected) = actual @>

[<Fact>]
let ``Parse function definition with simple types`` () =
    let expected =
        Cst.FunDefinition
            {| name = "F"
               type_ = { arguments = Cst.TupleType [ {| name = None; type_ = Cst.TypeName "int16" |} ]
                         result = Cst.TupleType [ {| name = None; type_ = Cst.TypeName "int32" |} ] }
               body = Cst.FunBody []
               attributes = Cst.AttrLists [] |}
    let actual = CstParser.ParseModule.moduleBodyItem [
        Lexeme.Identifier "fun"
        Lexeme.Identifier "F"
        Lexeme.Operator ":"
        Lexeme.Identifier "int16"
        Lexeme.Operator "->"
        Lexeme.Identifier "int32"
        Lexeme.LeftCurly
        Lexeme.RightCurly ]
    test <@ ParserHelper.Match ([], expected) = actual @>

[<Fact>]
let ``Parse function definition with simple types in brackets`` () =
    let expected =
        Cst.FunDefinition
            {| name = "F"
               type_ = { arguments = Cst.TupleType [ {| name = None; type_ = Cst.TypeName "int16" |} ]
                         result = Cst.TupleType [ {| name = None; type_ = Cst.TypeName "int32" |} ] }
               body = Cst.FunBody []
               attributes = Cst.AttrLists [] |}
    let actual = CstParser.ParseModule.moduleBodyItem [
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
        Lexeme.RightCurly ]
    test <@ ParserHelper.Match ([], expected) = actual @>

[<Fact>]
let ``Parse function definition with complex types`` () =
    let expected =
        Cst.FunDefinition
            {| name = "F"
               type_ = { arguments = Cst.TupleType [ {| name = None; type_ = Cst.TypeName "int8" |}
                                                     {| name = None; type_ = Cst.TypeName "int16" |} ]
                         result = Cst.TupleType [ {| name = None; type_ = Cst.TypeName "int32" |} 
                                                  {| name = None; type_ = Cst.TypeName "int64" |} ] }
               body = Cst.FunBody []
               attributes = Cst.AttrLists [] |}

    let actual = CstParser.ParseModule.moduleBodyItem [
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
        Lexeme.RightCurly ]
    test <@ ParserHelper.Match ([], expected) = actual @>

[<Fact>]
let ``Parse external function definition`` () =
    let expected =
        Cst.ExternFunDefinition
            {| name = "exit_process"
               type_ = { arguments = Cst.TupleType [ {| name = None; type_ = Cst.TypeName "uint32"; |} ]
                         result = Cst.TupleType [] }
               attributes = Cst.AttrLists [
                   Cst.AttrList [ { name = "dll_import"; value = Cst.String "kernel32.dll" }
                                  { name = "entry_point"; value = Cst.Int 5L } ]
                   Cst.AttrList [ { name = "std_call"; value = Cst.None } ] ] |}
    let actual = CstParser.ParseModule.moduleBodyItem [
        Lexeme.LeftSquare
        Lexeme.Identifier "dll_import"
        Lexeme.Operator "="
        Lexeme.StringLiteral "kernel32.dll"
        Lexeme.Comma
        Lexeme.Identifier "entry_point"
        Lexeme.Operator "="
        Lexeme.Int 5L
        Lexeme.RightSquare
        Lexeme.LeftSquare
        Lexeme.Identifier "std_call"
        Lexeme.RightSquare
        Lexeme.Identifier "extern"
        Lexeme.Identifier "fun"
        Lexeme.Identifier "exit_process"
        Lexeme.Operator ":"
        Lexeme.Identifier "uint32"
        Lexeme.Operator "->"
        Lexeme.LeftBracket
        Lexeme.RightBracket
        Lexeme.Semicolon ]
    test <@ ParserHelper.Match ([], expected) = actual @>

[<Fact>]
let ``Fail parsing attributes without function`` () =
    let actual = CstParser.ParseModule.moduleBodyItem [
        Lexeme.LeftSquare
        Lexeme.Identifier "dll_import"
        Lexeme.Operator "="
        Lexeme.StringLiteral "kernel32.dll"
        Lexeme.RightSquare ]
    test <@ match actual with
            | ParserHelper.Match _ -> false
            | ParserHelper.NoMatch -> false
            | ParserHelper.Error _ -> true @>

[<Fact>]
let ``Do not parse closing bracket in function defintion`` () =
    let actual = CstParser.ParseModule.funBodyItem [ Lexeme.RightCurly ]
    test <@ ParserHelper.NoMatch = actual @>

[<Fact>]
let ``Parse variable definition inside function`` () =
    let expected = Cst.VarStatement { name = "x"; type_ = Cst.TypeName "int32"; value = None; }
    let actual = CstParser.ParseModule.funBodyItem [
        Lexeme.Identifier "var"
        Lexeme.Identifier "x"
        Lexeme.Operator ":"
        Lexeme.Identifier "int32"
        Lexeme.Semicolon ]
    test <@ ParserHelper.Match ([], expected) = actual @>

[<Fact>]
let ``Parse assignment inside function`` () =
    let expected = Cst.Assignment { name = "x"; value = Cst.IntVal 5L; }
    let actual = CstParser.ParseModule.funBodyItem [
        Lexeme.Identifier "x"
        Lexeme.Operator ":="
        Lexeme.Int 5L
        Lexeme.Semicolon ]
    test <@ ParserHelper.Match ([], expected) = actual @>

[<Fact>]
let ``Parse function call inside function`` () =
    let expected = Cst.Expression <| Cst.FunCall {| func = Cst.Reference "f"; arguments = []; |}
    let actual = CstParser.ParseModule.funBodyItem [
        Lexeme.Identifier "f"
        Lexeme.LeftBracket
        Lexeme.RightBracket
        Lexeme.Semicolon ]
    test <@ ParserHelper.Match ([], expected) = actual @>
