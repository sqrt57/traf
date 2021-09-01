module Triton.Tests.CstParseTests

open Xunit
open Swensen.Unquote

open Triton
open Triton.Parse
open Triton.CstParse

[<Fact>]
let ``Top level: empty source`` () =
    test <@ Cst.TopLevel [] = parse []  @>

[<Fact>]
let ``Top level: empty module`` () =
    let expected = Cst.TopLevel [ { name = "X"; definitions = Cst.ModuleTopLevel [] } ]
    let actual = parse [
        Lexeme.Identifier "module"
        Lexeme.Identifier "X"
        Lexeme.LeftCurly
        Lexeme.RightCurly ]
    test <@ expected = actual @>

[<Fact>]
let ``Top level: two empty modules`` () =
    let expected = Cst.TopLevel [ { name = "X"; definitions = Cst.ModuleTopLevel [] }
                                  { name = "Y"; definitions = Cst.ModuleTopLevel [] } ]
    let actual = parse [
        Lexeme.Identifier "module"
        Lexeme.Identifier "X"
        Lexeme.LeftCurly
        Lexeme.RightCurly
        Lexeme.Identifier "module"
        Lexeme.Identifier "Y"
        Lexeme.LeftCurly
        Lexeme.RightCurly ]
    test <@ expected = actual @>

[<Fact>]
let ``Module: do not parse closing bracket`` () =
    let actual = ParseModule.moduleBodyItem [ Lexeme.RightCurly ]
    test <@ NoMatch = actual @>

[<Fact>]
let ``Module: const definition with simple type`` () =
    let expected = Cst.ConstDefinition { name = "C"; type_ = Cst.TypeRef "int32"; value = Cst.IntVal 15L }
    let actual = ParseModule.moduleBodyItem [
        Lexeme.Identifier "const"
        Lexeme.Identifier "C"
        Lexeme.Operator ":"
        Lexeme.Identifier "int32"
        Lexeme.Operator ":="
        Lexeme.Int 15L
        Lexeme.Semicolon ]
    test <@ Match ([], expected) = actual @>

[<Fact>]
let ``Module: const definition with pointer type`` () =
    let expected =
        Cst.ConstDefinition
            { name = "C"
              type_ = Cst.Pointer <| Cst.TypeRef "int32"
              value = Cst.IntVal 15L }
    let actual = ParseModule.moduleBodyItem [
        Lexeme.Identifier "const"
        Lexeme.Identifier "C"
        Lexeme.Operator ":"
        Lexeme.Caret
        Lexeme.Identifier "int32"
        Lexeme.Operator ":="
        Lexeme.Int 15L
        Lexeme.Semicolon ]
    test <@ Match ([], expected) = actual @>

[<Fact>]
let ``Module: const definition with sized array type`` () =
    let expected =
        Cst.ConstDefinition
            { name = "C"
              type_ = Cst.Array { type_ = Cst.TypeRef "int32"; size = Some <| Cst.IntVal 20L; }
              value = Cst.IntVal 15L }
    let actual = ParseModule.moduleBodyItem [
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
    test <@ Match ([], expected) = actual @>

[<Fact>]
let ``Module: const definition with unsized array type`` () =
    let expected =
        Cst.ConstDefinition
            { name = "C"
              type_ = Cst.Array { type_ = Cst.TypeRef "int32"; size = None; }
              value = Cst.IntVal 15L }
    let actual = ParseModule.moduleBodyItem [
        Lexeme.Identifier "const"
        Lexeme.Identifier "C"
        Lexeme.Operator ":"
        Lexeme.LeftSquare
        Lexeme.RightSquare
        Lexeme.Identifier "int32"
        Lexeme.Operator ":="
        Lexeme.Int 15L
        Lexeme.Semicolon ]
    test <@ Match ([], expected) = actual @>

[<Fact>]
let ``Module: const definition simple type with brackets`` () =
    let expected =
        Cst.ConstDefinition
            { name = "C"
              type_ = Cst.Type.Tuple <| Cst.TypeTuple [ { name = None; type_ = Cst.TypeRef "int32" } ]
              value = Cst.IntVal 15L }
    let actual = ParseModule.moduleBodyItem [
        Lexeme.Identifier "const"
        Lexeme.Identifier "C"
        Lexeme.Operator ":"
        Lexeme.LeftBracket
        Lexeme.Identifier "int32"
        Lexeme.RightBracket
        Lexeme.Operator ":="
        Lexeme.Int 15L
        Lexeme.Semicolon ]
    test <@ Match ([], expected) = actual @>

[<Fact>]
let ``Module: function definition`` () =
    let expected =
        Cst.FunDefinition
            { name = "F"
              type_ = { arguments = Cst.TypeTuple []; result = Cst.TypeTuple [] }
              body = Cst.FunBody []
              attributes = Cst.AttrLists [] }
    let actual = ParseModule.moduleBodyItem [
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
    test <@ Match ([], expected) = actual @>

[<Fact>]
let ``Module: function definition with simple types`` () =
    let expected =
        Cst.FunDefinition
            { name = "F"
              type_ = { arguments = Cst.TypeTuple [ { name = None; type_ = Cst.TypeRef "int16" } ]
                        result = Cst.TypeTuple [ { name = None; type_ = Cst.TypeRef "int32" } ] }
              body = Cst.FunBody []
              attributes = Cst.AttrLists [] }
    let actual = ParseModule.moduleBodyItem [
        Lexeme.Identifier "fun"
        Lexeme.Identifier "F"
        Lexeme.Operator ":"
        Lexeme.Identifier "int16"
        Lexeme.Operator "->"
        Lexeme.Identifier "int32"
        Lexeme.LeftCurly
        Lexeme.RightCurly ]
    test <@ Match ([], expected) = actual @>

[<Fact>]
let ``Module: function definition with simple types in brackets`` () =
    let expected =
        Cst.FunDefinition
            { name = "F"
              type_ = { arguments = Cst.TypeTuple [ { name = None; type_ = Cst.TypeRef "int16" } ]
                        result = Cst.TypeTuple [ { name = None; type_ = Cst.TypeRef "int32" } ] }
              body = Cst.FunBody []
              attributes = Cst.AttrLists [] }
    let actual = ParseModule.moduleBodyItem [
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
    test <@ Match ([], expected) = actual @>

[<Fact>]
let ``Module: function definition with complex types`` () =
    let expected =
        Cst.FunDefinition
            { name = "F"
              type_ = { arguments = Cst.TypeTuple [ { name = None; type_ = Cst.TypeRef "int8" }
                                                    { name = None; type_ = Cst.TypeRef "int16" } ]
                        result = Cst.TypeTuple [ { name = None; type_ = Cst.TypeRef "int32" } 
                                                 { name = None; type_ = Cst.TypeRef "int64" } ] }
              body = Cst.FunBody []
              attributes = Cst.AttrLists [] }

    let actual = ParseModule.moduleBodyItem [
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
    test <@ Match ([], expected) = actual @>

[<Fact>]
let ``Module: function definition with named parameters`` () =
    let expected =
        Cst.FunDefinition
            { name = "F"
              type_ = { arguments = Cst.TypeTuple [ { name = Some "x"; type_ = Cst.TypeRef "int8" }
                                                    { name = Some "y"; type_ = Cst.TypeRef "int16" } ]
                        result = Cst.TypeTuple [] }
              body = Cst.FunBody []
              attributes = Cst.AttrLists [] }

    let actual = ParseModule.moduleBodyItem [
        Lexeme.Identifier "fun"
        Lexeme.Identifier "F"
        Lexeme.Operator ":"
        Lexeme.LeftBracket
        Lexeme.Identifier "x"
        Lexeme.Operator ":"
        Lexeme.Identifier "int8"
        Lexeme.Comma
        Lexeme.Identifier "y"
        Lexeme.Operator ":"
        Lexeme.Identifier "int16"
        Lexeme.RightBracket
        Lexeme.Operator "->"
        Lexeme.LeftBracket
        Lexeme.RightBracket
        Lexeme.LeftCurly
        Lexeme.RightCurly ]
    test <@ Match ([], expected) = actual @>

[<Fact>]
let ``Module: external function definition`` () =
    let expected =
        Cst.ExternFunDefinition
            { name = "exit_process"
              type_ = { arguments = Cst.TypeTuple [ { name = None; type_ = Cst.TypeRef "uint32"; } ]
                        result = Cst.TypeTuple [] }
              attributes = Cst.AttrLists [
                  Cst.AttrList [ { name = "dll_import"; value = Cst.String "kernel32.dll" }
                                 { name = "entry_point"; value = Cst.Int 5L } ]
                  Cst.AttrList [ { name = "std_call"; value = Cst.NoneValue } ] ] }
    let actual = ParseModule.moduleBodyItem [
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
    test <@ Match ([], expected) = actual @>

[<Fact>]
let ``Module: fail parsing attributes without function`` () =
    let actual = ParseModule.moduleBodyItem [
        Lexeme.LeftSquare
        Lexeme.Identifier "dll_import"
        Lexeme.Operator "="
        Lexeme.StringLiteral "kernel32.dll"
        Lexeme.RightSquare ]
    test <@ match actual with
            | Match _ -> false
            | NoMatch -> false
            | Error _ -> true @>

[<Fact>]
let ``Function body: do not parse closing bracket`` () =
    let actual = ParseModule.funBodyItem [ Lexeme.RightCurly ]
    test <@ NoMatch = actual @>

[<Fact>]
let ``Function body: variable definition`` () =
    let expected = Cst.VarStatement { name = "x"; type_ = Cst.TypeRef "int32"; value = None; }
    let actual = ParseModule.funBodyItem [
        Lexeme.Identifier "var"
        Lexeme.Identifier "x"
        Lexeme.Operator ":"
        Lexeme.Identifier "int32"
        Lexeme.Semicolon ]
    test <@ Match ([], expected) = actual @>

[<Fact>]
let ``Function body: assignment`` () =
    let expected = Cst.Assignment { name = "x"; value = Cst.IntVal 5L; }
    let actual = ParseModule.funBodyItem [
        Lexeme.Identifier "x"
        Lexeme.Operator ":="
        Lexeme.Int 5L
        Lexeme.Semicolon ]
    test <@ Match ([], expected) = actual @>

[<Fact>]
let ``Function body: function call`` () =
    let expected = Cst.Expression <| Cst.FunCall { func = Cst.Ref "f"; arguments = []; }
    let actual = ParseModule.funBodyItem [
        Lexeme.Identifier "f"
        Lexeme.LeftBracket
        Lexeme.RightBracket
        Lexeme.Semicolon ]
    test <@ Match ([], expected) = actual @>

[<Fact>]
let ``Expression: integer constant`` () =
    let expected = Cst.IntVal 10L
    let actual = ParseExpression.tryExpression [
        Lexeme.Int 10L ]
    test <@ Match ([], expected) = actual @>

[<Fact>]
let ``Expression: negative integer constant`` () =
    let expected = Cst.Negate <| Cst.IntVal 10L
    let actual = ParseExpression.tryExpression [
        Lexeme.Operator "-"
        Lexeme.Int 10L ]
    test <@ Match ([], expected) = actual @>

[<Fact>]
let ``Expression: string constant`` () =
    let expected = Cst.StringVal "hello"
    let actual = ParseExpression.tryExpression [
        Lexeme.StringLiteral "hello" ]
    test <@ Match ([], expected) = actual @>

[<Fact>]
let ``Expression: address of variable`` () =
    let expected = Cst.AddressOf <| Cst.Ref "x"
    let actual = ParseExpression.tryExpression [
        Lexeme.AtSign
        Lexeme.Identifier "x" ]
    test <@ Match ([], expected) = actual @>

[<Fact>]
let ``Expression: double function call`` () =
    let funCall = Cst.FunCall { func = Cst.Ref "f"; arguments = []; }
    let expected = Cst.FunCall { func = funCall; arguments = []; }
    let actual = ParseExpression.tryExpression [
        Lexeme.Identifier "f"
        Lexeme.LeftBracket
        Lexeme.RightBracket
        Lexeme.LeftBracket
        Lexeme.RightBracket ]
    test <@ Match ([], expected) = actual @>

[<Fact>]
let ``Expression: nested function call`` () =
    let funCallG = Cst.FunCall { func = Cst.Ref "g"; arguments = []; }
    let funCallH = Cst.FunCall { func = Cst.Ref "h"; arguments = []; }
    let expected = Cst.FunCall { func = Cst.Ref "f"; arguments = [funCallG; funCallH]; }
    let actual = ParseExpression.tryExpression [
        Lexeme.Identifier "f"
        Lexeme.LeftBracket
        Lexeme.Identifier "g"
        Lexeme.LeftBracket
        Lexeme.RightBracket
        Lexeme.Comma
        Lexeme.Identifier "h"
        Lexeme.LeftBracket
        Lexeme.RightBracket
        Lexeme.RightBracket ]
    test <@ Match ([], expected) = actual @>
