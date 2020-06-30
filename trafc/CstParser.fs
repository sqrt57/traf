namespace Triton

open System
open System.Text
open System.Collections.Generic

[<RequireQualifiedAccess>]
module Cst =

    type Expr =
        | IntVal of int64
        | CharVal of char
        | BoolVal of bool
        | Reference of string
        | Null
        | FunCall of {| func: Expr; arguments: Expr list; |}
        | SizeOf of Expr
        | AddressOf of Expr

    type TupleType = Tuple of {| name: string option; type_: Type |} list
    and FunType = { arguments: TupleType; result: TupleType }
    and Type =
        | Builtin of string
        | Array of {| type_: Type; size: Expr option |}
        | Pointer of Type
        | Fun of FunType

    type AttrValue =
        | None
        | String of string
        | Int of int64
    type AttrDefinition = { name: string; value: AttrValue }
    type AttrList = AttrList of AttrDefinition list
    type AttrLists = AttrLists of AttrList list

    type ConstantDefinition = { name: string; type_: Type; value: Expr; }
    type VariableDefinition = { name: string; type_: Type; value: Expr option; }

    type Statement =
        | ConstStatement of ConstantDefinition
        | VarStatement of VariableDefinition
        | Assignment of {| name: string; value: Expr; |}
        | Expression of Expr
    type FunBody = FunBody of Statement list

    type ModuleTopLevel =
        | ConstDefinition of ConstantDefinition
        | FunDefinition of {| name: string; type_: FunType; body: FunBody; attributes: AttrLists |}
        | ExternFunDefinition of {| name: string; type_: FunType; attributes: AttrLists |}

    type Module = { name: string; definitions: ModuleTopLevel list }

    type TopLevel = TopLevel of Module list

module CstParser =

    exception CstParserError of {| message: string |}

    type private ParserBuilder() =

        member this.Bind(x, f) =
            match x with
            | Ok value -> f value
            | Error e -> Error e

        member this.Return(x) = Ok x

    let private parser = ParserBuilder()

    let errorExpectedButGot expected lexemes =
        match lexemes with 
        | lexeme :: _ -> raise <| CstParserError {| message = sprintf "expected %s but got %A" expected lexeme |}
        | [] -> raise <| CstParserError {| message = sprintf "expected %s but got EOF" expected |}

    let expectedButGot expected lexemes =
        match lexemes with 
        | lexeme :: _ -> Error <| sprintf "expected %s but got %A" expected lexeme
        | [] -> Error <| sprintf "expected %s but got EOF" expected

    let matchEq lexemes value errorMessage =
        match lexemes with
        | x :: rest when x = value -> Ok rest
        | other -> expectedButGot errorMessage other

    let parse (lexemes: Lexeme list) : Cst.TopLevel =

        let constDefinition lexemes =

            let parseResult = parser {
                let! lexemes, name =
                    match lexemes with
                    | Lexeme.Identifier name :: rest -> Ok (rest, name)
                    | other -> expectedButGot "constant name after const keyword" other

                let! lexemes = matchEq lexemes (Lexeme.Operator ":") "colon after constant name in constant definition"

                let! lexemes, typeName =
                    match lexemes with
                    | Lexeme.Identifier typeName :: rest -> Ok (rest, typeName)
                    | other -> expectedButGot "constant type after colon in constant definition" other

                let! lexemes = matchEq lexemes (Lexeme.Operator ":=") "assignment operator after constant type in constant definition"

                let! lexemes, intValue =
                    match lexemes with
                    | Lexeme.Int intValue :: rest -> Ok (rest, intValue)
                    | other -> expectedButGot "constant value after assignment operator in constant definition" other

                let! lexemes = matchEq lexemes Lexeme.Semicolon "semicolon after constant definition"

                let constantDefinition =
                    { Cst.ConstantDefinition.name = name
                      Cst.ConstantDefinition.type_ = Cst.Builtin typeName
                      Cst.ConstantDefinition.value = Cst.IntVal intValue }
                return constantDefinition, lexemes
            }

            match parseResult with
            | Ok result -> result
            | Error message -> raise <| CstParserError {| message = message |}

        let moduleBody lexemes =

            let moduleBodyItems = ResizeArray<Cst.ModuleTopLevel>()

            let rec moduleBodyItem lexemes =
                match lexemes with
                | Lexeme.Identifier "const" :: rest ->
                    let definition, rest = constDefinition rest
                    moduleBodyItems.Add <| Cst.ConstDefinition definition
                    moduleBodyItem rest
                | Lexeme.RightCurly :: _ -> lexemes
                | other -> errorExpectedButGot "closing curly bracket after module body" other

            let rest = moduleBodyItem lexemes
            List.ofSeq moduleBodyItems, rest

        let moduleDefinition lexemes =
            match lexemes with
            | Lexeme.Identifier name :: rest ->
                match rest with
                | Lexeme.LeftCurly :: rest ->
                    let definitions, rest = moduleBody rest
                    match rest with
                    | Lexeme.RightCurly :: rest -> { Cst.Module.name = name; Cst.definitions = definitions }, rest
                    | other -> errorExpectedButGot "closing curly bracket after module body" other
                | other -> errorExpectedButGot "opening curly bracket after module name" other
            | other -> errorExpectedButGot "module name after module keyword" other

        let topLevel lexemes =

            let modules = ResizeArray<Cst.Module>()

            let rec topLevelItem lexemes =
                match lexemes with
                | [] -> ()
                | Lexeme.Identifier "module" :: rest ->
                    let module_, rest = moduleDefinition rest
                    modules.Add module_
                    topLevelItem rest
                | l :: _ -> raise <| CstParserError {| message = sprintf "unexpected lexeme at top level: %A" l |}

            topLevelItem lexemes
            Cst.TopLevel <| List.ofSeq modules

        topLevel lexemes
