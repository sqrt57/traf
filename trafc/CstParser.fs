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

module ParserHelper =

    type Parser<'lex, 'value, 'err> = 'lex list -> Result<'lex list * 'value, 'err>

    type ParseSeqBuilder() =

        member this.Bind(x: Parser<'lex, 'x, 'err>,
                         f: 'x -> Parser<'lex, 'y, 'err>)
                         : Parser<'lex, 'y, 'err> =
            fun lexemes ->
                match x lexemes with
                | Ok (lexemes, value) -> f value lexemes
                | Error err -> Error err

        member this.Return(x: 'x) : Parser<'lex, 'x, 'err> =
            fun lexemes -> Ok (lexemes, x)

    let parseSeq = ParseSeqBuilder()

    let expectedButGot expected lexemes =
        match lexemes with 
        | lexeme :: _ -> Error <| sprintf "expected %s but got %A" expected lexeme
        | [] -> Error <| sprintf "expected %s but got EOF" expected

    let matchEq value errorMessage lexemes =
        match lexemes with
        | x :: rest when x = value -> Ok (rest, ())
        | other -> expectedButGot errorMessage other

    let matchFilter filter errorMessage lexemes =
        match lexemes with
        | x :: rest ->
            match filter x with
            | Some x -> Ok (rest, x)
            | None -> expectedButGot errorMessage (x :: rest)
        | other -> expectedButGot errorMessage other

module CstParser =

    open ParserHelper

    exception CstParserError of {| message: string |}

    let errorExpectedButGot expected lexemes =
        match lexemes with 
        | lexeme :: _ -> raise <| CstParserError {| message = sprintf "expected %s but got %A" expected lexeme |}
        | [] -> raise <| CstParserError {| message = sprintf "expected %s but got EOF" expected |}

    let parse (lexemes: Lexeme list) : Cst.TopLevel =

        let constDefinition lexemes =

            let parser = parseSeq {
                let! name = matchFilter (function
                                         | Lexeme.Identifier name -> Some name
                                         | _ -> None)
                                        "constant name after const keyword"

                do! matchEq (Lexeme.Operator ":") "colon after constant name in constant definition"

                let! typeName = matchFilter (function
                                            | Lexeme.Identifier typeName -> Some typeName
                                            | _ -> None)
                                            "constant type after colon in constant definition"

                do! matchEq (Lexeme.Operator ":=") "assignment operator after constant type in constant definition"

                let! intValue = matchFilter (function
                                            | Lexeme.Int intValue -> Some intValue
                                            | _ -> None)
                                            "constant value after assignment operator in constant definition"

                do! matchEq Lexeme.Semicolon "semicolon after constant definition"

                let constantDefinition =
                    { Cst.ConstantDefinition.name = name
                      Cst.ConstantDefinition.type_ = Cst.Builtin typeName
                      Cst.ConstantDefinition.value = Cst.IntVal intValue }
                return constantDefinition
            }

            let parseResult = parser lexemes

            match parseResult with
            | Ok result -> result
            | Error message -> raise <| CstParserError {| message = message |}

        let moduleBody lexemes =

            let moduleBodyItems = ResizeArray<Cst.ModuleTopLevel>()

            let rec moduleBodyItem lexemes =
                match lexemes with
                | Lexeme.Identifier "const" :: rest ->
                    let rest, definition = constDefinition rest
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
