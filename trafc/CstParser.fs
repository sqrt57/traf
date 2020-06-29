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
        | FunDefinition of {| name: string; type_: FunType; body: FunBody; attributes: AttrList |}
        | ExternFunDefinition of {| name: string; type_: FunType; attributes: AttrList |}

    type Module = { name: string; definitions: ModuleTopLevel list }

    type TopLevel = TopLevel of Module list

module CstParser =

    exception CstParserError of {| message: string |}

    let parse (lexemes: Lexeme list) : Cst.TopLevel =
        let modules = ResizeArray<Cst.Module>()

        let moduleBody lexemes =
            match lexemes with
            | [] -> raise <| CstParserError {| message = sprintf "expected opening curly bracket after module name but got EOF" |}
            | Lexeme.LeftCurly :: rest ->
                match rest with
                | [] -> raise <| CstParserError {| message = sprintf "expected closing curly bracket after module body but got EOF" |}
                | Lexeme.RightCurly :: rest -> rest
                | l :: _ -> raise <| CstParserError {| message = sprintf "expected closing curly bracket after module body but got %A" l |}
            | l :: _ -> raise <| CstParserError {| message = sprintf "expected opening curly bracket after module name but got %A" l |}


        let rec topLevel lexemes =
            match lexemes with
            | [] -> ()
            | Lexeme.Identifier "module" :: rest ->
                match rest with
                | Lexeme.Identifier name :: rest ->
                    let rest = moduleBody rest
                    { Cst.Module.name = name; Cst.definitions = [] } |> modules.Add
                    topLevel rest
                | [] -> ()
                | l :: _ -> raise <| CstParserError {| message = sprintf "expected module name after module keyword but got %A" l |}
            | l :: _ -> raise <| CstParserError {| message = sprintf "unexpected lexeme at top level: %A" l |}

        topLevel lexemes

        Cst.TopLevel <| List.ofSeq modules
