namespace Triton

open System
open System.Text
open System.Collections.Generic

[<RequireQualifiedAccess>]
module Ast =

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

module Parser =

    exception ParserError of {| message: string |}

    let parse (cst: Cst.TopLevel) : Ast.TopLevel =

        let mapModule { Cst.Module.name = name } = { Ast.Module.name = name; Ast.Module.definitions = []; }

        let topLevel (Cst.TopLevel modules) = Ast.TopLevel <| List.map mapModule modules

        topLevel cst
