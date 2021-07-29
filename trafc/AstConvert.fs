namespace Triton


[<RequireQualifiedAccess>]
module Ast =

    type ConstExpr =
        | IntVal of int64
        | CharVal of char
        | BoolVal of bool
        | StringVal of string
        | Ref of string
        | Null
        | Length of ConstExpr
        | SizeOf of ConstExpr
        | AddressOf of ConstExpr
        | Negate of ConstExpr
        | Operator of {| left: ConstExpr; op: string; right: ConstExpr; |}

    type Expr =
        | IntVal of int64
        | CharVal of char
        | BoolVal of bool
        | StringVal of string
        | Ref of string
        | Null
        | ConstExpr of ConstExpr
        | FunCall of {| func: Expr; arguments: Expr list; |}
        | Length of Expr
        | SizeOf of Expr
        | AddressOf of Expr
        | Negate of Expr
        | Operator of {| left: Expr; op: string; right: Expr; |}

    type TypeTupleSlot = { name: string option; type_: Type }
    and TypeTuple = TypeTuple of TypeTupleSlot list
    and FunType = { arguments: TypeTuple; result: TypeTuple }
    and Type =
        | TypeRef of string
        | Array of {| type_: Type; size: Expr option |}
        | Pointer of Type
        | Fun of FunType
        | Tuple of TypeTuple

    type ConstDefinition<'expr> = { name: string; type_: Type; value: 'expr; }

    type VariableDefinition<'expr> = { name: string; type_: Type; value: 'expr option; }

    type Assignment = { name: string; value: Expr; }

    type Statement =
        | ConstStatement of ConstDefinition<Expr>
        | VarStatement of VariableDefinition<Expr>
        | Assignment of Assignment
        | Expression of Expr

    type FunBody = FunBody of Statement list

    type FunDefinition =
        { name: string
          type_: FunType
          body: FunBody
          entry: bool }

    type ExternFunDefinition =
        { name: string
          type_: FunType
          dll_import: string option
          entry_point: string option
          entry_point_num: int option }

    type ModuleItem =
        | ConstDefinition of ConstDefinition<ConstExpr>
        | VarDefinition of ConstDefinition<ConstExpr>
        | FunDefinition of FunDefinition
        | ExternFunDefinition of ExternFunDefinition

    type ModuleTopLevel = ModuleTopLevel of ModuleItem list

    type Module = { name: string; definitions: ModuleTopLevel }

    type TopLevel = TopLevel of Module list


module AstConvert =

    exception AstConvertError of {| message: string |}

    let convert (cst: Cst.TopLevel) : Ast.TopLevel =

        let mapModule { Cst.Module.name = name } = { Ast.Module.name = name; Ast.Module.definitions = Ast.ModuleTopLevel []; }

        let topLevel (Cst.TopLevel modules) = Ast.TopLevel <| List.map mapModule modules

        topLevel cst
