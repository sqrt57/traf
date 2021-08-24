namespace Triton

module Ast =

    type ConstExpr<'a> =
        | IntVal of int64
        | CharVal of char
        | BoolVal of bool
        | StringVal of string
        | Ref of string
        | Null
        | Length of ConstExprAttr<'a>
        | SizeOf of ConstExprAttr<'a>
        | AddressOf of ConstExprAttr<'a>
        | Negate of ConstExprAttr<'a>
        | Operator of ConstOperatorCall<'a>
    and ConstOperatorCall<'a> = { left: ConstExprAttr<'a>; op: string; right: ConstExprAttr<'a>; }
    and ConstExprAttr<'a> = ConstExprAttr of ConstExpr<'a> * 'a

    type Expr =
        | IntVal of int64
        | CharVal of char
        | BoolVal of bool
        | StringVal of string
        | Ref of string
        | Null
        | Length of Expr
        | SizeOf of Expr
        | AddressOf of Expr
        | Negate of Expr
        | FunCall of FunCall
        | Operator of OperatorCall
    and FunCall = { func: Expr; arguments: Expr list; }
    and OperatorCall = { left: Expr; op: string; right: Expr; }

    type Type =
        | TypeRef of string
        | Array of ArrayType
        | Pointer of Type
        | Fun of FunType
        | Tuple of TypeTuple
    and TypeTupleSlot = { name: string option; type_: Type }
    and TypeTuple = TypeTuple of TypeTupleSlot list
    and FunType = { arguments: TypeTuple; result: TypeTuple }
    and ArrayType = { type_: Type; size: Expr option }

    type ConstDefinition<'expr> = { name: string; type_: Type; value: 'expr; }

    type VarDefinition<'expr> = { name: string; type_: Type; value: 'expr option; }

    type Assignment = { name: string; value: Expr; }

    type Statement =
        | ConstStatement of ConstDefinition<Expr>
        | VarStatement of VarDefinition<Expr>
        | Assignment of Assignment
        | Expression of Expr

    type FunBody = FunBody of Statement list

    type FunAttrs = { entry: bool }

    type FunDefinition =
        { name: string
          type_: FunType
          body: FunBody
          attrs: FunAttrs }

    type ExternFunAttrs =
        { dll_import: string option
          dll_entry_point_name: string option
          dll_entry_point_ordinal: int64 option }

    type ExternFunDefinition =
        { name: string
          type_: FunType
          attrs: ExternFunAttrs }

    type ModuleItem<'a> =
        | ConstDefinition of ConstDefinition<ConstExprAttr<'a>>
        | VarDefinition of VarDefinition<ConstExprAttr<'a>>
        | FunDefinition of FunDefinition
        | ExternFunDefinition of ExternFunDefinition

    type ModuleTopLevel<'a> = ModuleTopLevel of ModuleItem<'a> list

    type Module<'a> = { name: string; definitions: ModuleTopLevel<'a> }

    type TopLevel<'a> = TopLevel of Module<'a> list
