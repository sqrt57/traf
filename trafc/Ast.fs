namespace Triton

module Ast =

    type ConstExpr<'typ> =
        | IntVal of int64
        | CharVal of char
        | BoolVal of bool
        | StringVal of string
        | Ref of string
        | Null
        | Length of ConstExprAttr<'typ>
        | SizeOf of ConstExprAttr<'typ>
        | AddressOf of ConstExprAttr<'typ>
        | Negate of ConstExprAttr<'typ>
        | Operator of ConstOperatorCall<'typ>
    and ConstOperatorCall<'typ> = { left: ConstExprAttr<'typ>; op: string; right: ConstExprAttr<'typ>; }
    and ConstExprAttr<'typ> = ConstExprAttr of ConstExpr<'typ> * 'typ

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

    type ModuleItem<'typ> =
        | ConstDefinition of ConstDefinition<ConstExprAttr<'typ>>
        | VarDefinition of VarDefinition<ConstExprAttr<'typ>>
        | FunDefinition of FunDefinition
        | ExternFunDefinition of ExternFunDefinition

    type ModuleTopLevel<'typ> = ModuleTopLevel of ModuleItem<'typ> list

    type Module<'typ> = { name: string; definitions: ModuleTopLevel<'typ> }

    type TopLevel<'typ> = TopLevel of Module<'typ> list

module AstTransformer =

    type ISynthesizedAttribute<'source, 'target> =
        abstract member intVal: value: int64 -> source: 'source -> 'target
        abstract member charVal: value: char -> source: 'source -> 'target
        abstract member boolVal: value: bool -> source: 'source -> 'target
        abstract member stringVal: value: string -> source: 'source -> 'target
        abstract member reference: value: string -> source: 'source -> 'target
        abstract member null_: source: 'source -> 'target
        abstract member length: arg: 'target -> source: 'source -> 'target
        abstract member sizeOf: arg: 'target -> source: 'source -> 'target
        abstract member addressOf: arg: 'target -> source: 'source -> 'target
        abstract member negate: arg: 'target -> source: 'source -> 'target
        abstract member operator: left: 'target -> op: string -> right: 'target -> source: 'source -> 'target
