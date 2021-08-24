namespace Triton

module Cst =

    type Expr =
        | IntVal of int64
        | CharVal of char
        | BoolVal of bool
        | StringVal of string
        | Ref of string
        | Null
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

    type AttrValue =
        | None
        | String of string
        | Int of int64
    type AttrDefinition = { name: string; value: AttrValue }
    type AttrList = AttrList of AttrDefinition list
    type AttrLists = AttrLists of AttrList list

    type ConstDefinition = { name: string; type_: Type; value: Expr; }
    type VarDefinition = { name: string; type_: Type; value: Expr option; }

    type Assignment = { name: string; value: Expr; }

    type Statement =
        | ConstStatement of ConstDefinition
        | VarStatement of VarDefinition
        | Assignment of Assignment
        | Expression of Expr
    type FunBody = FunBody of Statement list

    type FunDefinition =
        { name: string
          type_: FunType
          body: FunBody
          attributes: AttrLists }

    type ExternFunDefinition =
        { name: string
          type_: FunType
          attributes: AttrLists }

    type ModuleItem =
        | ConstDefinition of ConstDefinition
        | VarDefinition of VarDefinition
        | FunDefinition of FunDefinition
        | ExternFunDefinition of ExternFunDefinition

    type ModuleTopLevel = ModuleTopLevel of ModuleItem list

    type Module = { name: string; definitions: ModuleTopLevel }

    type TopLevel = TopLevel of Module list
