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

module AstTransform =

    open Ast

    type IBottomFold<'topLevel, 'modul, 'modulDef, 'statement, 'constExpr, 'expr, 'funType, 'typ,
                     'topLevelAttr, 'moduleAttr,
                     'constDefAttr, 'varDefAttr, 'externFunDefAttr, 'funDefAttr,
                     'constStatementAttr, 'varStatementAttr, 'assignmentStatementAttr, 'exprStatementAttr,
                     'funTypeAttr, 'typeAttr, 'constExprAttr, 'exprAttr> =

        abstract member topLevel: modules: 'modul list  -> attr: 'topLevelAttr -> 'topLevel
        abstract member modul: name: string -> definitions: 'modulDef list -> attr: 'moduleItem -> 'modul

        abstract member constDef: name: string -> typ: 'typ -> value: 'constExpr ->
                                  attr: 'constDefAttr -> 'moduleDef
        abstract member varDef: name: string -> typ: 'typ -> value: 'constExpr option ->
                                attr: 'varDefAttr -> 'moduleDef
        abstract member externFunDef: name: string -> typ: 'funType -> attributes: ExternFunAttrs ->
                                      attr: 'externFunDefAttr -> 'moduleDef
        abstract member funDef: name: string -> typ: 'funType -> attributes: FunAttrs -> body: 'statement list ->
                                      attr: 'funDefAttr -> 'moduleDef

        abstract member constStatement: name: string -> typ: 'typ -> value: 'expr ->
                                        attr: 'constStatementAttr -> 'statement
        abstract member varStatement: name: string -> typ: 'typ -> value: 'expr option ->
                                      attr: 'varStatementAttr -> 'statement
        abstract member assignmentStatement: name: string -> value: 'expr ->
                                             attr: 'assignmentStatementAttr -> 'statement
        abstract member exprStatement: value: 'expr ->
                                       attr: 'exprStatementAttr -> 'statement


        abstract member funSignature: arguments: (string option * 'typ) list -> result: (string option * 'typ) list ->
                                      attr: 'funTypeAttr -> 'funType
        abstract member typeRef: name: string -> attr: 'typeAttr -> 'typ
        abstract member arrayType: elementType: 'typ -> size: 'expr option ->
                                   attr: 'typeAttr -> 'typ
        abstract member pointerType: elementType: 'typ -> attr: 'typeAttr -> 'typ
        abstract member funType: arguments: (string option * 'typ) list -> result: (string option * 'typ) list ->
                                 attr: 'typeAttr -> 'typ

        abstract member constIntVal: value: int64 -> attr: 'constExprAttr -> 'constExpr
        abstract member constCharVal: value: char -> attr: 'constExprAttr -> 'constExpr
        abstract member constBoolVal: value: bool -> attr: 'constExprAttr -> 'constExpr
        abstract member constStringVal: value: string -> attr: 'constExprAttr -> 'constExpr
        abstract member constReference: value: string -> attr: 'constExprAttr -> 'constExpr
        abstract member constNull_: attr: 'constExprAttr -> 'constExpr
        abstract member constLength: arg: 'constExpr -> attr: 'constExprAttr -> 'constExpr
        abstract member constSizeOf: arg: 'constExpr -> attr: 'constExprAttr -> 'constExpr
        abstract member constAddressOf: arg: 'constExpr -> attr: 'constExprAttr -> 'constExpr
        abstract member constNegate: arg: 'constExpr -> attr: 'constExprAttr -> 'constExpr
        abstract member constOperator: left: 'constExpr -> op: string -> right: 'constExpr -> attr: 'constExprAttr -> 'constExpr

        abstract member intVal: value: int64 -> attr: 'exprAttr -> 'expr
        abstract member charVal: value: char -> attr: 'exprAttr -> 'expr
        abstract member boolVal: value: bool -> attr: 'exprAttr -> 'expr
        abstract member stringVal: value: string -> attr: 'exprAttr -> 'expr
        abstract member reference: value: string -> attr: 'exprAttr -> 'expr
        abstract member null_: attr: 'exprAttr -> 'expr
        abstract member length: arg: 'expr -> attr: 'exprAttr -> 'expr
        abstract member sizeOf: arg: 'expr -> attr: 'exprAttr -> 'expr
        abstract member addressOf: arg: 'expr -> attr: 'exprAttr -> 'expr
        abstract member negate: arg: 'expr -> attr: 'exprAttr -> 'expr
        abstract member funCall: left: 'expr -> op: string -> right: 'expr -> attr: 'exprAttr -> 'expr
        abstract member operator: left: 'expr -> op: string -> right: 'expr -> attr: 'exprAttr -> 'expr

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

    let synthesizeAttr<'source, 'target>
            (attr: ISynthesizedAttribute<'source, 'target>)
            (ast: TopLevel<'source>) : TopLevel<'target> =
        let rec constExpr (ConstExprAttr (expr, srcAttr)) =
            match expr with
            | ConstExpr.IntVal i -> ConstExprAttr (ConstExpr.IntVal i, attr.intVal i srcAttr)
            | ConstExpr.CharVal c -> ConstExprAttr (ConstExpr.CharVal c, attr.charVal c srcAttr)
            | ConstExpr.BoolVal b -> ConstExprAttr (ConstExpr.BoolVal b, attr.boolVal b srcAttr)
            | ConstExpr.StringVal s -> ConstExprAttr (ConstExpr.StringVal s, attr.stringVal s srcAttr)
            | ConstExpr.Ref r -> ConstExprAttr (ConstExpr.Ref r, attr.reference r srcAttr)
            | ConstExpr.Null -> ConstExprAttr (ConstExpr.Null, attr.null_ srcAttr)
            | ConstExpr.AddressOf argExpr -> 
                let argExpr = constExpr argExpr
                let (ConstExprAttr (_, argAttr)) = argExpr
                ConstExprAttr (ConstExpr.AddressOf argExpr, attr.addressOf argAttr srcAttr)
            | ConstExpr.Length argExpr -> 
                let argExpr = constExpr argExpr
                let (ConstExprAttr (_, argAttr)) = argExpr
                ConstExprAttr (ConstExpr.Length argExpr, attr.addressOf argAttr srcAttr)
            | ConstExpr.SizeOf argExpr -> 
                let argExpr = constExpr argExpr
                let (ConstExprAttr (_, argAttr)) = argExpr
                ConstExprAttr (ConstExpr.SizeOf argExpr, attr.addressOf argAttr srcAttr)
            | ConstExpr.Negate argExpr -> 
                let argExpr = constExpr argExpr
                let (ConstExprAttr (_, argAttr)) = argExpr
                ConstExprAttr (ConstExpr.Negate argExpr, attr.addressOf argAttr srcAttr)
            | ConstExpr.Operator { left = leftExpr; op = op; right = rightExpr } ->
                let leftExpr = constExpr leftExpr
                let (ConstExprAttr (_, leftAttr)) = leftExpr
                let rightExpr = constExpr rightExpr
                let (ConstExprAttr (_, rightAttr)) = rightExpr
                ConstExprAttr (ConstExpr.Operator { left = leftExpr; op = op; right = rightExpr },
                               attr.operator leftAttr op rightAttr srcAttr)

        let toModuleItem moduleItem =
            match moduleItem with
            | ConstDefinition { name = name; type_ = type_; value = value } ->
                ConstDefinition { name = name; type_ = type_; value = constExpr value }
            | VarDefinition { name = name; type_ = type_; value = value } ->
                VarDefinition { name = name; type_ = type_; value = Option.map constExpr value }
            | FunDefinition f -> FunDefinition f
            | ExternFunDefinition ef -> ExternFunDefinition ef

        let module_ cstModule =
            let (ModuleTopLevel definitions) = cstModule.definitions
            { name = cstModule.name
              definitions = ModuleTopLevel <| List.map toModuleItem definitions }

        let topLevel (TopLevel modules) = TopLevel <| List.map module_ modules

        topLevel ast
