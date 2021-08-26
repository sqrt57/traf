namespace Triton

module Ast =

    type Expr<'exprAttr> =
        | IntVal of int64
        | CharVal of char
        | BoolVal of bool
        | StringVal of string
        | Ref of string
        | Null
        | Length of ExprAttr<'exprAttr>
        | SizeOf of ExprAttr<'exprAttr>
        | AddressOf of ExprAttr<'exprAttr>
        | Negate of ExprAttr<'exprAttr>
        | FunCall of FunCall<'exprAttr>
        | Operator of OperatorCall<'exprAttr>
    and FunCall<'exprAttr> = { func: ExprAttr<'exprAttr>; arguments: ExprAttr<'exprAttr> list; }
    and OperatorCall<'exprAttr> = { left: ExprAttr<'exprAttr>; op: string; right: ExprAttr<'exprAttr>; }
    and ExprAttr<'exprAttr> = ExprAttr of expr: Expr<'exprAttr> * attr: 'exprAttr

    type Type<'exprAttr> =
        | TypeRef of string
        | Array of ArrayType<'exprAttr>
        | Pointer of Type<'exprAttr>
        | Fun of FunType<'exprAttr>
        | Tuple of TypeTuple<'exprAttr>
    and TypeTupleSlot<'exprAttr> = { name: string option; type_: Type<'exprAttr> }
    and TypeTuple<'exprAttr> = TypeTuple of TypeTupleSlot<'exprAttr> list
    and FunType<'exprAttr> = { arguments: TypeTuple<'exprAttr>; result: TypeTuple<'exprAttr> }
    and ArrayType<'exprAttr> = { type_: Type<'exprAttr>; size: ExprAttr<'exprAttr> option }

    type ConstDefinition<'exprAttr> = { name: string; type_: Type<'exprAttr>; value: ExprAttr<'exprAttr>; }

    type VarDefinition<'exprAttr> = { name: string; type_: Type<'exprAttr>; value: ExprAttr<'exprAttr> option; }

    type Assignment<'exprAttr> = { name: string; value: ExprAttr<'exprAttr>; }

    type Statement<'exprAttr> =
        | ConstStatement of ConstDefinition<'exprAttr>
        | VarStatement of VarDefinition<'exprAttr>
        | Assignment of Assignment<'exprAttr>
        | Expression of ExprAttr<'exprAttr>

    type FunBody<'exprAttr> = FunBody of Statement<'exprAttr> list

    type FunAttrs = { entry: bool }

    type FunDefinition<'exprAttr> =
        { name: string
          type_: FunType<'exprAttr>
          body: FunBody<'exprAttr>
          attrs: FunAttrs }

    type ExternFunAttrs =
        { dll_import: string option
          dll_entry_point_name: string option
          dll_entry_point_ordinal: int64 option }

    type ExternFunDefinition<'exprAttr> =
        { name: string
          type_: FunType<'exprAttr>
          attrs: ExternFunAttrs }

    type ModuleItem<'exprAttr> =
        | ConstDefinition of ConstDefinition<'exprAttr>
        | VarDefinition of VarDefinition<'exprAttr>
        | FunDefinition of FunDefinition<'exprAttr>
        | ExternFunDefinition of ExternFunDefinition<'exprAttr>

    type ModuleTopLevel<'exprAttr> = ModuleTopLevel of ModuleItem<'exprAttr> list

    type Module<'exprAttr> = { name: string; definitions: ModuleTopLevel<'exprAttr> }

    type TopLevel<'exprAttr> = TopLevel of Module<'exprAttr> list

module AstTransform =

    open Ast

    type ILrAttribute<'source, 'target> =
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
        abstract member funCall: func: 'target -> args: 'target list -> source: 'source -> 'target
        abstract member operator: left: 'target -> op: string -> right: 'target -> source: 'source -> 'target

    let calcLrAttribute<'source, 'target>
            (attr: ILrAttribute<'source, 'target>)
            (ast: TopLevel<'source>) : TopLevel<'target> =

        let rec toExpr (ExprAttr (expr, srcAttr)) =
            match expr with
            | Expr.IntVal i -> ExprAttr (Expr.IntVal i, attr.intVal i srcAttr)
            | Expr.CharVal c -> ExprAttr (Expr.CharVal c, attr.charVal c srcAttr)
            | Expr.BoolVal b -> ExprAttr (Expr.BoolVal b, attr.boolVal b srcAttr)
            | Expr.StringVal s -> ExprAttr (Expr.StringVal s, attr.stringVal s srcAttr)
            | Expr.Ref r -> ExprAttr (Expr.Ref r, attr.reference r srcAttr)
            | Expr.Null -> ExprAttr (Expr.Null, attr.null_ srcAttr)
            | Expr.AddressOf argExpr -> 
                let argExpr = toExpr argExpr
                let (ExprAttr (_, argAttr)) = argExpr
                ExprAttr (Expr.AddressOf argExpr, attr.addressOf argAttr srcAttr)
            | Expr.Length argExpr -> 
                let argExpr = toExpr argExpr
                let (ExprAttr (_, argAttr)) = argExpr
                ExprAttr (Expr.Length argExpr, attr.length argAttr srcAttr)
            | Expr.SizeOf argExpr -> 
                let argExpr = toExpr argExpr
                let (ExprAttr (_, argAttr)) = argExpr
                ExprAttr (Expr.SizeOf argExpr, attr.sizeOf argAttr srcAttr)
            | Expr.Negate argExpr -> 
                let argExpr = toExpr argExpr
                let (ExprAttr (_, argAttr)) = argExpr
                ExprAttr (Expr.Negate argExpr, attr.negate argAttr srcAttr)
            | Expr.FunCall { func = funcExpr; arguments = argumentExprs } ->
                let funcExpr = toExpr funcExpr
                let (ExprAttr (_, funcAttr)) = funcExpr
                let argumentExprs = List.map toExpr argumentExprs
                let argumentAttrs = List.map (fun (ExprAttr (_, attr)) -> attr) argumentExprs
                ExprAttr (Expr.FunCall { func = funcExpr; arguments = argumentExprs },
                               attr.funCall funcAttr argumentAttrs srcAttr)
            | Expr.Operator { left = leftExpr; op = op; right = rightExpr } ->
                let leftExpr = toExpr leftExpr
                let (ExprAttr (_, leftAttr)) = leftExpr
                let rightExpr = toExpr rightExpr
                let (ExprAttr (_, rightAttr)) = rightExpr
                ExprAttr (Expr.Operator { left = leftExpr; op = op; right = rightExpr },
                               attr.operator leftAttr op rightAttr srcAttr)

        let rec toType typ =
            match typ with
            | TypeRef r -> TypeRef r
            | Array { type_ = type_; size = size } ->
                Array { type_ = toType type_; size = Option.map toExpr size }
            | Pointer t -> Pointer (toType t)
            | Fun t -> Fun (toFunType t)
            | Tuple tt -> Tuple (toTypeTuple tt)
        and toFunType { arguments = arguments; result = result } =
            { arguments = toTypeTuple arguments; result = toTypeTuple result }
        and toTypeTuple (TypeTuple t) = TypeTuple (List.map toTypeTupleSlot t)
        and toTypeTupleSlot ({ name = name; type_ = type_ }: TypeTupleSlot<'source>) =
            { name = name; type_ = toType type_ }

        let toStatement statement =
            match statement with
            | ConstStatement { name = name; type_ = type_; value = value; } ->
                ConstStatement { name = name; type_ = toType type_; value = toExpr value; }
            | VarStatement { name = name; type_ = type_; value = value; } ->
                VarStatement { name = name; type_ = toType type_; value = Option.map toExpr value; }
            | Assignment { name = name; value = value; } ->
                Assignment { name = name; value = toExpr value; }
            | Expression e -> Expression (toExpr e)

        let toBody (FunBody statements) = FunBody (List.map toStatement statements)

        let toModuleItem moduleItem =
            match moduleItem with
            | ConstDefinition { name = name; type_ = type_; value = value } ->
                ConstDefinition { name = name; type_ = toType type_; value = toExpr value }
            | VarDefinition { name = name; type_ = type_; value = value } ->
                VarDefinition { name = name; type_ = toType type_; value = Option.map toExpr value }
            | FunDefinition { name = name; type_ = type_; body = body; attrs = attrs } ->
                FunDefinition { name = name; type_ = toFunType type_; body = toBody body; attrs = attrs }
            | ExternFunDefinition { name = name; type_ = type_; attrs = attrs } ->
                ExternFunDefinition { name = name; type_ = toFunType type_; attrs = attrs }

        let module_ cstModule =
            let (ModuleTopLevel definitions) = cstModule.definitions
            { name = cstModule.name
              definitions = ModuleTopLevel <| List.map toModuleItem definitions }

        let topLevel (TopLevel modules) = TopLevel <| List.map module_ modules

        topLevel ast
