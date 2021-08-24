namespace Triton

module Types =

    type LangType =
        | None
        | Bool
        | Int
        | Char
        | String
        | Pointer

module MarkTypes =

    exception TypeError of {| message: string |}

    open Ast
    open Types

    let rec private constExpr (ConstExprAttr (expr, _)) =
        match expr with
        | ConstExpr.IntVal i -> ConstExprAttr (ConstExpr.IntVal i, Int)
        | ConstExpr.CharVal c -> ConstExprAttr (ConstExpr.CharVal c, Char)
        | ConstExpr.BoolVal b -> ConstExprAttr (ConstExpr.BoolVal b, Bool)
        | ConstExpr.StringVal s -> ConstExprAttr (ConstExpr.StringVal s, String)
        | ConstExpr.Ref r -> ConstExprAttr (ConstExpr.Ref r, None)
        | ConstExpr.Null -> ConstExprAttr (ConstExpr.Null, Pointer)
        | ConstExpr.AddressOf exprAttr -> ConstExprAttr (ConstExpr.AddressOf (constExpr exprAttr), Pointer)
        | ConstExpr.Negate exprAttr ->
            let exprAttr = constExpr exprAttr
            let (ConstExprAttr (_, t)) = exprAttr
            match t with
            | Int -> ConstExprAttr (ConstExpr.Negate exprAttr, Int)
            | _ -> raise (TypeError {| message = "Unary negation can be only applied to integers" |})
        | ConstExpr.Length expr -> ConstExprAttr (ConstExpr.Length (constExpr expr), Int)
        | ConstExpr.SizeOf expr -> ConstExprAttr (ConstExpr.SizeOf (constExpr expr), Int)
        | ConstExpr.Operator { left = left; op = op; right = right } ->
            ConstExprAttr (ConstExpr.Operator { left = constExpr left; op = op; right = constExpr right }, None)

    let private toModuleItem moduleItem =
        match moduleItem with
        | ConstDefinition { name = name; type_ = type_; value = value } ->
            ConstDefinition { name = name; type_ = type_; value = constExpr value }
        | VarDefinition { name = name; type_ = type_; value = value } ->
            VarDefinition { name = name; type_ = type_; value = Option.map constExpr value }
        | FunDefinition f -> FunDefinition f
        | ExternFunDefinition ef -> ExternFunDefinition ef

    let private module_ cstModule =
        let (ModuleTopLevel definitions) = cstModule.definitions
        { name = cstModule.name
          definitions = ModuleTopLevel <| List.map toModuleItem definitions }

    let private topLevel (TopLevel modules) = TopLevel <| List.map module_ modules

    let markTypes (ast: TopLevel<unit>) : TopLevel<LangType> = topLevel ast
