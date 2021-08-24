namespace Triton

module AstTransform =

    open Ast
    open AstTransformer

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
