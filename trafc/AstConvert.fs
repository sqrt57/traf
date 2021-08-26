namespace Triton

module AstConvert =

    open Error

    let rec private toExpr expr =
        match expr with
        | Cst.IntVal i -> Ast.ExprAttr (Ast.IntVal i, ())
        | Cst.CharVal c -> Ast.ExprAttr (Ast.CharVal c, ())
        | Cst.BoolVal b -> Ast.ExprAttr (Ast.BoolVal b, ())
        | Cst.StringVal s -> Ast.ExprAttr (Ast.StringVal s, ())
        | Cst.Ref r -> Ast.ExprAttr (Ast.Ref r, ())
        | Cst.Null -> Ast.ExprAttr (Ast.Null, ())
        | Cst.AddressOf expr -> Ast.ExprAttr (Ast.AddressOf (toExpr expr), ())
        | Cst.Negate expr -> Ast.ExprAttr (Ast.Negate (toExpr expr), ())
        | Cst.FunCall { func = Cst.Ref "length"; arguments = [arg] } ->
            Ast.ExprAttr (Ast.Length (toExpr arg), ())
        | Cst.FunCall { func = Cst.Ref "length"; arguments = _ } ->
            raise (AstConvertError {| message = "length() accepts exactly one argument" |})
        | Cst.FunCall { func = Cst.Ref "sizeof"; arguments = [arg] } ->
            Ast.ExprAttr (Ast.SizeOf (toExpr arg), ())
        | Cst.FunCall { func = Cst.Ref "sizeof"; arguments = _ } ->
            raise (AstConvertError {| message = "sizeof() accepts exactly one argument" |})
        | Cst.FunCall { func = func; arguments = arguments } ->
            Ast.ExprAttr (Ast.FunCall { func = toExpr func; arguments = List.map toExpr arguments }, ())
        | Cst.Operator { left = left; op = op; right = right } ->
            Ast.ExprAttr (Ast.Operator { left = toExpr left; op = op; right = toExpr right }, ())

    let rec private toType type_ =
        match type_ with
        | Cst.TypeRef s -> Ast.TypeRef s
        | Cst.Array { type_ = type_; size = size } ->
            Ast.Array { type_ = toType type_; size = Option.map toExpr size }
        | Cst.Pointer type_ -> Ast.Pointer (toType type_)
        | Cst.Fun f -> Ast.Fun (toFunType f)
        | Cst.Tuple t -> Ast.Tuple (toTypeTuple t)
    and private toTypeTupleSlot ({ name = name; type_ = type_ } : Cst.TypeTupleSlot) =
        { name = name; type_ = toType type_ } : Ast.TypeTupleSlot<unit>
    and private toTypeTuple (Cst.TypeTuple t) = Ast.TypeTuple (List.map toTypeTupleSlot t)
    and private toFunType { arguments = arguments; result = result } =
        { arguments = toTypeTuple arguments; result = toTypeTuple result }

    let private toFunBodyItem funBodyItem =
        match funBodyItem with
        | Cst.ConstStatement { name = name; type_ = type_; value = value } ->
            Ast.ConstStatement { name = name; type_ = toType type_; value = toExpr value }
        | Cst.VarStatement { name = name; type_ = type_; value = value } ->
            Ast.VarStatement { name = name; type_ = toType type_; value = Option.map toExpr value }
        | Cst.Assignment { name = name; value = value } ->
            Ast.Assignment { name = name; value = toExpr value }
        | Cst.Expression expr -> Ast.Expression (toExpr expr)

    let rec private foldAttrLists func acc (Cst.AttrLists attrLists)  =
        match attrLists with
        | Cst.AttrList ({ name = name; value = value } :: restList) :: restLists ->
            foldAttrLists func (func acc name value) (Cst.AttrLists (Cst.AttrList restList :: restLists))
        | Cst.AttrList [] :: restLists ->
            foldAttrLists func acc (Cst.AttrLists restLists)
        | [] -> acc

    let private fromFunAttr (attrs : Ast.FunAttrs) name value =
        match name, value with
        | "entry", Cst.None ->
            match attrs.entry with
            | false -> { entry = true } : Ast.FunAttrs
            | true -> raise (AstConvertError {| message = "duplicate entry function attribute" |})
        | "entry", _ -> raise (AstConvertError {| message = "entry function attribute should have no value" |})
        | a, _ -> raise (AstConvertError {| message = $"illegal function attribute: {a}" |})

    let private fromExternFunAttr (attrs : Ast.ExternFunAttrs) name value =
        match name, value with
        | "dll_import", Cst.String s ->
            match attrs.dll_import with
            | None -> { attrs with dll_import = Some s }
            | _ -> raise (AstConvertError {| message = "duplicate dll_import extern function attribute" |})
        | "dll_import", _ -> raise (AstConvertError {| message = "dll_import extern function attribute should have string value" |})
        | "dll_entry_point", Cst.String s ->
            match attrs.dll_entry_point_name, attrs.dll_entry_point_ordinal with
            | None, None -> { attrs with dll_entry_point_name = Some s }
            | _ -> raise (AstConvertError {| message = "duplicate dll_entry_point extern function attribute" |})
        | "dll_entry_point", Cst.Int i ->
            match attrs.dll_entry_point_name, attrs.dll_entry_point_ordinal with
            | None, None -> { attrs with dll_entry_point_ordinal = Some i }
            | _ -> raise (AstConvertError {| message = "duplicate dll_entry_point extern function attribute" |})
        | "dll_entry_point", _ -> raise (AstConvertError {| message = "dll_entry_point extern function attribute should have string or integer value" |})
        | a, _ -> raise (AstConvertError {| message = $"illegal extern function attribute: {a}" |})

    let private toFun
        ( { name = name
            type_ = type_
            body = Cst.FunBody body
            attributes = attributes } : Cst.FunDefinition) : Ast.FunDefinition<unit> =
        let initial : Ast.FunAttrs = { entry = false }
        let attrs = foldAttrLists fromFunAttr initial attributes
        { name = name
          type_ = toFunType type_
          body = Ast.FunBody (List.map toFunBodyItem body)
          attrs = attrs }

    let private toExternFun
        ( { name = name
            type_ = type_
            attributes = attributes } : Cst.ExternFunDefinition) : Ast.ExternFunDefinition<unit> =
        let initial : Ast.ExternFunAttrs = { dll_import = None; dll_entry_point_name = None; dll_entry_point_ordinal = None }
        let attrs = foldAttrLists fromExternFunAttr initial attributes
        { name = name
          type_ = toFunType type_
          attrs = attrs }

    let private toModuleItem moduleItem =
        match moduleItem with
        | Cst.ConstDefinition { name = name; type_ = type_; value = value } ->
            Ast.ConstDefinition { name = name; type_ = toType type_; value = toExpr value }
        | Cst.VarDefinition { name = name; type_ = type_; value = value } ->
            Ast.VarDefinition { name = name; type_ = toType type_; value = Option.map toExpr value }
        | Cst.FunDefinition f -> Ast.FunDefinition (toFun f)
        | Cst.ExternFunDefinition ef -> Ast.ExternFunDefinition (toExternFun ef)

    let private module_ (cstModule: Cst.Module) : Ast.Module<unit> =
        let (Cst.ModuleTopLevel definitions) = cstModule.definitions
        { name = cstModule.name
          definitions = Ast.ModuleTopLevel <| List.map toModuleItem definitions }

    let private topLevel (Cst.TopLevel modules) = Ast.TopLevel <| List.map module_ modules

    let convert (cst: Cst.TopLevel) : Ast.TopLevel<unit> = topLevel cst
