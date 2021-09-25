namespace Triton

module AstConvert =

    open Errors
    open Cst
    open AstCreate

    type AstEmpty = Ast.TopLevel<unit, unit, unit, unit, unit>

    let rec private toExpr expr =
        match expr with
        | IntVal i -> intExpr () i
        | CharVal c -> charExpr () c
        | BoolVal b -> boolExpr () b
        | StringVal s -> stringExpr () s
        | Ref r -> refExpr () r
        | Null -> nullExpr ()
        | AddressOf arg -> addressOfExpr () (toExpr arg)
        | Negate arg -> negateExpr () (toExpr arg)
        | FunCall { func = Ref "length"; arguments = [arg] } ->
            lengthExpr () (toExpr arg)
        | FunCall { func = Ref "length"; arguments = _ } ->
            raise (AstConvertError {| message = "length() accepts exactly one argument" |})
        | FunCall { func = Ref "sizeof"; arguments = [arg] } ->
            sizeOfExpr () (toExpr arg)
        | FunCall { func = Ref "sizeof"; arguments = _ } ->
            raise (AstConvertError {| message = "sizeof() accepts exactly one argument" |})
        | FunCall { func = func; arguments = arguments } ->
            funCallExpr () (toExpr func) (List.map toExpr arguments)
        | Operator { left = left; op = op; right = right } ->
            operatorExpr () (toExpr left) op (toExpr right)

    let rec private toType type_ =
        match type_ with
        | TypeRef s -> refType () s
        | Array { type_ = type_; size = size } ->
            arrayType () (toType type_) (Option.map toExpr size)
        | Pointer type_ -> pointerType () (toType type_)
        | Fun { arguments = TypeTuple arguments; result = TypeTuple result } ->
            funType () (List.map typeTupleSlot arguments) (List.map typeTupleSlot result)
        | Tuple _ -> raise (AstConvertError {| message = "type tuple is not supported" |})
    and typeTupleSlot ({ name = name; type_ = typ; }: TypeTupleSlot) = (name, (toType typ))

    let toFunType ({ arguments = TypeTuple arguments; result = TypeTuple result }: FunType) =
        funDefType () (List.map typeTupleSlot arguments) (List.map typeTupleSlot result)

    let private toFunBodyItem funBodyItem =
        match funBodyItem with
        | ConstStatement { name = name; type_ = type_; value = value } ->
            constStmt () name (toType type_) (toExpr value)
        | VarStatement { name = name; type_ = type_; value = value } ->
            varStmt () name (toType type_) (Option.map toExpr value)
        | Assignment { name = name; value = value } ->
            assignStmt () name (toExpr value)
        | Expression expr -> exprStmt () (toExpr expr)

    let rec private foldAttrLists func acc (AttrLists attrLists)  =
        match attrLists with
        | AttrList ({ name = name; value = value } :: restList) :: restLists ->
            foldAttrLists func (func acc name value) (AttrLists (AttrList restList :: restLists))
        | AttrList [] :: restLists ->
            foldAttrLists func acc (AttrLists restLists)
        | [] -> acc

    let private fromFunAttr (attrs: Ast.FunAttrs) name value =
        match name, value with
        | "entry", NoneValue ->
            match attrs.entry with
            | false -> { entry = true } : Ast.FunAttrs
            | true -> raise (AstConvertError {| message = "duplicate entry function attribute" |})
        | "entry", _ -> raise (AstConvertError {| message = "entry function attribute should have no value" |})
        | a, _ -> raise (AstConvertError {| message = $"illegal function attribute: {a}" |})

    let private fromExternFunAttr (attrs: Ast.ExternFunAttrs) name value =
        match name, value with
        | "dll_import", String s ->
            match attrs.dll_import with
            | None -> { attrs with dll_import = Some s }
            | _ -> raise (AstConvertError {| message = "duplicate dll_import extern function attribute" |})
        | "dll_import", _ -> raise (AstConvertError {| message = "dll_import extern function attribute should have string value" |})
        | "dll_entry_point", String s ->
            match attrs.dll_entry_point_name, attrs.dll_entry_point_ordinal with
            | None, None -> { attrs with dll_entry_point_name = Some s }
            | _ -> raise (AstConvertError {| message = "duplicate dll_entry_point extern function attribute" |})
        | "dll_entry_point", Int i ->
            match attrs.dll_entry_point_name, attrs.dll_entry_point_ordinal with
            | None, None -> { attrs with dll_entry_point_ordinal = Some i }
            | _ -> raise (AstConvertError {| message = "duplicate dll_entry_point extern function attribute" |})
        | "dll_entry_point", _ -> raise (AstConvertError {| message = "dll_entry_point extern function attribute should have string or integer value" |})
        | a, _ -> raise (AstConvertError {| message = $"illegal extern function attribute: {a}" |})

    let private toFun
        ( { name = name
            type_ = type_
            body = FunBody body
            attributes = attributes }: FunDefinition) =
        let initial: Ast.FunAttrs = { entry = false }
        let attrs = foldAttrLists fromFunAttr initial attributes
        funDef () name attrs (toFunType type_) (List.map toFunBodyItem body)

    let private toExternFun
        ( { name = name
            type_ = type_
            attributes = attributes }: ExternFunDefinition) =
        let initial : Ast.ExternFunAttrs = { dll_import = None; dll_entry_point_name = None; dll_entry_point_ordinal = None }
        let attrs = foldAttrLists fromExternFunAttr initial attributes
        externFunDef () name attrs (toFunType type_)

    let private toModuleItem moduleItem =
        match moduleItem with
        | ConstDefinition { name = name; type_ = type_; value = value } ->
            constDef () name (toType type_) (toExpr value)
        | VarDefinition { name = name; type_ = type_; value = value } ->
            varDef () name (toType type_) (Option.map toExpr value)
        | FunDefinition f -> toFun f
        | ExternFunDefinition ef -> toExternFun ef

    let private toModule (cstModule: Module) =
        let (ModuleTopLevel definitions) = cstModule.definitions
        modul () cstModule.name (List.map toModuleItem definitions)

    let private topLevel (TopLevel modules) = AstCreate.topLevel (List.map toModule modules)

    let convert (cst: TopLevel) : Ast.TopLevel<unit, unit, unit, unit, unit> = topLevel cst
