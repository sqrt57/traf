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


module AstConvert =

    exception AstConvertError of {| message: string |}

    let rec private toConstExpr expr =
        match expr with
        | Cst.IntVal i -> Ast.ConstExprAttr (Ast.ConstExpr.IntVal i, ())
        | Cst.CharVal c -> Ast.ConstExprAttr (Ast.ConstExpr.CharVal c, ())
        | Cst.BoolVal b -> Ast.ConstExprAttr (Ast.ConstExpr.BoolVal b, ())
        | Cst.StringVal s -> Ast.ConstExprAttr (Ast.ConstExpr.StringVal s, ())
        | Cst.Ref r -> Ast.ConstExprAttr (Ast.ConstExpr.Ref r, ())
        | Cst.Null -> Ast.ConstExprAttr (Ast.ConstExpr.Null, ())
        | Cst.AddressOf expr -> Ast.ConstExprAttr (Ast.ConstExpr.AddressOf (toConstExpr expr), ())
        | Cst.Negate (Cst.IntVal i) -> Ast.ConstExprAttr (Ast.ConstExpr.IntVal (-i), ())
        | Cst.Negate expr -> Ast.ConstExprAttr (Ast.ConstExpr.Negate (toConstExpr expr), ())
        | Cst.FunCall { func = Cst.Ref "length"; arguments = [arg] } ->
            Ast.ConstExprAttr (Ast.ConstExpr.Length (toConstExpr arg), ())
        | Cst.FunCall { func = Cst.Ref "length"; arguments = _ } ->
            raise (AstConvertError {| message = "length() accepts exactly one argument" |})
        | Cst.FunCall { func = Cst.Ref "sizeof"; arguments = [arg] } ->
            Ast.ConstExprAttr (Ast.ConstExpr.SizeOf (toConstExpr arg), ())
        | Cst.FunCall { func = Cst.Ref "sizeof"; arguments = _ } ->
            raise (AstConvertError {| message = "sizeof() accepts exactly one argument" |})
        | Cst.FunCall { func = _; arguments = _ } ->
            raise (AstConvertError {| message = "function call is illegal in constant expression" |})
        | Cst.Operator { left = left; op = op; right = right } ->
            Ast.ConstExprAttr (Ast.ConstExpr.Operator { left = toConstExpr left; op = op; right = toConstExpr right }, ())

    let rec private toExpr expr =
        match expr with
        | Cst.IntVal i -> Ast.Expr.IntVal i
        | Cst.CharVal c -> Ast.Expr.CharVal c
        | Cst.BoolVal b -> Ast.Expr.BoolVal b
        | Cst.StringVal s -> Ast.Expr.StringVal s
        | Cst.Ref r -> Ast.Expr.Ref r
        | Cst.Null -> Ast.Expr.Null
        | Cst.AddressOf expr -> Ast.Expr.AddressOf (toExpr expr)
        | Cst.Negate expr -> Ast.Expr.Negate (toExpr expr)
        | Cst.FunCall { func = Cst.Ref "length"; arguments = [arg] } ->
            Ast.Expr.Length (toExpr arg)
        | Cst.FunCall { func = Cst.Ref "length"; arguments = _ } ->
            raise (AstConvertError {| message = "length() accepts exactly one argument" |})
        | Cst.FunCall { func = Cst.Ref "sizeof"; arguments = [arg] } ->
            Ast.Expr.SizeOf (toExpr arg)
        | Cst.FunCall { func = Cst.Ref "sizeof"; arguments = _ } ->
            raise (AstConvertError {| message = "sizeof() accepts exactly one argument" |})
        | Cst.FunCall { func = func; arguments = arguments } ->
            Ast.Expr.FunCall { func = toExpr func; arguments = List.map toExpr arguments }
        | Cst.Operator { left = left; op = op; right = right } ->
            Ast.Expr.Operator { left = toExpr left; op = op; right = toExpr right }

    let rec private toType type_ =
        match type_ with
        | Cst.TypeRef s -> Ast.TypeRef s
        | Cst.Array { type_ = type_; size = size } ->
            Ast.Array { type_ = toType type_; size = Option.map toExpr size }
        | Cst.Pointer type_ -> Ast.Pointer (toType type_)
        | Cst.Fun f -> Ast.Fun (toFunType f)
        | Cst.Tuple t -> Ast.Tuple (toTypeTuple t)
    and private toTypeTupleSlot ({ name = name; type_ = type_ } : Cst.TypeTupleSlot) =
        { name = name; type_ = toType type_ } : Ast.TypeTupleSlot
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
            attributes = attributes } : Cst.FunDefinition) : Ast.FunDefinition =
        let initial : Ast.FunAttrs = { entry = false }
        let attrs = foldAttrLists fromFunAttr initial attributes
        { name = name
          type_ = toFunType type_
          body = Ast.FunBody (List.map toFunBodyItem body)
          attrs = attrs }

    let private toExternFun
        ( { name = name
            type_ = type_
            attributes = attributes } : Cst.ExternFunDefinition) : Ast.ExternFunDefinition =
        let initial : Ast.ExternFunAttrs = { dll_import = None; dll_entry_point_name = None; dll_entry_point_ordinal = None }
        let attrs = foldAttrLists fromExternFunAttr initial attributes
        { name = name
          type_ = toFunType type_
          attrs = attrs }

    let private toModuleItem moduleItem =
        match moduleItem with
        | Cst.ConstDefinition { name = name; type_ = type_; value = value } ->
            Ast.ConstDefinition { name = name; type_ = toType type_; value = toConstExpr value }
        | Cst.VarDefinition { name = name; type_ = type_; value = value } ->
            Ast.VarDefinition { name = name; type_ = toType type_; value = Option.map toConstExpr value }
        | Cst.FunDefinition f -> Ast.FunDefinition (toFun f)
        | Cst.ExternFunDefinition ef -> Ast.ExternFunDefinition (toExternFun ef)

    let private module_ (cstModule: Cst.Module) : Ast.Module<unit> =
        let (Cst.ModuleTopLevel definitions) = cstModule.definitions
        { name = cstModule.name
          definitions = Ast.ModuleTopLevel <| List.map toModuleItem definitions }

    let private topLevel (Cst.TopLevel modules) = Ast.TopLevel <| List.map module_ modules

    let convert (cst: Cst.TopLevel) : Ast.TopLevel<unit> = topLevel cst
