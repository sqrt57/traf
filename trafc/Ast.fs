namespace Triton

module Ast =

    type Attributes = Map<string, obj>

    let attrKey = "a"

    type Expr<'expr> =
        | IntVal of int64
        | CharVal of char
        | BoolVal of bool
        | StringVal of string
        | Ref of string
        | Null
        | Length of ExprAttr<'expr>
        | SizeOf of ExprAttr<'expr>
        | AddressOf of ExprAttr<'expr>
        | Negate of ExprAttr<'expr>
        | FunCall of FunCall<'expr>
        | Operator of OperatorCall<'expr>
    and FunCall<'expr> = { func: ExprAttr<'expr>; arguments: ExprAttr<'expr> list; }
    and OperatorCall<'expr> = { left: ExprAttr<'expr>; op: string; right: ExprAttr<'expr>; }
    and ExprAttr<'expr> = ExprAttr of expr: Expr<'expr> * attrs: Attributes

    type Type<'expr, 'typ> =
        | TypeRef of name: string * attrs: Attributes
        | Array of ArrayType<'expr, 'typ>
        | Pointer of typ: Type<'expr, 'typ> * attrs: Attributes
        | Fun of FunType<'expr, 'typ>
        | Tuple of 'typ * TypeTuple<'expr, 'typ>
    and TypeTupleSlot<'expr, 'typ> = { name: string option; type_: Type<'expr, 'typ> }
    and TypeTuple<'expr, 'typ> = TypeTuple of TypeTupleSlot<'expr, 'typ> list
    and FunType<'expr, 'typ> = { arguments: TypeTuple<'expr, 'typ>; result: TypeTuple<'expr, 'typ>; attrs: Attributes }
    and ArrayType<'expr, 'typ> = { type_: Type<'expr, 'typ>; size: ExprAttr<'expr> option; attrs: Attributes }

    type ConstDefinition<'itemAttr, 'expr, 'typ> =
        { name: string; type_: Type<'expr, 'typ>; value: ExprAttr<'expr>; attrs: Attributes }

    type VarDefinition<'itemAttr, 'expr, 'typ> =
        { name: string; type_: Type<'expr, 'typ>; value: ExprAttr<'expr> option; attrs: Attributes }

    type Assignment<'itemAttr, 'expr> =
        { name: string; value: ExprAttr<'expr>; attrs: Attributes }

    type Statement<'stmt, 'expr, 'typ> =
        | ConstStatement of ConstDefinition<'stmt, 'expr, 'typ>
        | VarStatement of VarDefinition<'stmt, 'expr, 'typ>
        | Assignment of Assignment<'stmt, 'expr>
        | Expression of expr: ExprAttr<'expr> * attrs: Attributes

    type FunBody<'stmt, 'expr, 'typ> =
        FunBody of Statement<'stmt, 'expr, 'typ> list

    type FunAttrs = { entry: bool }

    type FunDefinition<'def, 'stmt, 'expr, 'typ> =
        { name: string
          type_: FunType<'expr, 'typ>
          body: FunBody<'stmt, 'expr, 'typ>
          funAttributes: FunAttrs
          attrs: Attributes }

    type ExternFunAttrs =
        { dll_import: string option
          dll_entry_point_name: string option
          dll_entry_point_ordinal: int64 option }

    type ExternFunDefinition<'def, 'expr, 'typ> =
        { name: string
          type_: FunType<'expr, 'typ>
          funAttributes: ExternFunAttrs
          attrs: Attributes }

    type ModuleItem<'def, 'stmt, 'expr, 'typ> =
        | ConstDefinition of ConstDefinition<'def, 'expr, 'typ>
        | VarDefinition of VarDefinition<'def, 'expr, 'typ>
        | FunDefinition of FunDefinition<'def, 'stmt, 'expr, 'typ>
        | ExternFunDefinition of ExternFunDefinition<'def, 'expr, 'typ>

    type ModuleTopLevel<'def, 'stmt, 'expr, 'typ> =
        ModuleTopLevel of ModuleItem<'def, 'stmt, 'expr, 'typ> list

    type Module<'modul, 'def, 'stmt, 'expr, 'typ> =
        { name: string; definitions: ModuleTopLevel<'def, 'stmt, 'expr, 'typ>; attrs: Attributes; }

    type TopLevel<'modul, 'def, 'stmt, 'expr, 'typ> =
        TopLevel of Module<'modul, 'def, 'stmt, 'expr, 'typ> list

module AstCreate =

    open Ast

    let intExpr attr value =  ExprAttr (IntVal value, Map.add attrKey (attr :> obj) Map.empty)
    let charExpr attr value =  ExprAttr (CharVal value, Map.add attrKey (attr :> obj) Map.empty)
    let boolExpr attr value =  ExprAttr (BoolVal value, Map.add attrKey (attr :> obj) Map.empty)
    let stringExpr attr value =  ExprAttr (StringVal value, Map.add attrKey (attr :> obj) Map.empty)
    let refExpr attr name =  ExprAttr (Ref name, Map.add attrKey (attr :> obj) Map.empty)
    let nullExpr attr = ExprAttr (Null, Map.add attrKey (attr :> obj) Map.empty)
    let lengthExpr attr arg = ExprAttr(Length arg, Map.add attrKey (attr :> obj) Map.empty)
    let sizeOfExpr attr arg = ExprAttr(SizeOf arg, Map.add attrKey (attr :> obj) Map.empty)
    let addressOfExpr attr arg = ExprAttr(AddressOf arg, Map.add attrKey (attr :> obj) Map.empty)
    let negateExpr attr arg = ExprAttr(Negate arg, Map.add attrKey (attr :> obj) Map.empty)
    let funCallExpr attr func args = ExprAttr(FunCall { func = func; arguments = args; }, Map.add attrKey (attr :> obj) Map.empty)
    let operatorExpr attr left op right = ExprAttr(Operator { left = left; op = op; right = right; }, Map.add attrKey (attr :> obj) Map.empty)

    let refType attr name = TypeRef (name = name, attrs = Map.add attrKey (attr :> obj) Map.empty)
    let arrayType attr typ size = Array { type_ = typ; size = size; attrs = Map.add attrKey (attr :> obj) Map.empty }
    let pointerType attr typ = Pointer (typ = typ, attrs = Map.add attrKey (attr :> obj) Map.empty)
    let private argType (name, typ) = { name = name; type_ = typ; }
    let funType attr args result =
        Fun { arguments = TypeTuple (List.map argType args)
              result = TypeTuple (List.map argType result)
              attrs = Map.add attrKey (attr :> obj) Map.empty }
    let funDefType attr args result =
        { arguments = TypeTuple (List.map argType args)
          result = TypeTuple (List.map argType result)
          attrs = Map.add attrKey (attr :> obj) Map.empty }

    let constStmt attr name typ value = ConstStatement { name = name; type_ = typ; value = value; attrs = Map.add attrKey (attr :> obj) Map.empty; }
    let varStmt attr name typ value = VarStatement { name = name; type_ = typ; value = value; attrs = Map.add attrKey (attr :> obj) Map.empty; }
    let assignStmt attr name value = Assignment { name = name; value = value; attrs = Map.add attrKey (attr :> obj) Map.empty; }
    let exprStmt attr value = Expression (value, Map.add attrKey (attr :> obj) Map.empty)

    let constDef attr name typ value = ConstDefinition { name = name; type_ = typ; value = value; attrs = Map.add attrKey (attr :> obj) Map.empty; }
    let varDef attr name typ value = VarDefinition { name = name; type_ = typ; value = value; attrs = Map.add attrKey (attr :> obj) Map.empty; }
    let funDef attr name attrs typ stmts = FunDefinition { name = name; type_ = typ; body = FunBody stmts; funAttributes = attrs; attrs = Map.add attrKey (attr :> obj) Map.empty; }
    let externFunDef attr name attrs typ = ExternFunDefinition { name = name; type_ = typ; funAttributes = attrs; attrs = Map.add attrKey (attr :> obj) Map.empty; }

    let modul attr name definitions = { name = name; definitions = ModuleTopLevel definitions; attrs = Map.add attrKey (attr :> obj) Map.empty; }

    let topLevel modules = TopLevel modules

module AstRead =

    open Ast

    let typeAttr (typ : Type<'a,'b>) =
        match typ with
        | TypeRef (attrs = attrs) -> (Map.find attrKey attrs) :?> 'b
        | Array { attrs = attrs } -> (Map.find attrKey attrs) :?> 'b
        | Pointer (attrs = attrs) -> (Map.find attrKey attrs) :?> 'b
        | Fun { attrs = attrs } -> (Map.find attrKey attrs) :?> 'b
        | Tuple _ -> raise (System.InvalidOperationException())

    let funTypeAttr ({ attrs = attrs }: FunType<'expr, 'typ>) = (Map.find attrKey attrs) :?> 'typ

    let exprAttr (ExprAttr (_, attrs): ExprAttr<'a>) : 'a = (Map.find attrKey attrs) :?> 'a

    let stmtAttr (stmt: Statement<'a,'b,'c>) =
        match stmt with
        | ConstStatement constStmt -> (Map.find attrKey constStmt.attrs) :?> 'a
        | VarStatement varStmt -> (Map.find attrKey varStmt.attrs) :?> 'a
        | Assignment assign -> (Map.find attrKey assign.attrs) :?> 'a
        | Expression (attrs = attrs) -> (Map.find attrKey attrs) :?> 'a

    let defAttr (def: ModuleItem<'a,'b,'c,'d>) =
        match def with
        | ConstDefinition constDef -> (Map.find attrKey constDef.attrs) :?> 'a
        | VarDefinition varDef -> (Map.find attrKey varDef.attrs) :?> 'a
        | FunDefinition funDef -> (Map.find attrKey funDef.attrs) :?> 'a
        | ExternFunDefinition externFunDef -> (Map.find attrKey externFunDef.attrs) :?> 'a

    let moduleAttr (modul: Module<'modul, 'def, 'stmt, 'expr, 'typ>) = (Map.find attrKey modul.attrs) :?> 'modul

module AstTransform =

    open Ast
    open AstCreate
    open AstRead

    type IAstVisitor<'sourceModule, 'sourceDef, 'sourceStmt, 'sourceExpr, 'sourceType,
                     'targetModule, 'targetDef, 'targetStmt, 'targetExpr, 'targetType,
                     'topLevelContext, 'moduleContext, 'funContext, 'exprContext, 'typeContext> =
        // Type
        abstract member typeRef: name: string -> context: 'typeContext -> source: 'sourceType -> 'targetType
        abstract member typeArraySize: context: 'typeContext -> source: 'sourceType -> 'exprContext
        abstract member typeArray: arg: 'targetType -> size: 'targetExpr option -> context: 'typeContext -> source: 'sourceType -> 'targetType
        abstract member typePointer: arg: 'targetType -> context: 'typeContext -> source: 'sourceType -> 'targetType
        abstract member typeFun: args: (string option * 'targetType) list -> result: (string option * 'targetType) list ->
            context: 'typeContext -> source: 'sourceType -> 'targetType
        abstract member typeFunDef: args: (string option * 'targetType) list -> result: (string option * 'targetType) list ->
            context: 'typeContext -> source: 'sourceType -> 'targetType

        // Expression
        abstract member exprIntVal: value: int64 -> context: 'exprContext -> source: 'sourceExpr -> 'targetExpr
        abstract member exprCharVal: value: char -> context: 'exprContext -> source: 'sourceExpr -> 'targetExpr
        abstract member exprBoolVal: value: bool -> context: 'exprContext -> source: 'sourceExpr -> 'targetExpr
        abstract member exprStringVal: value: string -> context: 'exprContext -> source: 'sourceExpr -> 'targetExpr
        abstract member exprReference: value: string -> context: 'exprContext -> source: 'sourceExpr -> 'targetExpr
        abstract member exprNull: context: 'exprContext -> source: 'sourceExpr -> 'targetExpr

        abstract member exprLengthChild: context: 'exprContext -> source: 'sourceExpr -> 'exprContext
        abstract member exprLength: arg: 'targetExpr -> context: 'exprContext -> source: 'sourceExpr -> 'targetExpr

        abstract member exprSizeOfChild: context: 'exprContext -> source: 'sourceExpr -> 'exprContext
        abstract member exprSizeOf: arg: 'targetExpr -> context: 'exprContext -> source: 'sourceExpr -> 'targetExpr

        abstract member exprAddressOfChild: context: 'exprContext -> source: 'sourceExpr -> 'exprContext
        abstract member exprAddressOf: arg: 'targetExpr -> context: 'exprContext -> source: 'sourceExpr -> 'targetExpr

        abstract member exprNegateChild: context: 'exprContext -> source: 'sourceExpr -> 'exprContext
        abstract member exprNegate: arg: 'targetExpr -> context: 'exprContext -> source: 'sourceExpr -> 'targetExpr

        abstract member exprFunCallChild: context: 'exprContext -> source: 'sourceExpr -> 'exprContext
        abstract member exprFunCall: func: 'targetExpr -> args: 'targetExpr list -> context: 'exprContext -> source: 'sourceExpr -> 'targetExpr

        abstract member exprOperatorChild: context: 'exprContext -> source: 'sourceExpr -> 'exprContext
        abstract member exprOperator: left: 'targetExpr -> op: string -> right: 'targetExpr -> context: 'exprContext -> source: 'sourceExpr -> 'targetExpr

        // Statement
        abstract member stmtConstValue: context: 'funContext -> source: 'sourceStmt -> 'exprContext
        abstract member stmtConstType: context: 'funContext -> source: 'sourceStmt -> 'typeContext
        abstract member stmtConst: name: string -> type_: 'targetType -> value: 'targetExpr -> context: 'funContext -> source: 'sourceStmt -> 'funContext * 'targetStmt

        abstract member stmtVarValue: context: 'funContext -> source: 'sourceStmt -> 'exprContext
        abstract member stmtVarType: context: 'funContext -> source: 'sourceStmt -> 'typeContext
        abstract member stmtVar: name: string -> type_: 'targetType -> value: 'targetExpr option -> context: 'funContext -> source: 'sourceStmt -> 'funContext * 'targetStmt

        abstract member stmtAssignValue: context: 'funContext -> source: 'sourceStmt -> 'exprContext
        abstract member stmtAssign: name: string -> value: 'targetExpr -> context: 'funContext -> source: 'sourceStmt -> 'funContext * 'targetStmt

        abstract member stmtExprValue: context: 'funContext -> source: 'sourceStmt -> 'exprContext
        abstract member stmtExpr: value: 'targetExpr -> context: 'funContext -> source: 'sourceStmt -> 'funContext * 'targetStmt

        // Definition
        abstract member defConstValue: context: 'moduleContext -> source: 'sourceDef -> 'exprContext
        abstract member defConstType: context: 'moduleContext -> source: 'sourceDef -> 'typeContext
        abstract member defConst: name: string -> type_: 'targetType -> value: 'targetExpr -> context: 'moduleContext -> source: 'sourceDef -> 'moduleContext * 'targetDef

        abstract member defVarValue: context: 'moduleContext -> source: 'sourceDef -> 'exprContext
        abstract member defVarType: context: 'moduleContext -> source: 'sourceDef -> 'typeContext
        abstract member defVar: name: string -> type_: 'targetType -> value: 'targetExpr option -> context: 'moduleContext -> source: 'sourceDef -> 'moduleContext * 'targetDef

        abstract member defFunBody: context: 'moduleContext -> source: 'sourceDef -> 'funContext
        abstract member defFunType: context: 'moduleContext -> source: 'sourceDef -> 'typeContext
        abstract member defFun: name: string -> type_: 'targetType -> attrs: FunAttrs -> statements: 'targetStmt list -> childContext: 'funContext -> context: 'moduleContext -> source: 'sourceDef -> 'moduleContext * 'targetDef

        abstract member defExternFunType: context: 'moduleContext -> source: 'sourceDef -> 'typeContext
        abstract member defExternFun: name: string -> type_: 'targetType -> attrs: ExternFunAttrs -> context: 'moduleContext -> source: 'sourceDef -> 'moduleContext * 'targetDef

        // Module
        abstract member topModuleBody: context: 'topLevelContext -> source: 'sourceModule -> 'moduleContext
        abstract member topModule: name: string -> definitions: 'targetDef list -> childContext: 'moduleContext -> context: 'topLevelContext -> source: 'sourceModule -> 'topLevelContext * 'targetModule

    let visitAst<'sourceModule, 'sourceDef, 'sourceStmt, 'sourceExpr, 'sourceType,
                 'targetModule, 'targetDef, 'targetStmt, 'targetExpr, 'targetType,
                 'topLevelContext, 'moduleContext, 'funContext, 'exprContext, 'typeContext>
            (visitor: IAstVisitor<'sourceModule, 'sourceDef, 'sourceStmt, 'sourceExpr, 'sourceType,
                                  'targetModule, 'targetDef, 'targetStmt, 'targetExpr, 'targetType,
                                  'topLevelContext, 'moduleContext, 'funContext, 'exprContext, 'typeContext>)
            (ast: TopLevel<'sourceModule, 'sourceDef, 'sourceStmt, 'sourceExpr, 'sourceType>)
            (context: 'topLevelContext)
            : 'topLevelContext * TopLevel<'targetModule, 'targetDef, 'targetStmt, 'targetExpr, 'targetType> =

        let rec toExpr context (ExprAttr (expr, attrs)) =
            let source = (Map.find attrKey attrs) :?> 'sourceExpr
            match expr with
            | Expr.IntVal i -> intExpr (visitor.exprIntVal i context source) i
            | Expr.CharVal c -> charExpr (visitor.exprCharVal c context source) c
            | Expr.BoolVal b -> boolExpr (visitor.exprBoolVal b context source) b
            | Expr.StringVal s -> stringExpr (visitor.exprStringVal s context source) s
            | Expr.Ref r -> refExpr (visitor.exprReference r context source) r
            | Expr.Null -> nullExpr (visitor.exprNull context source)
            | Expr.AddressOf arg ->
                let childContext = visitor.exprAddressOfChild context source
                let arg = toExpr childContext arg
                addressOfExpr (visitor.exprAddressOf (exprAttr arg) context source) arg
            | Expr.Length arg ->
                let childContext = visitor.exprLengthChild context source
                let arg = toExpr childContext arg
                lengthExpr (visitor.exprLength (exprAttr arg) context source) arg
            | Expr.SizeOf arg ->
                let childContext = visitor.exprSizeOfChild context source
                let arg = toExpr childContext arg
                sizeOfExpr (visitor.exprSizeOf (exprAttr arg) context source) arg
            | Expr.Negate arg ->
                let childContext = visitor.exprNegateChild context source
                let arg = toExpr childContext arg
                negateExpr (visitor.exprNegate (exprAttr arg) context source) arg
            | Expr.FunCall { func = func; arguments = arguments } ->
                let childContext = visitor.exprFunCallChild context source
                let func = toExpr childContext func
                let arguments = List.map (toExpr childContext) arguments
                funCallExpr (visitor.exprFunCall (exprAttr func) (List.map exprAttr arguments) context source) func arguments
            | Expr.Operator { left = left; op = op; right = right } ->
                let childContext = visitor.exprOperatorChild context source
                let left = toExpr childContext left
                let right = toExpr childContext right
                operatorExpr (visitor.exprOperator (exprAttr left) op (exprAttr right) context source) left op right

        let fromTypeArg ({ name = name; type_ = type_ }: TypeTupleSlot<'sourceExpr, 'sourceType>) = (name, type_)
        let fromTypeTuple (TypeTuple tt) = List.map fromTypeArg tt

        let mapTypeArg f (name, t) = (name, f t)
        let mapTypeTuple f tt = List.map (mapTypeArg f) tt

        let rec toType context typ =
            let source = typeAttr typ
            match typ with
            | TypeRef (name = name) ->
                let target = visitor.typeRef name context source
                refType target name
            | Array { type_ = typ; size = size } ->
                let exprContext = visitor.typeArraySize context (typeAttr typ)
                let typ = toType context typ
                let size = Option.map (toExpr exprContext) size
                let target = visitor.typeArray (typeAttr typ) (Option.map exprAttr size) context source
                arrayType target typ size
            | Pointer (typ = typ) ->
                let typ = toType context typ
                let target = visitor.typePointer (typeAttr typ) context source
                pointerType target typ
            | Fun { arguments = args; result = result; } ->
                let args = mapTypeTuple (toType context) (fromTypeTuple args)
                let result = mapTypeTuple (toType context) (fromTypeTuple result)
                let argsAttr = mapTypeTuple typeAttr args
                let resultAttr = mapTypeTuple typeAttr result
                let target = visitor.typeFun argsAttr resultAttr context source
                funType target args result
            | Tuple _ -> raise (System.InvalidOperationException())

        let toFunType context typ =
            let { arguments = args; result = result; } = typ
            let args = mapTypeTuple (toType context) (fromTypeTuple args)
            let result = mapTypeTuple (toType context) (fromTypeTuple result)
            let argsAttr = mapTypeTuple typeAttr args
            let resultAttr = mapTypeTuple typeAttr result
            let source = funTypeAttr typ
            let target = visitor.typeFunDef argsAttr resultAttr context source
            funDefType target args result

        let toStatement (context, acc) statement =
            match statement with
            | ConstStatement { name = name; type_ = type_; value = value; attrs = attrs; } ->
                let attr = (Map.find attrKey attrs) :?> 'sourceStmt
                let childContext = visitor.stmtConstValue context attr
                let typeContext = visitor.stmtConstType context attr
                let type_ = toType typeContext type_
                let typeAttr = typeAttr type_
                let value = toExpr childContext value
                let childAttr = exprAttr value
                let (context, expr) = visitor.stmtConst name typeAttr childAttr context attr
                (context, constStmt expr name type_ value :: acc)
            | VarStatement { name = name; type_ = type_; value = value; attrs = attrs; } ->
                let attr = (Map.find attrKey attrs) :?> 'sourceStmt
                let childContext = visitor.stmtVarValue context attr
                let typeContext = visitor.stmtVarType context attr
                let type_ = toType typeContext type_
                let typeAttr = typeAttr type_
                let value = Option.map (toExpr childContext) value
                let childAttr = Option.map exprAttr value
                let (context, expr) = visitor.stmtVar name typeAttr childAttr context attr
                (context, varStmt expr name type_ value :: acc)
            | Assignment { name = name; value = value; attrs = attrs; } ->
                let attr = (Map.find attrKey attrs) :?> 'sourceStmt
                let childContext = visitor.stmtAssignValue context attr
                let value = toExpr childContext value
                let childAttr = exprAttr value
                let (context, expr) = visitor.stmtAssign name childAttr context attr
                (context, assignStmt expr name value :: acc)
            | Expression (value, attrs) ->
                let attr = (Map.find attrKey attrs) :?> 'sourceStmt
                let childContext = visitor.stmtExprValue context attr
                let value = toExpr childContext value
                let childAttr = exprAttr value
                let (context, expr) = visitor.stmtExpr childAttr context attr
                (context, exprStmt expr value :: acc)

        let toModuleItem (context, acc) moduleItem =
            match moduleItem with
            | ConstDefinition { name = name; type_ = type_; value = value; attrs = attrs; } ->
                let source = (Map.find attrKey attrs) :?> 'sourceDef
                let valueContext = visitor.defConstValue context source
                let typeContext = visitor.defConstType context source
                let value = toExpr valueContext value
                let type_ = toType typeContext type_
                let typeAttr = typeAttr type_
                let childAttr = exprAttr value
                let (context, target) = visitor.defConst name typeAttr childAttr context source
                (context, constDef target name type_ value :: acc)
            | VarDefinition { name = name; type_ = type_; value = value; attrs = attrs; } ->
                let source = (Map.find attrKey attrs) :?> 'sourceDef
                let valueContext = visitor.defVarValue context source
                let typeContext = visitor.defVarType context source
                let value = Option.map (toExpr valueContext) value
                let type_ = toType typeContext type_
                let childAttr = Option.map exprAttr value
                let typeAttr = typeAttr type_
                let (context, target) = visitor.defVar name typeAttr childAttr context source
                (context, varDef target name type_ value :: acc)
            | FunDefinition { name = name; type_ = type_; body = FunBody statements; funAttributes = funAttributes; attrs = attrs; } ->
                let source = (Map.find attrKey attrs) :?> 'sourceDef
                let bodyContext = visitor.defFunBody context source
                let typeContext = visitor.defFunType context source
                let type_ = toFunType typeContext type_
                let typeAttr = funTypeAttr type_
                let (bodyContext, statementsRev) = List.fold toStatement (bodyContext, []) statements
                let statements = List.rev statementsRev
                let childAttrs = List.map stmtAttr statements
                let (context, target) = visitor.defFun name typeAttr funAttributes childAttrs bodyContext context source
                (context, funDef target name funAttributes type_ statements :: acc)
            | ExternFunDefinition { name = name; type_ = type_; funAttributes = funAttributes; attrs = attrs; } ->
                let source = (Map.find attrKey attrs) :?> 'sourceDef
                let typeContext = visitor.defExternFunType context source
                let type_ = toFunType typeContext type_
                let typeAttr = funTypeAttr type_
                let (context, target) = visitor.defExternFun name typeAttr funAttributes context source
                (context, externFunDef target name funAttributes type_ :: acc)

        let toModule (context, acc) (srcModule: Module<'sourceModule, 'sourceDef, 'sourceStmt, 'sourceExpr, 'sourceType>) =
            let sourceModuleAttr = (Map.find attrKey srcModule.attrs) :?> 'sourceModule
            let childContext = visitor.topModuleBody context sourceModuleAttr
            let (ModuleTopLevel definitions) = srcModule.definitions
            let (childContext, definitionsRev) = List.fold toModuleItem (childContext, []) definitions
            let (context, targetAttr) = visitor.topModule srcModule.name [] childContext context sourceModuleAttr
            (context, (modul targetAttr srcModule.name (List.rev definitionsRev)) :: acc)

        let topLevel context (TopLevel modules) =
            let (context, modulesRev) = List.fold toModule (context, []) modules
            in (context, AstCreate.topLevel (List.rev modulesRev))

        topLevel context ast
