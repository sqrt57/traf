namespace Triton

module Ast =

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
    and ExprAttr<'expr> = ExprAttr of expr: Expr<'expr> * attr: 'expr

    type Type<'expr> =
        | TypeRef of string
        | Array of ArrayType<'expr>
        | Pointer of Type<'expr>
        | Fun of FunType<'expr>
        | Tuple of TypeTuple<'expr>
    and TypeTupleSlot<'expr> = { name: string option; type_: Type<'expr> }
    and TypeTuple<'expr> = TypeTuple of TypeTupleSlot<'expr> list
    and FunType<'expr> = { arguments: TypeTuple<'expr>; result: TypeTuple<'expr> }
    and ArrayType<'expr> = { type_: Type<'expr>; size: ExprAttr<'expr> option }

    type ConstDefinition<'itemAttr, 'expr> =
        { name: string; type_: Type<'expr>; value: ExprAttr<'expr>; attr: 'itemAttr }

    type VarDefinition<'itemAttr, 'expr> =
        { name: string; type_: Type<'expr>; value: ExprAttr<'expr> option; attr: 'itemAttr }

    type Assignment<'itemAttr, 'expr> =
        { name: string; value: ExprAttr<'expr>;  attr: 'itemAttr }

    type Statement<'stmt, 'expr> =
        | ConstStatement of ConstDefinition<'stmt, 'expr>
        | VarStatement of VarDefinition<'stmt, 'expr>
        | Assignment of Assignment<'stmt, 'expr>
        | Expression of expr: ExprAttr<'expr> * attr: 'stmt

    type FunBody<'stmt, 'expr> =
        FunBody of Statement<'stmt, 'expr> list

    type FunAttrs = { entry: bool }

    type FunDefinition<'def, 'stmt, 'expr> =
        { name: string
          type_: FunType<'expr>
          body: FunBody<'stmt, 'expr>
          attrs: FunAttrs
          attr: 'def }

    type ExternFunAttrs =
        { dll_import: string option
          dll_entry_point_name: string option
          dll_entry_point_ordinal: int64 option }

    type ExternFunDefinition<'def, 'expr> =
        { name: string
          type_: FunType<'expr>
          attrs: ExternFunAttrs
          attr: 'def}

    type ModuleItem<'def, 'stmt, 'expr> =
        | ConstDefinition of ConstDefinition<'def, 'expr>
        | VarDefinition of VarDefinition<'def, 'expr>
        | FunDefinition of FunDefinition<'def, 'stmt, 'expr>
        | ExternFunDefinition of ExternFunDefinition<'def, 'expr>

    type ModuleTopLevel<'def, 'stmt, 'expr> =
        ModuleTopLevel of ModuleItem<'def, 'stmt, 'expr> list

    type Module<'modul, 'def, 'stmt, 'expr> =
        { name: string; definitions: ModuleTopLevel<'def, 'stmt, 'expr>; attr: 'modul }

    type TopLevel<'modul, 'def, 'stmt, 'expr> =
        TopLevel of Module<'modul, 'def, 'stmt, 'expr> list

module AstCreate =

    open Ast

    let intExpr attr value =  ExprAttr (IntVal value, attr)
    let charExpr attr value =  ExprAttr (CharVal value, attr)
    let boolExpr attr value =  ExprAttr (BoolVal value, attr)
    let stringExpr attr value =  ExprAttr (StringVal value, attr)
    let refExpr attr name =  ExprAttr (Ref name, attr)
    let nullExpr attr = ExprAttr (Null, attr)
    let lengthExpr attr arg = ExprAttr(Length arg, attr)
    let sizeOfExpr attr arg = ExprAttr(SizeOf arg, attr)
    let addressOfExpr attr arg = ExprAttr(AddressOf arg, attr)
    let negateExpr attr arg = ExprAttr(Negate arg, attr)
    let funCallExpr attr func args = ExprAttr(FunCall { func = func; arguments = args; }, attr)
    let operatorExpr attr left op right = ExprAttr(Operator { left = left; op = op; right = right; }, attr)

    let refType name = TypeRef name
    let arrayType typ size = Array { type_ = typ; size = size; }
    let pointerType typ = Pointer typ
    let argType name typ = { name = name; type_ = typ; }
    let funDefType args result = { arguments = TypeTuple args; result = TypeTuple result }
    let funType args result = Fun { arguments = TypeTuple args; result = TypeTuple result }

    let constStmt attr name typ value = ConstStatement { name = name; type_ = typ; value = value; attr = attr; }
    let varStmt attr name typ value = VarStatement { name = name; type_ = typ; value = value; attr = attr; }
    let assignStmt attr name value = Assignment { name = name; value = value; attr = attr; }
    let exprStmt attr value = Expression (value, attr)

    let constDef attr name typ value = ConstDefinition { name = name; type_ = typ; value = value; attr = attr; }
    let varDef attr name typ value = VarDefinition { name = name; type_ = typ; value = value; attr = attr; }
    let funDef attr name attrs typ stmts = FunDefinition { name = name; type_ = typ; body = FunBody stmts; attrs = attrs; attr = attr; }
    let externFunDef attr name attrs typ = ExternFunDefinition { name = name; type_ = typ; attrs = attrs; attr = attr; }

    let modul attr name definitions = { name = name; definitions = ModuleTopLevel definitions; attr = attr }

    let topLevel modules = TopLevel modules

module AstRead =

    open Ast

    let exprAttr (ExprAttr (_, childAttr)) = childAttr

    let stmtAttr stmt =
        match stmt with
        | ConstStatement constStmt -> constStmt.attr
        | VarStatement varStmt -> varStmt.attr
        | Assignment assign -> assign.attr
        | Expression (_, attr) -> attr

    let defAttr def =
        match def with
        | ConstDefinition constDef -> constDef.attr
        | VarDefinition varDef -> varDef.attr
        | FunDefinition funDef -> funDef.attr
        | ExternFunDefinition externFunDef -> externFunDef.attr

    let moduleAttr (modul: Module<'modul, 'def, 'stmt, 'expr>) = modul.attr

module AstTransform =

    open Ast
    open AstCreate
    open AstRead

    type IAstVisitor<'sourceModule, 'sourceDef, 'sourceStmt, 'sourceExpr, 
                     'targetModule, 'targetDef, 'targetStmt, 'targetExpr,
                     'topLevelContext, 'moduleContext, 'funContext, 'exprContext> =
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
        abstract member stmtConstType: context: 'funContext -> source: 'sourceStmt -> 'exprContext
        abstract member stmtConst: name: string -> value: 'targetExpr -> context: 'funContext -> source: 'sourceStmt -> 'funContext * 'targetStmt

        abstract member stmtVarValue: context: 'funContext -> source: 'sourceStmt -> 'exprContext
        abstract member stmtVarType: context: 'funContext -> source: 'sourceStmt -> 'exprContext
        abstract member stmtVar: name: string -> value: 'targetExpr option -> context: 'funContext -> source: 'sourceStmt -> 'funContext * 'targetStmt

        abstract member stmtAssignValue: context: 'funContext -> source: 'sourceStmt -> 'exprContext
        abstract member stmtAssign: name: string -> value: 'targetExpr -> context: 'funContext -> source: 'sourceStmt -> 'funContext * 'targetStmt

        abstract member stmtExprValue: context: 'funContext -> source: 'sourceStmt -> 'exprContext
        abstract member stmtExpr: value: 'targetExpr -> context: 'funContext -> source: 'sourceStmt -> 'funContext * 'targetStmt

        // Definition
        abstract member defConstValue: context: 'moduleContext -> source: 'sourceDef -> 'exprContext
        abstract member defConstType: context: 'moduleContext -> source: 'sourceDef -> 'exprContext
        abstract member defConst: name: string -> value: 'targetExpr -> context: 'moduleContext -> source: 'sourceDef -> 'moduleContext * 'targetDef

        abstract member defVarValue: context: 'moduleContext -> source: 'sourceDef -> 'exprContext
        abstract member defVarType: context: 'moduleContext -> source: 'sourceDef -> 'exprContext
        abstract member defVar: name: string -> value: 'targetExpr option -> context: 'moduleContext -> source: 'sourceDef -> 'moduleContext * 'targetDef

        abstract member defFunBody: context: 'moduleContext -> source: 'sourceDef -> 'funContext
        abstract member defFunType: context: 'moduleContext -> source: 'sourceDef -> 'exprContext
        abstract member defFun: name: string -> attrs: FunAttrs -> statements: 'targetStmt list -> context: 'moduleContext -> source: 'sourceDef -> 'moduleContext * 'targetDef

        abstract member defExternFunType: context: 'moduleContext -> source: 'sourceDef -> 'exprContext
        abstract member defExternFun: name: string -> attrs: ExternFunAttrs -> context: 'moduleContext -> source: 'sourceDef -> 'moduleContext * 'targetDef

        // Module
        abstract member topModuleBody: context: 'topLevelContext -> source: 'sourceModule -> 'moduleContext
        abstract member topModule: name: string -> definitions: 'targetDef list -> context: 'topLevelContext -> source: 'sourceModule -> 'topLevelContext * 'targetModule

    let visitAst<'sourceModule, 'sourceDef, 'sourceStmt, 'sourceExpr, 
                 'targetModule, 'targetDef, 'targetStmt, 'targetExpr,
                 'topLevelContext, 'moduleContext, 'funContext, 'exprContext>
            (visitor: IAstVisitor<'sourceModule, 'sourceDef, 'sourceStmt, 'sourceExpr, 
                                  'targetModule, 'targetDef, 'targetStmt, 'targetExpr,
                                  'topLevelContext, 'moduleContext, 'funContext, 'exprContext>)
            (ast: TopLevel<'sourceModule, 'sourceDef, 'sourceStmt, 'sourceExpr>)
            (context: 'topLevelContext)
            : 'topLevelContext * TopLevel<'targetModule, 'targetDef, 'targetStmt, 'targetExpr> =

        let rec toExpr context (ExprAttr (expr, source)) =
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
                ExprAttr (Expr.SizeOf arg, visitor.exprSizeOf (exprAttr arg) context source)
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

        let rec toType context typ =
            match typ with
            | TypeRef r -> TypeRef r
            | Array { type_ = type_; size = size } ->
                Array { type_ = toType context type_; size = Option.map (toExpr context) size }
            | Pointer t -> Pointer (toType context t)
            | Fun t -> Fun (toFunType context t)
            | Tuple tt -> Tuple (toTypeTuple context tt)
        and toFunType context { arguments = arguments; result = result } =
            { arguments = toTypeTuple context arguments; result = toTypeTuple context result }
        and toTypeTuple context (TypeTuple t) = TypeTuple (List.map (toTypeTupleSlot context) t)
        and toTypeTupleSlot context ({ name = name; type_ = type_ }: TypeTupleSlot<'sourceExpr>) =
            { name = name; type_ = toType context type_ }

        let toStatement (context, acc) statement =
            match statement with
            | ConstStatement { name = name; type_ = type_; value = value; attr = attr; } ->
                let childContext = visitor.stmtConstValue context attr
                let value = toExpr childContext value
                let childAttr = exprAttr value
                let (context, expr) = visitor.stmtConst name childAttr context attr
                (context, constStmt expr name (toType childContext type_) value :: acc)
            | VarStatement { name = name; type_ = type_; value = value; attr = attr; } ->
                let childContext = visitor.stmtVarValue context attr
                let value = Option.map (toExpr childContext) value
                let childAttr = Option.map exprAttr value
                let (context, expr) = visitor.stmtVar name childAttr context attr
                (context, varStmt expr name (toType childContext type_) value :: acc)
            | Assignment { name = name; value = value; attr = attr; } ->
                let childContext = visitor.stmtAssignValue context attr
                let value = toExpr childContext value
                let childAttr = exprAttr value
                let (context, expr) = visitor.stmtAssign name childAttr context attr
                (context, assignStmt expr name value :: acc)
            | Expression (value, attr) ->
                let childContext = visitor.stmtExprValue context attr
                let value = toExpr childContext value
                let childAttr = exprAttr value
                let (context, expr) = visitor.stmtExpr childAttr context attr
                (context, exprStmt expr value :: acc)

        let toModuleItem (context, acc) moduleItem =
            match moduleItem with
            | ConstDefinition { name = name; type_ = type_; value = value; attr = source } ->
                let valueContext = visitor.defConstValue context source
                let typeContext = visitor.defConstType context source
                let value = toExpr valueContext value
                let childAttr = exprAttr value
                let (context, target) = visitor.defConst name childAttr context source
                (context, constDef target name (toType typeContext type_) value :: acc)
            | VarDefinition { name = name; type_ = type_; value = value; attr = source } ->
                let valueContext = visitor.defVarValue context source
                let typeContext = visitor.defVarType context source
                let value = Option.map (toExpr valueContext) value
                let childAttr = Option.map exprAttr value
                let (context, target) = visitor.defVar name childAttr context source
                (context, varDef target name (toType typeContext type_) value :: acc)
            | FunDefinition { name = name; type_ = type_; body = FunBody statements; attrs = attrs; attr = source; } ->
                let bodyContext = visitor.defFunBody context source
                let typeContext = visitor.defFunType context source
                let (bodyContext, statementsRev) = List.fold toStatement (bodyContext, []) statements
                let statements = List.rev statementsRev
                let childAttrs = List.map stmtAttr statements
                let (context, target) = visitor.defFun name attrs childAttrs context source
                (context, funDef target name attrs (toFunType typeContext type_) statements :: acc)
            | ExternFunDefinition { name = name; type_ = type_; attrs = attrs; attr = source } ->
                let typeContext = visitor.defFunType context source
                let (context, target) = visitor.defExternFun name attrs context source
                (context, externFunDef target name attrs (toFunType typeContext type_) :: acc)

        let toModule (context, acc) srcModule =
            let childContext = visitor.topModuleBody context srcModule.attr
            let (ModuleTopLevel definitions) = srcModule.definitions
            let (childContext, deinitionsRev) = List.fold toModuleItem (childContext, []) definitions
            let (context, targetAttr) = visitor.topModule srcModule.name [] context srcModule.attr
            (context, (modul targetAttr srcModule.name (List.rev deinitionsRev)) :: acc)

        let topLevel context (TopLevel modules) =
            let (context, modulesRev) = List.fold toModule (context, []) modules
            in (context, AstCreate.topLevel (List.rev modulesRev))

        topLevel context ast
