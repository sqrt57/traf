namespace Triton

module Ast =

    type Attributes = Map<string, obj>

    let attrKey = "a"

    type Expr =
        | IntVal of int64
        | CharVal of char
        | BoolVal of bool
        | StringVal of string
        | Ref of string
        | Null
        | Length of ExprWithAttr
        | SizeOf of ExprWithAttr
        | AddressOf of ExprWithAttr
        | Negate of ExprWithAttr
        | FunCall of func: ExprWithAttr * arguments: ExprWithAttr list
        | Operator of left: ExprWithAttr * op: string * right: ExprWithAttr
    and ExprWithAttr = { expr: Expr; attrs: Attributes; }

    type Type =
        | TypeRef of name: string
        | Array of type_: TypeWithAttr * size: ExprWithAttr option
        | Pointer of type_: TypeWithAttr
        | Fun of FunType
        | Tuple of TypeTuple
    and TypeTuple = (string option * TypeWithAttr) list
    and FunType = { arguments: TypeTuple; result: TypeTuple; attrs: Attributes }
    and TypeWithAttr = { type_: Type; attrs: Attributes; }

    type ConstDefinition = { name: string; type_: TypeWithAttr; value: ExprWithAttr; }

    type VarDefinition = { name: string; type_: TypeWithAttr; value: ExprWithAttr option; }

    type Assignment = { name: string; value: ExprWithAttr; }

    type Statement =
        | ConstStatement of ConstDefinition
        | VarStatement of VarDefinition
        | Assignment of Assignment
        | Expression of ExprWithAttr
    type StatementWithAttr = { statement: Statement; attrs: Attributes; }

    type FunBody = StatementWithAttr list

    type FunAttributes = { entry: bool }

    type FunDefinition =
        { name: string
          type_: FunType
          body: FunBody
          funAttributes: FunAttributes }

    type ExternFunAttributes =
        { dll_import: string option
          dll_entry_point_name: string option
          dll_entry_point_ordinal: int64 option }

    type ExternFunDefinition =
        { name: string
          type_: FunType
          funAttributes: ExternFunAttributes }

    type ModuleItem =
        | ConstDefinition of ConstDefinition
        | VarDefinition of VarDefinition
        | FunDefinition of FunDefinition
        | ExternFunDefinition of ExternFunDefinition
    type ModuleItemWithAttr = { moduleItem: ModuleItem; attrs: Attributes; }

    type Module =
        { name: string; definitions: ModuleItemWithAttr list; attrs: Attributes; }

    type TopLevel =
        TopLevel of Module list

module AstCreate =

    open Ast

    let intExpr attr value : ExprWithAttr =
        { expr = IntVal value; attrs = Map.add attrKey (attr :> obj) Map.empty }
    let charExpr attr value : ExprWithAttr =
        { expr = CharVal value; attrs = Map.add attrKey (attr :> obj) Map.empty }
    let boolExpr attr value : ExprWithAttr =
        { expr = BoolVal value; attrs = Map.add attrKey (attr :> obj) Map.empty; }
    let stringExpr attr value : ExprWithAttr =
        { expr = StringVal value; attrs = Map.add attrKey (attr :> obj) Map.empty; }
    let refExpr attr name : ExprWithAttr =
        { expr = Ref name; attrs = Map.add attrKey (attr :> obj) Map.empty; }
    let nullExpr attr : ExprWithAttr =
        { expr = Null; attrs = Map.add attrKey (attr :> obj) Map.empty; }
    let lengthExpr attr arg : ExprWithAttr =
        { expr = Length arg; attrs = Map.add attrKey (attr :> obj) Map.empty; }
    let sizeOfExpr attr arg : ExprWithAttr =
        { expr = SizeOf arg; attrs = Map.add attrKey (attr :> obj) Map.empty; }
    let addressOfExpr attr arg : ExprWithAttr =
        { expr = AddressOf arg; attrs = Map.add attrKey (attr :> obj) Map.empty; }
    let negateExpr attr arg : ExprWithAttr =
        { expr = Negate arg; attrs = Map.add attrKey (attr :> obj) Map.empty; }
    let funCallExpr attr func args : ExprWithAttr =
        { expr = FunCall (func = func, arguments = args); attrs = Map.add attrKey (attr :> obj) Map.empty; }
    let operatorExpr attr left op right : ExprWithAttr =
        { expr = Operator (left = left, op = op, right = right); attrs = Map.add attrKey (attr :> obj) Map.empty; }

    let refType attr name : TypeWithAttr =
        { type_ = TypeRef (name = name); attrs = Map.add attrKey (attr :> obj) Map.empty; }
    let arrayType attr type_ size : TypeWithAttr =
        { type_ = Array (type_ = type_, size = size); attrs = Map.add attrKey (attr :> obj) Map.empty; }
    let pointerType attr type_ : TypeWithAttr =
        { type_ = Pointer (type_ = type_); attrs = Map.add attrKey (attr :> obj) Map.empty; }
    let private argType (name, type_) = (name, type_)
    let funType attr args result : TypeWithAttr =
        { type_ = Fun { arguments = List.map argType args
                        result = List.map argType result
                        attrs = Map.add attrKey (attr :> obj) Map.empty }
          attrs = Map.add attrKey (attr :> obj) Map.empty }
    let funDefType attr args result =
        { arguments = List.map argType args
          result = List.map argType result
          attrs = Map.add attrKey (attr :> obj) Map.empty }

    let constStmt attr name type_ value : StatementWithAttr =
        { statement = ConstStatement { name = name; type_ = type_; value = value; }
          attrs = Map.add attrKey (attr :> obj) Map.empty }
    let varStmt attr name type_ value : StatementWithAttr =
        { statement = VarStatement { name = name; type_ = type_; value = value;  }
          attrs = Map.add attrKey (attr :> obj) Map.empty }
    let assignStmt attr name value : StatementWithAttr =
        { statement = Assignment { name = name; value = value; }
          attrs = Map.add attrKey (attr :> obj) Map.empty }
    let exprStmt attr value : StatementWithAttr =
        { statement = Expression value
          attrs = Map.add attrKey (attr :> obj) Map.empty }

    let constDef attr name type_ value : ModuleItemWithAttr =
        { moduleItem = ConstDefinition { name = name; type_ = type_; value = value; }
          attrs = Map.add attrKey (attr :> obj) Map.empty }
    let varDef attr name type_ value : ModuleItemWithAttr =
        { moduleItem = VarDefinition { name = name; type_ = type_; value = value; }
          attrs = Map.add attrKey (attr :> obj) Map.empty }
    let funDef attr name attrs type_ stmts : ModuleItemWithAttr =
        { moduleItem = FunDefinition { name = name; type_ = type_; body = stmts; funAttributes = attrs; }
          attrs = Map.add attrKey (attr :> obj) Map.empty }
    let externFunDef attr name attrs type_ : ModuleItemWithAttr =
        { moduleItem = ExternFunDefinition { name = name; type_ = type_; funAttributes = attrs; }
          attrs = Map.add attrKey (attr :> obj) Map.empty }

    let module_ attr name definitions = { name = name; definitions = definitions; attrs = Map.add attrKey (attr :> obj) Map.empty; }

    let topLevel modules = TopLevel modules

module AstRead =

    open Ast

    let typeAttr ({ attrs = attrs }: TypeWithAttr) = (Map.find attrKey attrs) :?> 'type_

    let funTypeAttr ({ attrs = attrs }: FunType) = (Map.find attrKey attrs) :?> 'type_

    let exprAttr ({ attrs = attrs }: ExprWithAttr) = (Map.find attrKey attrs) :?> 'a

    let stmtAttr ({ attrs = attrs }: StatementWithAttr) = (Map.find attrKey attrs) :?> 'a

    let defAttr ({ attrs = attrs }: ModuleItemWithAttr) = (Map.find attrKey attrs) :?> 'a

    let moduleAttr (module_: Module) = (Map.find attrKey module_.attrs) :?> 'modul

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
        abstract member defFun: name: string -> type_: 'targetType -> attrs: FunAttributes -> statements: 'targetStmt list -> childContext: 'funContext -> context: 'moduleContext -> source: 'sourceDef -> 'moduleContext * 'targetDef

        abstract member defExternFunType: context: 'moduleContext -> source: 'sourceDef -> 'typeContext
        abstract member defExternFun: name: string -> type_: 'targetType -> attrs: ExternFunAttributes -> context: 'moduleContext -> source: 'sourceDef -> 'moduleContext * 'targetDef

        // Module
        abstract member topModuleBody: context: 'topLevelContext -> source: 'sourceModule -> 'moduleContext
        abstract member topModule: name: string -> definitions: 'targetDef list -> childContext: 'moduleContext -> context: 'topLevelContext -> source: 'sourceModule -> 'topLevelContext * 'targetModule

    let visitAst<'sourceModule, 'sourceDef, 'sourceStmt, 'sourceExpr, 'sourceType,
                 'targetModule, 'targetDef, 'targetStmt, 'targetExpr, 'targetType,
                 'topLevelContext, 'moduleContext, 'funContext, 'exprContext, 'typeContext>
            (visitor: IAstVisitor<'sourceModule, 'sourceDef, 'sourceStmt, 'sourceExpr, 'sourceType,
                                  'targetModule, 'targetDef, 'targetStmt, 'targetExpr, 'targetType,
                                  'topLevelContext, 'moduleContext, 'funContext, 'exprContext, 'typeContext>)
            (ast: TopLevel)
            (context: 'topLevelContext)
            : 'topLevelContext * TopLevel =

        let rec toExpr context ({ expr = expr; attrs = attrs; }: ExprWithAttr) =
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
            | Expr.FunCall (func = func; arguments = arguments) ->
                let childContext = visitor.exprFunCallChild context source
                let func = toExpr childContext func
                let arguments = List.map (toExpr childContext) arguments
                funCallExpr (visitor.exprFunCall (exprAttr func) (List.map exprAttr arguments) context source) func arguments
            | Expr.Operator (left = left; op = op; right = right) ->
                let childContext = visitor.exprOperatorChild context source
                let left = toExpr childContext left
                let right = toExpr childContext right
                operatorExpr (visitor.exprOperator (exprAttr left) op (exprAttr right) context source) left op right

        let mapTypeArg f (name, t) = (name, f t)
        let mapTypeTuple f tt = List.map (mapTypeArg f) tt

        let rec toType context ({ type_ = type_ }: TypeWithAttr as typeWithAttr) =
            let source = typeAttr typeWithAttr
            match type_ with
            | TypeRef (name = name) ->
                let target = visitor.typeRef name context source
                refType target name
            | Array (type_ = type_; size = size) ->
                let exprContext = visitor.typeArraySize context (typeAttr type_)
                let type_ = toType context type_
                let size = Option.map (toExpr exprContext) size
                let target = visitor.typeArray (typeAttr type_) (Option.map exprAttr size) context source
                arrayType target type_ size
            | Pointer (type_ = type_) ->
                let type_ = toType context type_
                let target = visitor.typePointer (typeAttr type_) context source
                pointerType target type_
            | Fun { arguments = args; result = result; } ->
                let args = mapTypeTuple (toType context) args
                let result = mapTypeTuple (toType context) result
                let argsAttr = mapTypeTuple typeAttr args
                let resultAttr = mapTypeTuple typeAttr result
                let target = visitor.typeFun argsAttr resultAttr context source
                funType target args result
            | Tuple _ -> raise (System.InvalidOperationException())

        let toFunType context type_ =
            let { arguments = args; result = result; } = type_
            let args = mapTypeTuple (toType context) args
            let result = mapTypeTuple (toType context) result
            let argsAttr = mapTypeTuple typeAttr args
            let resultAttr = mapTypeTuple typeAttr result
            let source = funTypeAttr type_
            let target = visitor.typeFunDef argsAttr resultAttr context source
            funDefType target args result

        let toStatement (context, acc) ({ statement = statement }: StatementWithAttr as statementWithAttr) =
            let attr = stmtAttr statementWithAttr
            match statement with
            | ConstStatement { name = name; type_ = type_; value = value; } ->
                let childContext = visitor.stmtConstValue context attr
                let typeContext = visitor.stmtConstType context attr
                let type_ = toType typeContext type_
                let typeAttr = typeAttr type_
                let value = toExpr childContext value
                let childAttr = exprAttr value
                let context, expr = visitor.stmtConst name typeAttr childAttr context attr
                (context, constStmt expr name type_ value :: acc)
            | VarStatement { name = name; type_ = type_; value = value; } ->
                let childContext = visitor.stmtVarValue context attr
                let typeContext = visitor.stmtVarType context attr
                let type_ = toType typeContext type_
                let typeAttr = typeAttr type_
                let value = Option.map (toExpr childContext) value
                let childAttr = Option.map exprAttr value
                let context, expr = visitor.stmtVar name typeAttr childAttr context attr
                (context, varStmt expr name type_ value :: acc)
            | Assignment { name = name; value = value; } ->
                let childContext = visitor.stmtAssignValue context attr
                let value = toExpr childContext value
                let childAttr = exprAttr value
                let context, expr = visitor.stmtAssign name childAttr context attr
                (context, assignStmt expr name value :: acc)
            | Expression value ->
                let childContext = visitor.stmtExprValue context attr
                let value = toExpr childContext value
                let childAttr = exprAttr value
                let context, expr = visitor.stmtExpr childAttr context attr
                (context, exprStmt expr value :: acc)

        let toModuleItem (context, acc) ({ moduleItem = moduleItem }: ModuleItemWithAttr as moduleItemWithAttr) =
            let source = defAttr moduleItemWithAttr
            match moduleItem with
            | ConstDefinition { name = name; type_ = type_; value = value; } ->
                let valueContext = visitor.defConstValue context source
                let typeContext = visitor.defConstType context source
                let value = toExpr valueContext value
                let type_ = toType typeContext type_
                let typeAttr = typeAttr type_
                let childAttr = exprAttr value
                let context, target = visitor.defConst name typeAttr childAttr context source
                (context, constDef target name type_ value :: acc)
            | VarDefinition { name = name; type_ = type_; value = value; } ->
                let valueContext = visitor.defVarValue context source
                let typeContext = visitor.defVarType context source
                let value = Option.map (toExpr valueContext) value
                let type_ = toType typeContext type_
                let childAttr = Option.map exprAttr value
                let typeAttr = typeAttr type_
                let context, target = visitor.defVar name typeAttr childAttr context source
                (context, varDef target name type_ value :: acc)
            | FunDefinition { name = name; type_ = type_; body = statements; funAttributes = funAttributes; } ->
                let bodyContext = visitor.defFunBody context source
                let typeContext = visitor.defFunType context source
                let type_ = toFunType typeContext type_
                let typeAttr = funTypeAttr type_
                let bodyContext, statementsRev = List.fold toStatement (bodyContext, []) statements
                let statements = List.rev statementsRev
                let childAttrs = List.map stmtAttr statements
                let context, target = visitor.defFun name typeAttr funAttributes childAttrs bodyContext context source
                (context, funDef target name funAttributes type_ statements :: acc)
            | ExternFunDefinition { name = name; type_ = type_; funAttributes = funAttributes; } ->
                let typeContext = visitor.defExternFunType context source
                let type_ = toFunType typeContext type_
                let typeAttr = funTypeAttr type_
                let context, target = visitor.defExternFun name typeAttr funAttributes context source
                (context, externFunDef target name funAttributes type_ :: acc)

        let toModule (context, acc) srcModule =
            let sourceModuleAttr = (Map.find attrKey srcModule.attrs) :?> 'sourceModule
            let childContext = visitor.topModuleBody context sourceModuleAttr
            let childContext, definitionsRev = List.fold toModuleItem (childContext, []) srcModule.definitions
            let context, targetAttr = visitor.topModule srcModule.name [] childContext context sourceModuleAttr
            (context, (module_ targetAttr srcModule.name (List.rev definitionsRev)) :: acc)

        let topLevel context (TopLevel modules) =
            let context, modulesRev = List.fold toModule (context, []) modules
            in (context, topLevel (List.rev modulesRev))

        topLevel context ast
