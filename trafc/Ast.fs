namespace Triton

module Ast =

    type Attributes = Map<string, obj>

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

    type ModuleWithAttr =
        { name: string; definitions: ModuleItemWithAttr list; attrs: Attributes; }

    type TopLevelWithAttr =
         { modules: ModuleWithAttr list; attrs: Attributes; }

module AstCreate =

    open Ast

    let intExpr value : ExprWithAttr =
        { expr = IntVal value; attrs = Map.empty }
    let charExpr value : ExprWithAttr =
        { expr = CharVal value; attrs = Map.empty }
    let boolExpr value : ExprWithAttr =
        { expr = BoolVal value; attrs = Map.empty; }
    let stringExpr value : ExprWithAttr =
        { expr = StringVal value; attrs = Map.empty; }
    let refExpr name : ExprWithAttr =
        { expr = Ref name; attrs = Map.empty; }
    let nullExpr : ExprWithAttr =
        { expr = Null; attrs = Map.empty; }
    let lengthExpr arg : ExprWithAttr =
        { expr = Length arg; attrs = Map.empty; }
    let sizeOfExpr arg : ExprWithAttr =
        { expr = SizeOf arg; attrs = Map.empty; }
    let addressOfExpr arg : ExprWithAttr =
        { expr = AddressOf arg; attrs = Map.empty; }
    let negateExpr arg : ExprWithAttr =
        { expr = Negate arg; attrs = Map.empty; }
    let funCallExpr func args : ExprWithAttr =
        { expr = FunCall (func = func, arguments = args); attrs = Map.empty; }
    let operatorExpr left op right : ExprWithAttr =
        { expr = Operator (left = left, op = op, right = right); attrs = Map.empty; }

    let refType name : TypeWithAttr =
        { type_ = TypeRef (name = name); attrs = Map.empty; }
    let arrayType type_ size : TypeWithAttr =
        { type_ = Array (type_ = type_, size = size); attrs = Map.empty; }
    let pointerType type_ : TypeWithAttr =
        { type_ = Pointer (type_ = type_); attrs = Map.empty; }
    let private argType (name, type_) = (name, type_)
    let funType args result : TypeWithAttr =
        { type_ = Fun { arguments = List.map argType args
                        result = List.map argType result
                        attrs = Map.empty }
          attrs = Map.empty }
    let funDefType args result =
        { arguments = List.map argType args
          result = List.map argType result
          attrs = Map.empty }

    let constStmt name type_ value : StatementWithAttr =
        { statement = ConstStatement { name = name; type_ = type_; value = value; }
          attrs = Map.empty }
    let varStmt name type_ value : StatementWithAttr =
        { statement = VarStatement { name = name; type_ = type_; value = value;  }
          attrs = Map.empty }
    let assignStmt name value : StatementWithAttr =
        { statement = Assignment { name = name; value = value; }
          attrs = Map.empty }
    let exprStmt value : StatementWithAttr =
        { statement = Expression value
          attrs = Map.empty }

    let constDef name type_ value : ModuleItemWithAttr =
        { moduleItem = ConstDefinition { name = name; type_ = type_; value = value; }
          attrs = Map.empty }
    let varDef name type_ value : ModuleItemWithAttr =
        { moduleItem = VarDefinition { name = name; type_ = type_; value = value; }
          attrs = Map.empty }
    let funDef name attrs type_ stmts : ModuleItemWithAttr =
        { moduleItem = FunDefinition { name = name; type_ = type_; body = stmts; funAttributes = attrs; }
          attrs = Map.empty }
    let externFunDef name attrs type_ : ModuleItemWithAttr =
        { moduleItem = ExternFunDefinition { name = name; type_ = type_; funAttributes = attrs; }
          attrs = Map.empty }

    let module_ name definitions = { name = name; definitions = definitions; attrs = Map.empty; }
    let topLevel modules = { modules = modules; attrs = Map.empty; }

module AstRead =

    open Ast

    let typeAttr attrName ({ attrs = attrs }: TypeWithAttr) = (Map.find attrName attrs) :?> 'a

    let funTypeAttr attrName ({ attrs = attrs }: FunType) = (Map.find attrName attrs) :?> 'a

    let exprAttr attrName ({ attrs = attrs }: ExprWithAttr) = (Map.find attrName attrs) :?> 'a

    let stmtAttr attrName ({ attrs = attrs }: StatementWithAttr) = (Map.find attrName attrs) :?> 'a

    let defAttr attrName ({ attrs = attrs }: ModuleItemWithAttr) = (Map.find attrName attrs) :?> 'a

    let moduleAttr attrName (module_: ModuleWithAttr) = (Map.find attrName module_.attrs) :?> 'a
    let topAttr attrName (top: TopLevelWithAttr) = (Map.find attrName top.attrs) :?> 'a

module AstTransform =

    open Ast

    type ISynthesizedAttribute<'attr> =
        // Type
        abstract member typeRef: name: string -> attrs: Attributes -> 'attr
        abstract member typeArray: itemType: 'attr -> size: 'attr -> attrs: Attributes -> 'attr
        abstract member typePointer: itemType: 'attr -> attrs: Attributes -> 'targetType
        abstract member typeFun: args: (string option * 'attr) list -> result: (string option * 'attr) list ->
            attrs: Attributes -> 'attr
        abstract member typeFunDef: args: (string option * 'attr) list -> result: (string option * 'attr) list ->
            attrs: Attributes -> 'attr

        // Expression
        abstract member exprIntVal: value: int64 -> attrs: Attributes -> 'attr
        abstract member exprCharVal: value: char -> attrs: Attributes -> 'attr
        abstract member exprBoolVal: value: bool -> attrs: Attributes -> 'attr
        abstract member exprStringVal: value: string -> attrs: Attributes -> 'attr
        abstract member exprReference: value: string -> attrs: Attributes -> 'attr
        abstract member exprNull: attrs: Attributes -> 'attr

        abstract member exprLength: arg: 'attr -> attrs: Attributes -> 'attr
        abstract member exprSizeOf: arg: 'attr -> attrs: Attributes -> 'attr
        abstract member exprAddressOf: arg: 'attr -> attrs: Attributes -> 'attr
        abstract member exprNegate: arg: 'attr -> attrs: Attributes -> 'attr

        abstract member exprFunCall: func: 'attr -> args: 'attr list -> attrs: Attributes -> 'attr
        abstract member exprOperator: left: 'attr -> op: string -> right: 'attr -> attrs: Attributes -> 'attr

        // Statement
        abstract member stmtConst: name: string -> type_: 'attr -> value: 'attr -> attrs: Attributes -> 'attr
        abstract member stmtVar: name: string -> type_: 'attr -> value: 'attr option -> attrs: Attributes -> 'attr
        abstract member stmtAssign: name: string -> value: 'attr -> attrs: Attributes -> 'attr
        abstract member stmtExpr: value: 'attr -> attrs: Attributes -> 'attr

        // Definition
        abstract member defConst: name: string -> type_: 'attr -> value: 'attr -> attrs: Attributes -> 'attr
        abstract member defVar: name: string -> type_: 'attr -> value: 'attr option -> attrs: Attributes -> 'attr
        abstract member defFun: name: string -> attributes: FunAttributes -> type_: 'attr -> statements: 'attr list ->
            attrs: Attributes -> 'attr
        abstract member defExternFun: name: string -> attributes: ExternFunAttributes -> type_: 'attr -> attrs: Attributes -> 'attr

        // Module
        abstract member topModule: name: string -> definitions: 'attr list -> attrs: Attributes -> 'attr

    let calculateSynthesized (synthesizedAttribute: ISynthesizedAttribute<'attr>)  { modules = modules; attrs = attrs; } : TopLevelWithAttr =
        { modules = modules; attrs = attrs; }
