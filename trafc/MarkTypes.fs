namespace Triton

module MarkTypes =

    open Error
    open AstTransform
    open LangType
    open Context

    type AstWithTypes = Ast.TopLevel<unit, unit, unit, Type, int>

    let markTypesVisitor =
        { new IAstVisitor<unit, unit, unit, unit, unit,
                          unit, unit, unit, Type, int,
                          Context, Context, Context, Context, Context> with
            member this.typeRef name context source = 0
            member this.typeArraySize context source = context
            member this.typeArray arg size context source = 0
            member this.typePointer arg context source = 0
            member this.typeFun args result context source = 0
            member this.typeFunDef args result context source = 0

            member this.exprIntVal value context source = Int
            member this.exprCharVal value context source = Char
            member this.exprBoolVal value context source = Bool
            member this.exprStringVal value context source = ByteString
            member this.exprReference value context source = ByteString
            member this.exprNull context source = Pointer

            member this.exprLengthChild context source = context
            member this.exprLength arg context source =
                match arg with
                | ByteString -> Int
                | _ -> raise (TypeError {| message = "Length argument should be bytestring" |})

            member this.exprSizeOfChild context source = context
            member this.exprSizeOf arg context source = raise (System.NotImplementedException())

            member this.exprAddressOfChild context source = context
            member this.exprAddressOf arg context source =
                match arg with
                | ByteString -> PointerTo Char
                | t -> PointerTo t

            member this.exprNegateChild context source = context
            member this.exprNegate arg context source =
                match arg with
                | Int -> Int
                | _ -> raise (TypeError {| message = "Unary negation can be only applied to integers" |})

            member this.exprFunCallChild context source = context
            member this.exprFunCall func args context source = NoneType

            member this.exprOperatorChild context source = context
            member this.exprOperator left op right context source = NoneType

            // Statement
            member this.stmtConstValue context source = context
            member this.stmtConstType context source = context
            member this.stmtConst name value context source = (context, ())

            member this.stmtVarValue context source = context
            member this.stmtVarType context source = context
            member this.stmtVar name value context source = (context, ())

            member this.stmtAssignValue context source = context
            member this.stmtAssign name value context source = (context, ())

            member this.stmtExprValue context source = context
            member this.stmtExpr value context source = (context, ())

            // Definition
            member this.defConstValue context source = context
            member this.defConstType context source = context
            member this.defConst name value context source = (context, ())

            member this.defVarValue context source = context
            member this.defVarType context source = context
            member this.defVar name value context source = (context, ())

            member this.defFunBody context source = context
            member this.defFunType context source = context
            member this.defFun name attrs statements context source = (context, ())

            member this.defExternFunType context source = context
            member this.defExternFun name attrs context source = (context, ())

            // Module
            member this.topModuleBody context source = context
            member this.topModule name definitions context source = (context, ())
        }

    let markTypes ast =
        let context = Context Map.empty
        let (context, ast) = visitAst markTypesVisitor ast context
        ast
