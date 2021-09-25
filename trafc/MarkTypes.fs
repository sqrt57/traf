namespace Triton

module Context =

    open ContextType
    open Errors

    let emptyFrame = { bindings = Map.empty }
    let emptyTypeFrame = { typeBindings = Map.empty }

    let initialContext = { frames = [emptyFrame]; typeFrames = [emptyTypeFrame] }
    let pushEmptyFrame context = { context with frames = emptyFrame :: context.frames }
    let pushEmptyTypeFrame context = { context with typeFrames = emptyTypeFrame :: context.typeFrames }

    let addBinding symbolInfo context =
        match context.frames with
        | frame :: rest ->
            if Map.containsKey symbolInfo.name frame.bindings then
                raise (TypeError (message = $"Duplicate identifier {symbolInfo.name}"))
            else
                let bindings = Map.add symbolInfo.name symbolInfo frame.bindings
                { context with frames = { bindings = bindings } :: rest }
        | _ -> raise (System.InvalidOperationException())

    let addTypeBinding typeInfo context =
        match context.typeFrames with
        | frame :: rest ->
            if Map.containsKey typeInfo.typeName frame.typeBindings then
                raise (TypeError (message = $"Duplicate type {typeInfo.typeName}"))
            else
                let typeBindings = Map.add typeInfo.typeName typeInfo frame.typeBindings
                { context with typeFrames = { typeBindings = typeBindings } :: rest }
        | _ -> raise (System.InvalidOperationException())

    let getBinding context name =
        let getFrameBinding frame = Map.tryFind name frame.bindings
        let symbolInfo = List.tryPick getFrameBinding context.frames
        match symbolInfo with
        | Some s -> s
        | None -> raise (TypeError (message = $"Unknown identifier {name}"))

    let getTypeBinding context name =
        let getFrameBinding frame = Map.tryFind name frame.typeBindings
        let typeInfo = List.tryPick getFrameBinding context.typeFrames
        match typeInfo with
        | Some s -> s
        | None -> raise (TypeError (message = $"Unknown type {name}"))

module MarkTypes =

    open Errors
    open AstTransform
    open LangType
    open ContextType
    open Context

    type AstWithTypes = Ast.TopLevel<unit, unit, unit, Type, Type>

    let markTypesVisitor =
        { new IAstVisitor<unit, unit, unit, unit, unit,
                          unit, unit, unit, Type, Type,
                          Context, Context, Context, Context, Context> with

            member this.typeRef name context source = (getTypeBinding context name).type_
            member this.typeArraySize context source = context
            member this.typeArray arg size context source = Array (typ = arg, size = None)
            member this.typePointer arg context source = PointerTo arg
            member this.typeFun args result context source = NoneType
            member this.typeFunDef args result context source = NoneType

            member this.exprIntVal value context source = SomeInt
            member this.exprCharVal value context source = UInt8
            member this.exprBoolVal value context source = Bool
            member this.exprStringVal value context source = ByteString
            member this.exprReference name context source = (getBinding context name).symbolType
            member this.exprNull context source = Pointer

            member this.exprLengthChild context source = context
            member this.exprLength arg context source =
                match arg with
                | ByteString -> SomeInt
                | _ -> raise (TypeError (message = "Length argument should be bytestring"))

            member this.exprSizeOfChild context source = context
            member this.exprSizeOf arg context source = raise (System.NotImplementedException())

            member this.exprAddressOfChild context source = context
            member this.exprAddressOf arg context source =
                match arg with
                | ByteString -> PointerTo UInt8
                | t -> PointerTo t

            member this.exprNegateChild context source = context
            member this.exprNegate arg context source =
                match arg with
                | SomeInt -> SomeInt
                | _ -> raise (TypeError (message = "Unary negation can be only applied to integers"))

            member this.exprFunCallChild context source = context
            member this.exprFunCall func args context source = NoneType

            member this.exprOperatorChild context source = context
            member this.exprOperator left op right context source = NoneType

            // Statement
            member this.stmtConstValue context source = context
            member this.stmtConstType context source = context
            member this.stmtConst name type_ value context source =
                let symbolInfo = { name = name; symbolClass = SymbolClass.Constant; symbolType = type_ }
                let context = addBinding symbolInfo context
                (context, ())

            member this.stmtVarValue context source = context
            member this.stmtVarType context source = context
            member this.stmtVar name type_ value context source =
                let symbolInfo = { name = name; symbolClass = SymbolClass.Variable; symbolType = type_ }
                let context = addBinding symbolInfo context
                (context, ())

            member this.stmtAssignValue context source = context
            member this.stmtAssign name value context source = (context, ())

            member this.stmtExprValue context source = context
            member this.stmtExpr value context source = (context, ())

            // Definition
            member this.defConstValue context source = context
            member this.defConstType context source = context
            member this.defConst name type_ value context source =
                let symbolInfo = { name = name; symbolClass = SymbolClass.Constant; symbolType = type_ }
                let context = addBinding symbolInfo context
                (context, ())

            member this.defVarValue context source = context
            member this.defVarType context source = context
            member this.defVar name type_ value context source =
                let symbolInfo = { name = name; symbolClass = SymbolClass.Variable; symbolType = type_ }
                let context = addBinding symbolInfo context
                (context, ())

            member this.defFunBody context source = context |> pushEmptyFrame
            member this.defFunType context source = context
            member this.defFun name type_ attrs statements context source =
                let symbolInfo = { name = name; symbolClass = SymbolClass.Function; symbolType = type_ }
                let context = addBinding symbolInfo context
                (context, ())

            member this.defExternFunType context source = context
            member this.defExternFun name type_ attrs context source =
                let symbolInfo = { name = name; symbolClass = SymbolClass.Function; symbolType = type_ }
                let context = addBinding symbolInfo context
                (context, ())

            // Module
            member this.topModuleBody context source = context |> pushEmptyFrame |> pushEmptyTypeFrame
            member this.topModule name definitions context source = (context, ())
        }

    let addBuiltins context =
        context
        |> addTypeBinding { typeName = "byte"; type_ = Type.UInt8 }
        |> addTypeBinding { typeName = "uint8"; type_ = Type.UInt8 }
        |> addTypeBinding { typeName = "uint16"; type_ = Type.UInt16 }
        |> addTypeBinding { typeName = "uint32"; type_ = Type.UInt32 }
        |> addTypeBinding { typeName = "int8"; type_ = Type.Int8 }
        |> addTypeBinding { typeName = "int16"; type_ = Type.Int16 }
        |> addTypeBinding { typeName = "int32"; type_ = Type.Int32 }
        |> addTypeBinding { typeName = "bool"; type_ = Type.Bool }
        |> addTypeBinding { typeName = "pointer"; type_ = Type.Pointer }
        |> addTypeBinding { typeName = "bytestring"; type_ = Type.ByteString }
        |> addBinding { name = "null"; symbolClass = SymbolClass.Constant; symbolType = Type.Pointer }

    let markTypes ast =
        let context = addBuiltins initialContext
        let (context, ast) = visitAst markTypesVisitor ast context
        ast
