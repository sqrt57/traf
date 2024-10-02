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

    let getAssignmentValueType t =
        match t with
        | Tuple [(_, t)] -> t
        | Tuple [] -> raise (TypeError (message = "value has unit type, it cannot be assigned to variable"))
        | Tuple _ -> raise (TypeError (message = "value has tuple type, it cannot be assigned to variable"))
        | t -> t

    let isTypeAssignable source target =
        match (source, target) with
        | (s, t) when s = t -> true
        | (AnyPointer, Pointer) -> true
        | (AnyPointer, PointerTo _) -> true
        | (SomeInt, Int32) -> true
        | (SomeInt, UInt32) -> true
        | _ -> false

    let typesAttribute =
        { new ISynthesizedAttribute<Type> with
            member this.defConst name type_ value attrs = failwith "todo"
            member this.defExternFun name attributes type_ attrs = failwith "todo"
            member this.defFun name attributes type_ statements attrs = failwith "todo"
            member this.defVar name type_ value attrs = failwith "todo"
            member this.exprAddressOf arg attrs = failwith "todo"
            member this.exprBoolVal value attrs = failwith "todo"
            member this.exprCharVal value attrs = failwith "todo"
            member this.exprFunCall func args attrs = failwith "todo"
            member this.exprIntVal value attrs = failwith "todo"
            member this.exprLength arg attrs = failwith "todo"
            member this.exprNegate arg attrs = failwith "todo"
            member this.exprNull(attrs) = failwith "todo"
            member this.exprOperator left op right attrs = failwith "todo"
            member this.exprReference value attrs = failwith "todo"
            member this.exprSizeOf arg attrs = failwith "todo"
            member this.exprStringVal value attrs = failwith "todo"
            member this.stmtAssign name value attrs = failwith "todo"
            member this.stmtConst name type_ value attrs = failwith "todo"
            member this.stmtExpr value attrs = failwith "todo"
            member this.stmtVar name type_ value attrs = failwith "todo"
            member this.topModule name definitions attrs = failwith "todo"
            member this.typeArray itemType size attrs = failwith "todo"
            member this.typeFun args result attrs = failwith "todo"
            member this.typeFunDef args result attrs = failwith "todo"
            member this.typePointer itemType attrs = failwith "todo"
            member this.typeRef name attrs = failwith "todo"
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

    let markTypes ast =
        let context = addBuiltins initialContext
        let ast = calculateSynthesized typesAttribute ast
        ast
