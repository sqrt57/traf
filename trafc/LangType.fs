namespace Triton

module LangType =

    type Type =
        | NoType
        | Bool
        | UInt8
        | UInt16
        | UInt32
        | Int8
        | Int16
        | Int32
        | SomeInt
        | ByteString
        | AnyPointer
        | Pointer
        | PointerTo of Type
        | Array of typ: Type * size: int option
        | Tuple of (string option * Type) list
        | Fun of args: Type * result: Type

    type Value =
        | NoVal
        | BoolVal of bool
        | IntVal of int64
        | ByteStringVal of string

    type AstWithTypes = Ast.TopLevel<unit, unit, unit, Type * Value, Type>


module ContextType =

    type SymbolClass =
        | Variable
        | Constant
        | Function

    type SymbolInfo =
        { name: string
          symbolClass: SymbolClass
          symbolType: LangType.Type
          value: LangType.Value
        }

    type ContextFrame =
        { bindings: Map<string, SymbolInfo>
        }

    type TypeInfo =
        { typeName: string
          type_: LangType.Type
        }

    type TypeContextFrame =
        { typeBindings: Map<string, TypeInfo>
        }

    type Context =
        { frames: ContextFrame list
          typeFrames: TypeContextFrame list
        }
