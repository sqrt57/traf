namespace Triton

module LangType =

    type Type =
        | NoneType
        | Bool
        | UInt8
        | UInt16
        | UInt32
        | Int8
        | Int16
        | Int32
        | SomeInt
        | ByteString
        | Pointer
        | PointerTo of Type
        | Array of typ: Type * size: int option

module ContextType =

    type SymbolClass =
        | Variable
        | Constant
        | Function

    type SymbolInfo =
        { name: string
          symbolClass: SymbolClass
          symbolType: LangType.Type
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
