namespace Triton

module LangType =

    type Type =
        | NoneType
        | Bool
        | Int
        | Char
        | ByteString
        | Pointer
        | PointerTo of Type
        | Array of typ: Type * size: int option

module Context =

    type SymbolClass =
        | Variable
        | Constant
        | Function

    type SymbolInfo =
        SymbolInfo of
            name: string *
            symbolClass: SymbolClass *
            symbolType: LangType.Type

    type Context = Context of Map<string, SymbolInfo>
