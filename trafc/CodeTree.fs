namespace Triton

open System.Collections.Immutable

module CodeTree =

    type Reference =
        | ModuleGlobal of moduleName: string * index: int
        | FunLocal of index: int

    type Type =
        | Bool
        | UInt8
        | UInt16
        | UInt32
        | Int8
        | Int16
        | Int32
        | ByteString
        | Pointer
        | PointerTo of Type
        | Array of type_: Type * size: int option
        | Tuple of Type list
        | Fun of args: Type * result: Type

    type Value =
        | NoVal
        | BoolVal of bool
        | IntVal of int64
        | ByteStringVal of ImmutableArray<byte>

    type Expr =
        | UInt32Val of uint32
        | Int32Val of int32
        | BoolVal of bool
        | ByteStringVal of ImmutableArray<byte>
        | Ref of reference: Reference * type_: Type
        | Null of type_: Type
        | Length of arg: Expr * type_: Type
        | SizeOf of arg: Expr * type_: Type
        | AddressOf of arg: Expr * type_: Type
        | Negate of arg: Expr * type_: Type
        | FunCall of func: Expr * arguments: Expr list * type_: Type
        | Operator of left: Expr * op: string * right: Expr * type_: Type
        with
            member this.type_() =
                match this with
                | UInt32Val _ -> UInt32
                | Int32Val _ -> Int32
                | BoolVal _ -> Bool
                | ByteStringVal _ -> ByteString
                | Ref (type_ = type_) -> type_
                | Null (type_ = type_) -> type_
                | Length (type_ = type_) -> type_
                | SizeOf (type_ = type_) -> type_
                | AddressOf (type_ = type_) -> type_
                | Negate (type_ = type_) -> type_
                | FunCall (type_ = type_) -> type_
                | Operator (type_ = type_) -> type_

    type LocalDefinition =
        | ConstDefinition of name: string * type_: Type
        | VarDefinition of name: string * type_: Type

    type Statement =
        | Assignment of target: Reference * value: Expr
        | Expression of expr: Expr

    type FunAttrs = { entry: bool }

    type FunBody =
        { funType_: Type
          localDefinitions: ImmutableArray<LocalDefinition>
          body: Statement list }

    type ExternFunAttrs =
        { dll_import: string option
          dll_entry_point_name: string option
          dll_entry_point_ordinal: int64 option }

    type ModuleDefinition =
        | ConstDefinition of name: string * type_: Type * value: Expr
        | VarDefinition of name: string * type_: Type * value: Expr option
        | FunDefinition of name: string * type_: Type * attrs: FunAttrs * body: FunBody
        | ExternFunDefinition of name: string * type_: Type * attrs: ExternFunAttrs

    type Module =
        { name: string;
          definitions: ImmutableArray<ModuleDefinition> }

    type ExeProject =
        { modules: ImmutableArray<Module>
          entryPoint: Reference }
