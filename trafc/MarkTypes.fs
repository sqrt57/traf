namespace Triton

module MarkTypes =

    open Error
    open AstTransform
    open LangType

    let markTypes =
        { new ISynthesizedAttribute<unit, Type> with
            member this.intVal value source = Int
            member this.charVal value source = Char
            member this.boolVal value source = Bool
            member this.stringVal value source = String
            member this.reference value source = None
            member this.null_ source = Pointer
            member this.length arg source = None
            member this.sizeOf arg source = None
            member this.addressOf arg source = None
            member this.negate arg source =
                match arg with
                | Int -> Int
                | _ -> raise (TypeError {| message = "Unary negation can be only applied to integers" |})
            member this.operator left op right source = None
        }
