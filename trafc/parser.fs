namespace Triton

open System
open System.Text
open System.Collections.Generic

[<RequireQualifiedAccess>]
module Ast =

    type Module = { name: string }

    type TopLevel = TopLevel of Module array

module Parser =

    exception ParserError of {| fileName: string; message: string |}

    let parse (lexemes: Lexeme array) : Ast.TopLevel =
        Ast.TopLevel [||]
