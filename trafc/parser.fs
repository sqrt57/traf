namespace Triton

open System
open System.Text
open System.Collections.Generic

[<RequireQualifiedAccess>]
module Ast =

    [<RequireQualifiedAccess>]
    type Module = { name: string }

    type TopLevel = TopLevel of Module array

module Parser =

    exception ParserError of {| message: string |}

    let parse (lexemes: Lexeme list) : Ast.TopLevel =
        let modules = ResizeArray<Ast.Module>()

        let moduleBody lexemes =
            match lexemes with
            | [] -> raise <| ParserError {| message = sprintf "expected opening curly bracket after module name but got EOF" |}
            | Lexeme.LeftCurly :: rest ->
                match rest with
                | [] -> raise <| ParserError {| message = sprintf "expected closing curly bracket after module body but got EOF" |}
                | Lexeme.RightCurly :: rest -> rest
                | l :: _ -> raise <| ParserError {| message = sprintf "expected closing curly bracket after module body but got %A" l |}
            | l :: _ -> raise <| ParserError {| message = sprintf "expected opening curly bracket after module name but got %A" l |}


        let rec topLevel lexemes =
            match lexemes with
            | [] -> ()
            | Lexeme.Identifier "module" :: rest ->
                match rest with
                | Lexeme.Identifier name :: rest ->
                    let rest = moduleBody rest
                    { Ast.Module.name = name } |> modules.Add
                    topLevel rest
                | [] -> ()
                | l :: _ -> raise <| ParserError {| message = sprintf "expected module name after module keyword but got %A" l |}
            | l :: _ -> raise <| ParserError {| message = sprintf "unexpected lexeme at top level: %A" l |}

        topLevel lexemes

        Ast.TopLevel (modules.ToArray())
