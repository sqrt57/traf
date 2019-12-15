namespace Triton

open System

[<AutoOpen>]
module LexemeModule =

    [<RequireQualifiedAccess>]
    type Lexeme =
        | Identifier of string
        | LeftCurly
        | RightCurly

module Lexer =

    exception LexerError of {| fileName: string; message: string |}

    let private isIdentStart (c : char) = Char.IsLetter c || c = '_'

    let private isIdent (c : char) = Char.IsLetterOrDigit c || c = '_'

    let private isWhite (c : char) = Char.IsWhiteSpace c

    let singleCharTokens = readOnlyDict [
        '{', Lexeme.LeftCurly
        '}', Lexeme.RightCurly
    ]

    let lex (fileName : string) (source : string) : Lexeme array =
        let result = ResizeArray<Lexeme>()

        let rec processChar i =
            if i < source.Length then
                let c = source.[i]
                if isWhite c then
                    processChar (i + 1)
                else if isIdentStart c then
                    processIdentifier i (i + 1)
                else if singleCharTokens.ContainsKey c then
                    singleCharTokens.Item c |> result.Add
                    processChar (i + 1)
                else raise <| LexerError {| fileName = fileName; message = sprintf "invalid char: %c" c |}
            else
                ()

        and processIdentifier start i =
            if i < source.Length then
                let c = source.[i]
                if isIdent c then
                    processIdentifier start (i + 1)
                else
                    source.Substring(start, i - start) |> Lexeme.Identifier |> result.Add
                    processChar i
            else
                source.Substring(start) |> Lexeme.Identifier |> result.Add
                ()

        processChar 0

        result.ToArray()
