namespace Triton

open System
open System.Text

[<AutoOpen>]
module LexemeModule =

    [<RequireQualifiedAccess>]
    type Lexeme =
        | Identifier of string
        | StringLiteral of string
        | CharLiteral of char
        | Int of int64
        | LeftCurly
        | RightCurly
        | LeftBracket
        | RightBracket
        | LeftSquare
        | RightSquare
        | Dot
        | Comma
        | Semicolon
        | Caret
        | AtSign

module Lexer =

    exception LexerError of {| fileName: string; message: string |}

    let private isIdentStart (c : char) = Char.IsLetter c || c = '_'

    let private isIdent (c : char) = Char.IsLetterOrDigit c || c = '_'

    let private isWhite (c : char) = Char.IsWhiteSpace c

    let singleCharTokens = readOnlyDict [
        '{', Lexeme.LeftCurly
        '}', Lexeme.RightCurly
        '(', Lexeme.LeftBracket
        ')', Lexeme.RightBracket
        '[', Lexeme.LeftSquare
        ']', Lexeme.RightSquare
        '.', Lexeme.Dot
        ',', Lexeme.Comma
        ';', Lexeme.Semicolon
        '^', Lexeme.Caret
        '@', Lexeme.AtSign
    ]

    let literalSpecialChars = readOnlyDict [
        '\\', '\\'
        'n', '\n'
        't', '\t'
        '"', '"'
        '\'', '\''
        '0', '\u0000'
    ]

    let decimalDigits = readOnlyDict [
        '0', 0L
        '1', 1L
        '2', 2L
        '3', 3L
        '4', 4L
        '5', 5L
        '6', 6L
        '7', 7L
        '8', 8L
        '9', 9L
    ]

    let lex (fileName : string) (source : string) : Lexeme array =
        let result = ResizeArray<Lexeme>()

        let rec processChar i =
            if i < source.Length then
                let c = source.[i]
                if isWhite c then
                    processChar (i + 1)
                else if isIdentStart c then
                    processIdentifier i
                else if singleCharTokens.ContainsKey c then
                    singleCharTokens.Item c |> result.Add
                    processChar (i + 1)
                else if decimalDigits.ContainsKey c then
                    processNumericalLiteral i
                else if c = '"' then
                    processStringLiteral i
                else if c = '\'' then
                    processCharLiteral i
                else if c = '/' then
                    if i + 1 < source.Length then
                        let c1 = source.[i+1]
                        if c1 = '/' then
                            processLineComment (i + 2)
                        else if c1 = '*' then
                            processComment (i + 2)
                        else
                            raise <| LexerError {| fileName = fileName; message = sprintf "invalid character sequence: '%s'" (source.Substring(i, 2)) |}
                    else
                        raise <| LexerError {| fileName = fileName; message = sprintf "invalid character sequence: '%s'" (source.Substring(i, 2)) |}
                else
                    raise <| LexerError {| fileName = fileName; message = sprintf "invalid char: %c" c |}
            else
                ()

        and processLineComment i =
            if i < source.Length then
                let c = source.[i]
                if c = '\n' then
                    processChar (i + 1)
                else
                    processLineComment (i + 1)
            else
                ()

        and processComment i =
            let rec processCommentInner level prev i =
                if i < source.Length then
                    let c = source.[i]
                    if prev = '/' && c = '*' then
                        processCommentInner (level + 1) ' ' (i + 1)
                    else if prev = '*' && c = '/' then
                        if level > 1 then
                            processCommentInner (level - 1) ' ' (i + 1)
                        else
                            processChar (i + 1)
                    else
                        processCommentInner level c (i + 1)
                else
                    raise <| LexerError {| fileName = fileName; message = "comment close expected but got EOF" |}

            processCommentInner 1 ' ' i

        and processIdentifier start =
            let rec processIdentifierInner i =
                if i < source.Length then
                    let c = source.[i]
                    if isIdent c then
                        processIdentifierInner (i + 1)
                    else
                        source.Substring(start, i - start) |> Lexeme.Identifier |> result.Add
                        processChar i
                else
                    source.Substring(start) |> Lexeme.Identifier |> result.Add
                    ()
            processIdentifierInner (start + 1)

        and processStringLiteral start =
            let literal = StringBuilder()

            let rec processStringLiteralInner i =
                if i < source.Length then
                    let c = source.[i]
                    if c = '"' then
                        literal.ToString() |> Lexeme.StringLiteral |> result.Add
                        processChar (i+1)
                    else if c = '\\' then
                        if i + 1 < source.Length then
                            let c1 = source.[i+1]
                            if literalSpecialChars.ContainsKey c1 then
                                literalSpecialChars.Item c1 |> literal.Append |> ignore
                                processStringLiteralInner (i + 2)
                            else
                                raise <| LexerError {| fileName = fileName; message = sprintf "unknow escape sequence in string literal: '%s'" (source.Substring(i, 2)) |}
                        else
                            raise <| LexerError {| fileName = fileName; message = sprintf "unexpected EOF in string literal: %s" (source.Substring start) |}
                    else
                        literal.Append c |> ignore
                        processStringLiteralInner (i + 1)
                else
                    raise <| LexerError {| fileName = fileName; message = sprintf "unexpected EOF in string literal: %s" (source.Substring start) |}

            processStringLiteralInner (start + 1)

        and processCharLiteral start =
            let charLiteralCheckClose start close =
                if close < source.Length then
                    let c = source.[close]
                    if c = '\'' then
                        processChar (close+1)
                    else
                        raise <| LexerError {| fileName = fileName; message = sprintf "expected closing apostrophe in char literal: %s" (source.Substring(start, (close-start+1))) |}
                else
                    raise <| LexerError {| fileName = fileName; message = sprintf "unexpected EOF in char literal: %s" (source.Substring start) |}

            if start + 1 < source.Length then
                let c = source.[start+1]
                if c = '\'' then
                    raise <| LexerError {| fileName = fileName; message = sprintf "empty char literal: '%s'" (source.Substring(start, 2)) |}
                else if c = '\\' then
                    if start + 2 < source.Length then
                        let c1 = source.[start+2]
                        if literalSpecialChars.ContainsKey c1 then
                            literalSpecialChars.Item c1 |> Lexeme.CharLiteral |> result.Add
                            charLiteralCheckClose start (start + 3)
                        else
                            raise <| LexerError {| fileName = fileName; message = sprintf "unknown escape sequence in char literal: '%s'" (source.Substring(start, 3)) |}
                    else
                        raise <| LexerError {| fileName = fileName; message = sprintf "unexpected EOF in char literal: %s" (source.Substring start) |}
                else
                    c |> Lexeme.CharLiteral |> result.Add
                    charLiteralCheckClose start (start + 2)
            else
                raise <| LexerError {| fileName = fileName; message = sprintf "unexpected EOF in char literal: %s" (source.Substring start) |}

        and processNumericalLiteral start =
            let doneNumericalLiteral i acc =
                acc |> Lexeme.Int |> result.Add
                processChar i

            let rec processNumericalLiteralInner i acc =
                if i < source.Length then
                    let c = source.[i]
                    if decimalDigits.ContainsKey c then
                        let digit = decimalDigits.Item c
                        processNumericalLiteralInner (i + 1) (acc * 10L + digit)
                    else if isIdentStart c then
                        raise <| LexerError {| fileName = fileName; message = sprintf "unexpected char %c in numerical literal: '%s'" c (source.Substring(start, i + 1 - start)) |}
                    else
                        doneNumericalLiteral i acc
                else
                    doneNumericalLiteral i acc

            processNumericalLiteralInner start 0L

        processChar 0

        result.ToArray()
