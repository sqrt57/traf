namespace Triton

module Lexeme =

    type Lexeme =
        | Identifier of string
        | Operator of string
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
