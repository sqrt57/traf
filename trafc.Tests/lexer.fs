module Triton.Tests.LexerTests

open Xunit
open Swensen.Unquote

open Triton

[<Fact>]
let ``Lex empty string`` () =
    test <@ [||] = Lexer.lex "" "" @>

[<Fact>]
let ``Lex whitespace`` () =
    test <@ [||] = Lexer.lex "" "  " @>

[<Fact>]
let ``Lex identifier`` () =
    test <@ [|Lexeme.Identifier "abc"|] = Lexer.lex "" "abc" @>

[<Fact>]
let ``Lex identifier with digits`` () =
    test <@ [|Lexeme.Identifier "abc123"|] = Lexer.lex "" "abc123" @>

[<Fact>]
let ``Lex identifier with underscore`` () =
    test <@ [|Lexeme.Identifier "_abc_def"|] = Lexer.lex "" "_abc_def" @>

[<Fact>]
let ``Lex identifier with whitespace`` () =
    test <@ [|Lexeme.Identifier "abc"|] = Lexer.lex "" " abc " @>

[<Fact>]
let ``Lex two identifiers`` () =
    test <@ [|Lexeme.Identifier "abc"; Lexeme.Identifier "def"|] = Lexer.lex "" "abc def" @>

[<Fact>]
let ``Lex identifier with left and right curly brackets`` () =
    test <@ [| Lexeme.Identifier "abc"; Lexeme.LeftCurly; Lexeme.RightCurly |] = Lexer.lex "" "abc{}" @>

[<Fact>]
let ``Lex single chars`` () =
    test <@ [| Lexeme.LeftCurly; Lexeme.RightCurly; Lexeme.LeftBracket; Lexeme.RightBracket
               Lexeme.LeftSquare; Lexeme.RightSquare; Lexeme.Dot; Lexeme.Comma
               Lexeme.Semicolon; Lexeme.Caret; Lexeme.AtSign
            |] = Lexer.lex "" "{}()[].,;^@" @>

[<Fact>]
let ``When lexing incorrect character then get error`` () =
    raises<Lexer.LexerError> <@ Lexer.lex "" "~" @>
