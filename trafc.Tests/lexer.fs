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
let ``When lexing incorrect character then raise error`` () =
    raises<Lexer.LexerError> <@ Lexer.lex "" "~" @>

[<Fact>]
let ``Lex line comment`` () =
    test <@ [|Lexeme.Identifier "abc"; Lexeme.Identifier "def" |] = Lexer.lex "" "abc//xyz\ndef" @>
    test <@ [|Lexeme.Identifier "abc"; Lexeme.Identifier "def" |] = Lexer.lex "" "abc//\ndef" @>
    test <@ [|Lexeme.Identifier "abc"; Lexeme.Identifier "def" |] = Lexer.lex "" "abc//x/*y*/z\ndef" @>
    test <@ [|Lexeme.Identifier "abc"|] = Lexer.lex "" "abc//xyz" @>
    test <@ [|Lexeme.Identifier "abc"|] = Lexer.lex "" "abc//" @>

[<Fact>]
let ``Lex comment`` () =
    test <@ [|Lexeme.Identifier "abc"; Lexeme.Identifier "def" |] = Lexer.lex "" "abc/*xyz*/def" @>
    test <@ [|Lexeme.Identifier "abc"; Lexeme.Identifier "def" |] = Lexer.lex "" "abc/**/def" @>
    test <@ [|Lexeme.Identifier "abc"; Lexeme.Identifier "def" |] = Lexer.lex "" "abc/*//\n*/def" @>
    test <@ [|Lexeme.Identifier "abc"; Lexeme.Identifier "def" |] = Lexer.lex "" "abc/*// */def" @>
    test <@ [|Lexeme.Identifier "abc"|] = Lexer.lex "" "abc/*xyz*/" @>

[<Fact>]
let ``Lex nested comment`` () =
    test <@ [|Lexeme.Identifier "abc"; Lexeme.Identifier "def" |] = Lexer.lex "" "abc/*x/*y*/*/def" @>
    test <@ [|Lexeme.Identifier "abc"; Lexeme.Identifier "def" |] = Lexer.lex "" "abc/*/**/*/def" @>

[<Fact>]
let ``When lexing unclosed comment then raise error`` () =
    raises<Lexer.LexerError> <@ Lexer.lex "" "/*" @>

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
let ``Lex empty string literal`` () =
    test <@ [| Lexeme.StringLiteral "" |] = Lexer.lex "" "\"\"" @>

[<Fact>]
let ``Lex string literal`` () =
    test <@ [| Lexeme.StringLiteral "abc" |] = Lexer.lex "" "\"abc\"" @>

[<Fact>]
let ``Lex string literal with newlines`` () =
    test <@ [| Lexeme.StringLiteral "\n \n" |] = Lexer.lex "" "\"\n \\n\"" @>

[<Fact>]
let ``Lex string literal with tabs`` () =
    test <@ [| Lexeme.StringLiteral "\t \t" |] = Lexer.lex "" "\"\t \\t\"" @>

[<Fact>]
let ``Lex string literal with apostrophes`` () =
    test <@ [| Lexeme.StringLiteral "' '" |] = Lexer.lex "" "\"' \\'\"" @>

[<Fact>]
let ``Lex string literal with quotes`` () =
    test <@ [| Lexeme.StringLiteral "\"" |] = Lexer.lex "" "\"\\\"\"" @>

[<Fact>]
let ``When lexing unclosed string literal then get error`` () =
    raises<Lexer.LexerError> <@ Lexer.lex "" "\"" @>

[<Fact>]
let ``When lexing string literal with bad escape sequence then get error`` () =
    raises<Lexer.LexerError> <@ Lexer.lex "" "\"\\g\"" @>

[<Fact>]
let ``Lex char literal`` () =
    test <@ [| Lexeme.CharLiteral 'x' |] = Lexer.lex "" "'x'" @>

[<Fact>]
let ``Lex char literal with newline`` () =
    test <@ [| Lexeme.CharLiteral '\n'; Lexeme.CharLiteral '\n' |] = Lexer.lex "" "'\n' '\\n'" @>

[<Fact>]
let ``Lex char literal with tab`` () =
    test <@ [| Lexeme.CharLiteral '\t'; Lexeme.CharLiteral '\t' |] = Lexer.lex "" "'\t' '\\t'" @>

[<Fact>]
let ``Lex char literal with apostrophe`` () =
    test <@ [| Lexeme.CharLiteral '\'' |] = Lexer.lex "" "'\\''" @>

[<Fact>]
let ``Lex char literal with quote`` () =
    test <@ [| Lexeme.CharLiteral '\"'; Lexeme.CharLiteral '\"' |] = Lexer.lex "" "'\"' '\\\"'" @>

[<Fact>]
let ``When lexing empty char literal then get error`` () =
    raises<Lexer.LexerError> <@ Lexer.lex "" "''" @>

[<Fact>]
let ``When lexing unclosed char literal then get error`` () =
    raises<Lexer.LexerError> <@ Lexer.lex "" "'" @>
    raises<Lexer.LexerError> <@ Lexer.lex "" "'x" @>
    raises<Lexer.LexerError> <@ Lexer.lex "" "'x " @>

[<Fact>]
let ``When lexing char literal with bad escape sequence then get error`` () =
    raises<Lexer.LexerError> <@ Lexer.lex "" "'\\g'" @>

[<Fact>]
let ``Lex zero number literal`` () =
    test <@ [| Lexeme.Int 0L |] = Lexer.lex "" "0" @>

[<Fact>]
let ``Lex positive number literal`` () =
    test <@ [| Lexeme.Int 1L; Lexeme.Int 23L |] = Lexer.lex "" "1 23" @>

[<Fact>]
let ``Lex negative number literal`` () =
    test <@ [| Lexeme.Int 0L; Lexeme.Int -1L; Lexeme.Int -23L |] = Lexer.lex "" "-0 -1 -23" @>

[<Fact>]
let ``Lex number literal with underscore`` () =
    test <@ [| Lexeme.Int 11L; Lexeme.Int 12L |] = Lexer.lex "" "1__1 0_1_2" @>

[<Fact>]
let ``Lex hexadecimal number literal`` () =
    test <@ [| Lexeme.Int 0L; Lexeme.Int 0x2bfL; Lexeme.Int -0xabcL |] = Lexer.lex "" "0x0 0x2bf -0Xa_b_C" @>
