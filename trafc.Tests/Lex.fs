module Triton.Tests.LexTests

open Xunit
open Swensen.Unquote

open Triton
open Triton.Errors

[<Fact>]
let ``Lex empty string`` () =
    test <@ [||] = (Lex.lex "" |> Seq.toArray) @>

[<Fact>]
let ``Lex whitespace`` () =
    test <@ [||] = (Lex.lex "  " |> Seq.toArray) @>

[<Fact>]
let ``When lexing incorrect character then raise error`` () =
    raises<LexerError> <@ Lex.lex "~" @>

[<Fact>]
let ``Lex line comment`` () =
    test <@ [| Lexeme.Identifier "abc"; Lexeme.Identifier "def" |] = (Lex.lex "abc//xyz\ndef" |> Seq.toArray) @>
    test <@ [| Lexeme.Identifier "abc"; Lexeme.Identifier "def" |] = (Lex.lex "abc//\ndef" |> Seq.toArray) @>
    test <@ [| Lexeme.Identifier "abc"; Lexeme.Identifier "def" |] = (Lex.lex "abc//x/*y*/z\ndef" |> Seq.toArray) @>
    test <@ [| Lexeme.Identifier "abc"|] = (Lex.lex "abc//xyz" |> Seq.toArray) @>
    test <@ [| Lexeme.Identifier "abc"|] = (Lex.lex "abc//" |> Seq.toArray) @>

[<Fact>]
let ``Lex comment`` () =
    test <@ [| Lexeme.Identifier "abc"; Lexeme.Identifier "def" |] = (Lex.lex "abc/*xyz*/def" |> Seq.toArray) @>
    test <@ [| Lexeme.Identifier "abc"; Lexeme.Identifier "def" |] = (Lex.lex "abc/**/def" |> Seq.toArray) @>
    test <@ [| Lexeme.Identifier "abc"; Lexeme.Identifier "def" |] = (Lex.lex "abc/*//\n*/def" |> Seq.toArray) @>
    test <@ [| Lexeme.Identifier "abc"; Lexeme.Identifier "def" |] = (Lex.lex "abc/*// */def" |> Seq.toArray) @>
    test <@ [| Lexeme.Identifier "abc"|] = (Lex.lex "abc/*xyz*/" |> Seq.toArray) @>

[<Fact>]
let ``Lex nested comment`` () =
    test <@ [| Lexeme.Identifier "abc"; Lexeme.Identifier "def" |] = (Lex.lex "abc/*x/*y*/*/def" |> Seq.toArray) @>
    test <@ [| Lexeme.Identifier "abc"; Lexeme.Identifier "def" |] = (Lex.lex "abc/*/**/*/def" |> Seq.toArray) @>

[<Fact>]
let ``When lexing unclosed comment then raise error`` () =
    raises<LexerError> <@ Lex.lex "/*" @>

[<Fact>]
let ``Lex identifier`` () =
    test <@ [| Lexeme.Identifier "abc" |] = (Lex.lex "abc" |> Seq.toArray) @>

[<Fact>]
let ``Lex identifier with digits`` () =
    test <@ [| Lexeme.Identifier "abc123" |] = (Lex.lex "abc123" |> Seq.toArray) @>

[<Fact>]
let ``Lex identifier with underscore`` () =
    test <@ [| Lexeme.Identifier "_abc_def" |] = (Lex.lex "_abc_def" |> Seq.toArray) @>

[<Fact>]
let ``Lex identifier with whitespace`` () =
    test <@ [| Lexeme.Identifier "abc" |] = (Lex.lex " abc " |> Seq.toArray) @>

[<Fact>]
let ``Lex two identifiers`` () =
    test <@ [| Lexeme.Identifier "abc"; Lexeme.Identifier "def" |] = (Lex.lex "abc def" |> Seq.toArray) @>

[<Fact>]
let ``Lex identifier with left and right curly brackets`` () =
    test <@ [| Lexeme.Identifier "abc"; Lexeme.LeftCurly; Lexeme.RightCurly |] = (Lex.lex "abc{}" |> Seq.toArray) @>

[<Fact>]
let ``Lex operator`` () =
    test <@ [| Lexeme.Operator "+"; |] = (Lex.lex "+" |> Seq.toArray) @>
    test <@ [| Lexeme.Operator "+"; |] = (Lex.lex " + " |> Seq.toArray) @>
    test <@ [| Lexeme.Operator "==>" |] = (Lex.lex "==>" |> Seq.toArray) @>
    test <@ [| Lexeme.Operator "==>" |] = (Lex.lex " ==> " |> Seq.toArray) @>

[<Fact>]
let ``Lex operator starting with minus`` () =
    test <@ [| Lexeme.Operator "-" |] = (Lex.lex "-" |> Seq.toArray) @>
    test <@ [| Lexeme.Operator "-" |] = (Lex.lex " - " |> Seq.toArray) @>
    test <@ [| Lexeme.Operator "--" |] = (Lex.lex "--" |> Seq.toArray) @>
    test <@ [| Lexeme.Operator "--" |] = (Lex.lex " -- " |> Seq.toArray) @>

[<Fact>]
let ``Lex operator starting with slash`` () =
    test <@ [| Lexeme.Operator "/" |] = (Lex.lex "/" |> Seq.toArray) @>
    test <@ [| Lexeme.Operator "/" |] = (Lex.lex " / " |> Seq.toArray) @>
    test <@ [| Lexeme.Operator "/%" |] = (Lex.lex "/%" |> Seq.toArray) @>
    test <@ [| Lexeme.Operator "/%" |] = (Lex.lex " /% " |> Seq.toArray) @>

[<Fact>]
let ``Lex identifier and operator`` () =
    test <@ [| Lexeme.Identifier "a3"; Lexeme.Operator "++"; Lexeme.Identifier "_b4" |] = (Lex.lex "a3++_b4" |> Seq.toArray) @>

[<Fact>]
let ``Lex number and operator`` () =
    test <@ [| Lexeme.Int 2L; Lexeme.Operator "+"; Lexeme.Int 3L |] = (Lex.lex "2+3" |> Seq.toArray) @>
    test <@ [| Lexeme.Int 2L; Lexeme.Operator "-"; Lexeme.Int 3L |] = (Lex.lex "2-3" |> Seq.toArray) @>
    test <@ [| Lexeme.Int 2L; Lexeme.Operator "-"; Lexeme.Int 3L |] = (Lex.lex "2 - 3" |> Seq.toArray) @>
    test <@ [| Lexeme.Int 2L; Lexeme.Operator "-"; Lexeme.Int 3L |] = (Lex.lex "2 -3" |> Seq.toArray) @>

[<Fact>]
let ``Lex delimiters and operator`` () =
    test <@ [| Lexeme.LeftBracket; Lexeme.Operator "++"; Lexeme.Dot; Lexeme.Operator "**"; Lexeme.RightBracket |] = (Lex.lex "(++.**)" |> Seq.toArray) @>

[<Fact>]
let ``Lex single chars`` () =
    test <@ [| Lexeme.LeftCurly; Lexeme.RightCurly; Lexeme.LeftBracket; Lexeme.RightBracket
               Lexeme.LeftSquare; Lexeme.RightSquare; Lexeme.Dot; Lexeme.Comma
               Lexeme.Semicolon; Lexeme.Caret; Lexeme.AtSign
            |] = (Lex.lex "{}()[].,;^@" |> Seq.toArray) @>

[<Fact>]
let ``Lex empty string literal`` () =
    test <@ [| Lexeme.StringLiteral "" |] = (Lex.lex "\"\"" |> Seq.toArray) @>

[<Fact>]
let ``Lex string literal`` () =
    test <@ [| Lexeme.StringLiteral "abc" |] = (Lex.lex "\"abc\"" |> Seq.toArray) @>

[<Fact>]
let ``Lex string literal with newlines`` () =
    test <@ [| Lexeme.StringLiteral "\n \n" |] = (Lex.lex "\"\n \\n\"" |> Seq.toArray) @>

[<Fact>]
let ``Lex string literal with tabs`` () =
    test <@ [| Lexeme.StringLiteral "\t \t" |] = (Lex.lex "\"\t \\t\"" |> Seq.toArray) @>

[<Fact>]
let ``Lex string literal with apostrophes`` () =
    test <@ [| Lexeme.StringLiteral "' '" |] = (Lex.lex "\"' \\'\"" |> Seq.toArray) @>

[<Fact>]
let ``Lex string literal with quotes`` () =
    test <@ [| Lexeme.StringLiteral "\"" |] = (Lex.lex "\"\\\"\"" |> Seq.toArray) @>

[<Fact>]
let ``When lexing unclosed string literal then get error`` () =
    raises<LexerError> <@ Lex.lex "\"" @>

[<Fact>]
let ``When lexing string literal with bad escape sequence then get error`` () =
    raises<LexerError> <@ Lex.lex "\"\\g\"" @>

[<Fact>]
let ``Lex char literal`` () =
    test <@ [| Lexeme.CharLiteral 'x' |] = (Lex.lex "'x'" |> Seq.toArray) @>

[<Fact>]
let ``Lex char literal with newline`` () =
    test <@ [| Lexeme.CharLiteral '\n'; Lexeme.CharLiteral '\n' |] = (Lex.lex "'\n' '\\n'" |> Seq.toArray) @>

[<Fact>]
let ``Lex char literal with tab`` () =
    test <@ [| Lexeme.CharLiteral '\t'; Lexeme.CharLiteral '\t' |] = (Lex.lex "'\t' '\\t'" |> Seq.toArray) @>

[<Fact>]
let ``Lex char literal with apostrophe`` () =
    test <@ [| Lexeme.CharLiteral '\'' |] = (Lex.lex "'\\''" |> Seq.toArray) @>

[<Fact>]
let ``Lex char literal with quote`` () =
    test <@ [| Lexeme.CharLiteral '\"'; Lexeme.CharLiteral '\"' |] = (Lex.lex "'\"' '\\\"'" |> Seq.toArray) @>

[<Fact>]
let ``When lexing empty char literal then get error`` () =
    raises<LexerError> <@ Lex.lex "''" @>

[<Fact>]
let ``When lexing unclosed char literal then get error`` () =
    raises<LexerError> <@ Lex.lex "'" @>
    raises<LexerError> <@ Lex.lex "'x" @>
    raises<LexerError> <@ Lex.lex "'x " @>

[<Fact>]
let ``When lexing char literal with bad escape sequence then get error`` () =
    raises<LexerError> <@ Lex.lex "'\\g'" @>

[<Fact>]
let ``Lex zero number literal`` () =
    test <@ [| Lexeme.Int 0L |] = (Lex.lex "0" |> Seq.toArray) @>
    test <@ [| Lexeme.Int 0L |] = (Lex.lex " 0 " |> Seq.toArray) @>

[<Fact>]
let ``Lex positive number literal`` () =
    test <@ [| Lexeme.Int 1L |] = (Lex.lex "1" |> Seq.toArray) @>
    test <@ [| Lexeme.Int 1L |] = (Lex.lex " 1 " |> Seq.toArray) @>
    test <@ [| Lexeme.Int 23L |] = (Lex.lex "23" |> Seq.toArray) @>
    test <@ [| Lexeme.Int 23L |] = (Lex.lex " 23 " |> Seq.toArray) @>

[<Fact>]
let ``Lex negative number literal`` () =
    test <@ [| Lexeme.Operator "-"; Lexeme.Int 0L |] = (Lex.lex "-0" |> Seq.toArray) @>
    test <@ [| Lexeme.Operator "-"; Lexeme.Int 1L |] = (Lex.lex "-1" |> Seq.toArray) @>
    test <@ [| Lexeme.Operator "-"; Lexeme.Int 23L |] = (Lex.lex "-23" |> Seq.toArray) @>

[<Fact>]
let ``Lex number literal with underscore`` () =
    test <@ [| Lexeme.Int 11L |] = (Lex.lex "1__1" |> Seq.toArray) @>
    test <@ [| Lexeme.Int 11L |] = (Lex.lex " 1__1 " |> Seq.toArray) @>
    test <@ [| Lexeme.Int 12L |] = (Lex.lex "0_1_2" |> Seq.toArray) @>
    test <@ [| Lexeme.Int 12L |] = (Lex.lex " 0_1_2 " |> Seq.toArray) @>

[<Fact>]
let ``Lex hexadecimal number literal`` () =
    test <@ [| Lexeme.Int 0L |] = (Lex.lex "0x0" |> Seq.toArray) @>
    test <@ [| Lexeme.Int 0L |] = (Lex.lex " 0x0 " |> Seq.toArray) @>
    test <@ [| Lexeme.Int 0x2bfL |] = (Lex.lex "0x2bf" |> Seq.toArray) @>
    test <@ [| Lexeme.Int 0x2bfL |] = (Lex.lex " 0x2bf " |> Seq.toArray) @>
    test <@ [| Lexeme.Operator "-"; Lexeme.Int 0xabcL |] = (Lex.lex "-0Xa_b_C" |> Seq.toArray) @>
    test <@ [| Lexeme.Operator "-"; Lexeme.Int 0xabcL |] = (Lex.lex " -0Xa_b_C " |> Seq.toArray) @>
