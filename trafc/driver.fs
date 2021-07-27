namespace Triton

open System.IO

module Driver =

    type Verbosity =
        | Silent
        | Normal

    type Config =
        { inputs: string list
          exeOutput: string option
          lexerOutput: string option
          cstParserOutput: string option
          astParserOutput: string option
          verbose: Verbosity }

    type CompiledModule =
        { fileName: string
          contents: string
          lexemes: Lexeme list
          cst: Cst.TopLevel }

    let compileModule fileName =

        let contents = File.ReadAllText fileName
        let lexemes = Lexer.lex contents |> List.ofSeq
        let cst = CstParser.parse lexemes

        { fileName = fileName
          contents = contents
          lexemes = lexemes
          cst = cst }

    let writeLexerOutput (fileName: string) (modules: CompiledModule seq) =
        use writer = new StreamWriter(fileName)
        for m in modules do
            fprintfn writer "Lex result for %s:" fileName
            for lexeme in m.lexemes do
                fprintfn writer "%O" lexeme
            fprintfn writer ""

    let writeCstOutput (fileName: string) (modules: CompiledModule seq) =
        use writer = new StreamWriter(fileName)
        for m in modules do
            fprintfn writer "CST parse result for %s:" fileName
            fprintfn writer "%O" m.cst
            fprintfn writer ""

    let runCompiler (config: Config) =
        let modules = List.map compileModule config.inputs

        match config.lexerOutput with
        | Some fileName -> writeLexerOutput fileName modules
        | _ -> ()

        match config.cstParserOutput with
        | Some fileName -> writeCstOutput fileName modules
        | _ -> ()
