namespace Triton

open System.Collections.Immutable

module Drive =

    open System.IO

    type Verbosity =
        | Silent
        | Normal

    type Config =
        { inputs: string list
          exeOutput: string option
          lexerOutput: string option
          cstOutput: string option
          astOutput: string option
          astWithTypesOutput: string option
          codeTreeOutput: string option
          verbose: Verbosity }

    type CompiledModule =
        { fileName: string
          contents: string
          lexemes: Lexeme.Lexeme list
          cst: Cst.TopLevel
          ast: Ast.TopLevel
          astWithTypes: Ast.TopLevel }

    let compileModule fileName =

        let contents = File.ReadAllText fileName
        let lexemes = Lex.lex contents |> List.ofSeq
        let cst = CstParse.parse lexemes
        let ast = AstConvert.convert cst
        let astWithTypes = MarkTypes.markTypes ast

        { fileName = fileName
          contents = contents
          lexemes = lexemes
          cst = cst
          ast = ast
          astWithTypes = astWithTypes }

    let writeLexerOutput (fileName: string) (modules: CompiledModule seq) =
        use writer = new StreamWriter(fileName)
        for m in modules do
            fprintfn writer "Lex result for %s:" m.fileName
            for lexeme in m.lexemes do
                fprintfn writer "%A" lexeme
            fprintfn writer ""

    let writeCstOutput (fileName: string) (modules: CompiledModule seq) =
        use writer = new StreamWriter(fileName)
        for m in modules do
            fprintfn writer "CST parse result for %s:" m.fileName
            fprintfn writer "%A" m.cst
            fprintfn writer ""

    let writeAstOutput (fileName: string) (modules: CompiledModule seq) =
        use writer = new StreamWriter(fileName)
        for m in modules do
            fprintfn writer "AST parse result for %s:" m.fileName
            fprintfn writer "%A" m.ast
            fprintfn writer ""

    let writeAstWithTypesOutput (fileName: string) (modules: CompiledModule seq) =
        use writer = new StreamWriter(fileName)
        for m in modules do
            fprintfn writer "Mark types result for %s:" m.fileName
            fprintfn writer "%A" m.astWithTypes
            fprintfn writer ""

    let writeCodeTreeOutput (fileName: string) (codeTree: CodeTree.ExeProject) =
        use writer = new StreamWriter(fileName)
        fprintfn writer "Code tree:"
        fprintfn writer "%A" codeTree

    let runCompiler (config: Config) =
        let modules = List.map compileModule config.inputs
        let codeTree : CodeTree.ExeProject =
          { modules = ImmutableArray<CodeTree.Module>.Empty
            entryPoint = CodeTree.ModuleGlobal (moduleName="", index=0) }
        match config.lexerOutput with
        | Some fileName -> writeLexerOutput fileName modules
        | _ -> ()

        match config.cstOutput with
        | Some fileName -> writeCstOutput fileName modules
        | _ -> ()

        match config.astOutput with
        | Some fileName -> writeAstOutput fileName modules
        | _ -> ()

        match config.astWithTypesOutput with
        | Some fileName -> writeAstWithTypesOutput fileName modules
        | _ -> ()

        match config.codeTreeOutput with
        | Some fileName -> writeCodeTreeOutput fileName codeTree
        | _ -> ()

        match config.exeOutput with
        | Some exeName -> PeCoffOutput.writeExe exeName
        | None -> ()
