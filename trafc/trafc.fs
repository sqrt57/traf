namespace Triton

module Trafc =

    open Error
    open Drive

    let parseCmdLine (argv : string list) : Config =

        let defaultConfig : Config = {
            inputs = []
            exeOutput = None
            lexerOutput = None
            cstOutput = None
            astOutput = None
            astWithTypesOutput = None
            verbose = Verbosity.Normal }

        let rec parse (argv: string list) (config: Config) =
            match argv with
            | [] -> { config with inputs = List.rev config.inputs }
            | "-o" :: rest ->
                match rest with
                | [] -> raise (CommandLineParserError "output file name expected after '-o'")
                | fileName :: rest -> 
                    match config.exeOutput with
                    | Some _ -> raise (CommandLineParserError "multiple output files specified")
                    | None ->  parse rest { config with exeOutput = Some fileName }
            | "--lexer" :: rest ->
                match rest with
                | [] -> raise (CommandLineParserError "lexer output file name expected after '--cst'")
                | fileName :: rest -> 
                    match config.lexerOutput with
                    | Some _ -> raise (CommandLineParserError "multiple lexer output files specified")
                    | None ->  parse rest { config with lexerOutput = Some fileName }
            | "--cst" :: rest ->
                match rest with
                | [] -> raise (CommandLineParserError "CST output file name expected after '--cst'")
                | fileName :: rest -> 
                    match config.cstOutput with
                    | Some _ -> raise (CommandLineParserError "multiple CST output files specified")
                    | None ->  parse rest { config with cstOutput = Some fileName }
            | "--ast" :: rest ->
                match rest with
                | [] -> raise (CommandLineParserError "AST output file name expected after '--ast'")
                | fileName :: rest -> 
                    match config.astOutput with
                    | Some _ -> raise (CommandLineParserError "multiple AST output files specified")
                    | None ->  parse rest { config with astOutput = Some fileName }
            | "--ast-types" :: rest ->
                match rest with
                | [] -> raise (CommandLineParserError "AST with types output file name expected after '--ast-types'")
                | fileName :: rest -> 
                    match config.astWithTypesOutput with
                    | Some _ -> raise (CommandLineParserError "multiple AST with types output files specified")
                    | None ->  parse rest { config with astWithTypesOutput = Some fileName }
            | x :: _ when x.StartsWith("-") -> raise (CommandLineParserError <| sprintf "unknown option '%s'" x)
            | fileName :: rest -> parse rest { config with inputs = fileName :: config.inputs }

        parse argv defaultConfig

    [<EntryPoint>]
    let main argv =
        try
            let config = parseCmdLine (List.ofArray argv)

            if List.isEmpty config.inputs then
                raise (CommandLineParserError "no input files specified")

            if config.exeOutput = None then
                raise (CommandLineParserError "no output file specified")

            runCompiler config
            0
        with
            | CommandLineParserError msg ->
                eprintfn "Command line arguments error: %s" msg
                eprintfn ""
                eprintfn "Usage:"
                eprintfn "  trafc -o output input1 [input2] ..."
                eprintfn ""
                eprintfn "Other options:"
                eprintfn "  -o      FILE : write resulting executable to FILE"
                eprintfn "  --lexer FILE : write lexer output to FILE"
                eprintfn "  --cst   FILE : write CST parser output to FILE"
                eprintfn "  --ast   FILE : write AST parser output to FILE"
                1
            | LexerError err ->
                eprintfn "Error while lexing: %s" err.message
                1
            | CstParserError err ->
                eprintfn "Error while parsing to CST: Expected %s but got %A" err.expected err.got
                1
            | AstConvertError err ->
                eprintfn "Error while converting CST to AST: %s" err.message
                1
            | TypeError (message = message) ->
                eprintfn "Error while marking types: %s" message
                1
