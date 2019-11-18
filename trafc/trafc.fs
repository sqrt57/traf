module Trafc

open System.IO

exception CommandLineParserError of string

let parseCmdLine (argv : string list) : string list * string =
    let mutable inputs = []
    let mutable output = null

    let rec parse (argv : string list) =
        match argv with
        | [] -> ()
        | "-o" :: rest ->
            match rest with
            | [] -> raise (CommandLineParserError "output file name expected after '-o'")
            | o :: rest ->
                if output = null then
                    output <- o
                    parse rest
                else
                    raise (CommandLineParserError "multiple output files specified")
        | x :: _ when x.StartsWith("-") -> raise (CommandLineParserError <| sprintf "unknown option '%s'" x)
        | x :: rest ->
            inputs <- x :: inputs
            parse rest

    parse argv

    if List.isEmpty inputs then
        raise (CommandLineParserError "no input files specified")

    if output = null then
        raise (CommandLineParserError "no output file specified")

    (List.rev inputs, output)

let compile inputs output =
    let driver = Drive.createDriver()
    for input in inputs do
        let source = File.ReadAllText input
        Drive.addSource driver source
    let binary = Drive.getExe driver
    File.WriteAllBytes(output, binary)

[<EntryPoint>]
let main argv =
    try
        let (inputs, output) = parseCmdLine (List.ofArray argv)
        compile inputs output
        0
    with
        | CommandLineParserError msg ->
            eprintfn "Command line arguments error: %s" msg
            eprintfn ""
            eprintfn "Usage:"
            eprintfn "  trafc -o output input1 [input2] ..."
            1
