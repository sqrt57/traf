[<AutoOpen>]
module Triton.DriverModule

type Driver = private { __ : unit }

module Driver =

    let createDriver() : Driver = { __ = () }

    let addSource (driver : Driver) (filename : string) (source : string) : unit =
        printfn "Compiling source file \"%s\"" filename
        let tokens = Lexer.lex filename source
        for token in tokens do
            printfn "%O" token

    let getExe (driver : Driver) : byte array =
        System.Text.Encoding.UTF8.GetBytes "Hello, world!"
