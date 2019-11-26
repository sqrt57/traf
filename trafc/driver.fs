namespace Triton

type Driver = private { __ : unit }

module Driver =

    let createDriver() : Driver = { __ = () }

    let addSource (driver : Driver) (filename : string) (source : string) : unit =
        let short = if source.Length <= 50 then source else source.Substring(0, 50) + "..."
        printfn "Compiling source file \"%s\":\n%s\n" filename short

    let getExe (driver : Driver) : byte array =
        System.Text.Encoding.UTF8.GetBytes "Hello, world!"
