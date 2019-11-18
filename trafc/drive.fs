module Drive

type Driver = Driver

let createDriver() : Driver = Driver

let addSource (driver : Driver) (source : string) : unit =
    let short = if source.Length <= 50 then source else source.Substring(0, 50) + "..."
    printfn "Compiling source: %s" short

let getExe (driver : Driver) : byte array =
    System.Text.Encoding.UTF8.GetBytes "Hello, world!"
