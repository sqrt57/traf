module Tests

open System
open Xunit

[<Fact>]
let ``Driver`` () =
    let driver = Drive.createDriver()
    let binary = Drive.getExe driver
    let text = System.Text.Encoding.UTF8.GetString binary
    Assert.Equal("Hello, world!", text)
