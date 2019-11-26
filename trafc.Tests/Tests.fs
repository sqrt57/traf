module Tests

open System
open Xunit

open Triton

[<Fact>]
let ``Driver`` () =
    let driver = Driver.createDriver()
    let binary = Driver.getExe driver
    let text = System.Text.Encoding.UTF8.GetString binary
    Assert.Equal("Hello, world!", text)
