module Triton.Tests.Driver

open System
open Xunit
open Swensen.Unquote

open Triton

[<Fact>]
let ``Driver`` () =
    let driver = Driver.createDriver()
    let binary = Driver.getExe driver
    let text = System.Text.Encoding.UTF8.GetString binary
    test <@ "Hello, world!" = text @>
