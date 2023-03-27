module Tests

open System
open Xunit
open Validation.Example
open Validation.Framework
let badInput = {nameCriteria="😎";anotherNameCriteria="😎"}

[<Fact>]
let ``My test`` () =
    let vc = validateAll badInput
    let s = sprintf $"%A{vc}"
    let a = match vc with
            | VR (input, Valid) -> true
            | VR (input, Invalid validationMessages) -> false
    Assert.True(a, s)