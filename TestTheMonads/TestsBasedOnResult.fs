module TestTheMonads.TestsBasedOnResult

open System
open Xunit
open Validation.Results.Example
let badInput = {nameCriteria="😎";anotherNameCriteria="😎";pageNo=0;pageSize=20000}

[<Fact>]
let ``Validate using bind`` () =
    let vc = validateInput badInput
    let s = sprintf $"%A{vc}"
    let a = match vc with
            | Ok _ -> ""
            | Error ms -> ms
    Assert.True(String.length a = 0, s)
    
[<Fact>]
let ``Validate using computation expression`` () =
    let vc = validateAll badInput
    let s = sprintf $"%A{vc}"
    let a = match vc with
            | Ok _ -> ""
            | Error ms -> ms
    Assert.True(String.length a = 0, s)
    
    

