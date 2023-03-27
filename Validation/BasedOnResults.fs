namespace Validation.Results

open System

module Example =
    
    let (>>=) inp f = Result.bind f inp
    let retn a = Ok a
    
    
    type ValidationBuilder() =
        member this.Bind(m, f) = m >>= f
        member this.Return(x) = retn x
        
    let validation = new ValidationBuilder()
    
    let validate getValue fieldName validationFunc =
        let inner input =
            let v = getValue input
            match validationFunc v with
            | Ok fieldValue -> Ok fieldValue
            | Error message -> Error $"{fieldName}: {message}" 
        inner
    
    let addMessage f message =
        let inner input =
            match f input with
            | true -> Ok input
            | _ -> Error message
        inner

    let isAscii (s:string) =
        s |> Seq.fold (fun s c -> s && Char.IsAscii(c)) true
    let isInRange min max v =
        v >= min && v <= max
        
    let isAsciiValidation =
        addMessage isAscii "Is Not ASCII"
        
    let isInRangeValidation =
        addMessage (isInRange 1 Int32.MaxValue) $"Must be between 1 and {Int32.MaxValue}"

    type InputCriteria =
        {
            nameCriteria: string
            anotherNameCriteria: string
            pageNo: int
            pageSize: int
        }
        
    let validateNameCriteria =
        validate (fun i -> i.nameCriteria) "nameCriteria" isAsciiValidation
    let validateAnotherNameCriteria =
        validate (fun i -> i.anotherNameCriteria) "anotherNameCriteria" isAsciiValidation        
    let validatePageNo =
        validate (fun i -> i.pageNo) "pageNo" isInRangeValidation
    let validatePageSize =
        validate (fun i -> i.pageSize) "pageSize" isInRangeValidation
        
        
    let validateInput input =
        validateNameCriteria input >>= fun name ->
        validateAnotherNameCriteria input >>= fun anotherName -> 
        validatePageNo input >>= fun pageNo ->
        retn (name,anotherName,pageNo)
        
    let validateAll input =
        validation {
            let! name = validateNameCriteria input
            let! anotherName = validateAnotherNameCriteria input
            let! pageNo = validatePageNo input
            let! pageSize = validatePageSize input
            return (name,anotherName,pageNo, pageSize)
        }