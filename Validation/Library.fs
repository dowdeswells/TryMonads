namespace Validation
open System
module Say =
    let hello name =
        printfn "Hello %s" name
        
module Framework =
    
    type ValidationResult<'a> =
        | VR of 'a * ValidationContext
    and ValidationContext =
        | Valid
        | Invalid of ValidationMessage list
    and ValidationMessage =
        V of ValidationItem
    and ValidationItem =
        {
            key: string
            messages: string list
        }
        
    
    let validate getValue fieldName validationFunc =
        let inner input =
            let v = getValue input
            match validationFunc v with
            | true, _ -> VR (input, Valid)
            | _, message -> VR (input, Invalid [ V {key=fieldName; messages=[message]} ]) 
        inner
        
    let map (f:'a->'b) (VR (input, vc)) =
        let input' = f input
        VR (input', vc)
        
    let bind (vr:ValidationResult<'a>) (f:'a -> ValidationResult<'b>) =
        let (VR (input, vc)) = vr
        let (VR (input', vc')) = f input
        match vc with
        | Valid -> VR (input', vc')
        | Invalid m1 ->
            match vc' with
            | Valid -> VR (input', vc')
            | Invalid m2 -> VR (input', Invalid (m1 @ m2))
            
    let retn input =
        VR (input, Valid)
        
    let (>>=) = bind
    
    type ValidationBuilder() =
        member this.Bind(m, f) = bind m f
        member this.Return(x) = retn x
        
    let validation = new ValidationBuilder()
    
    let addMessage f message =
        let inner input =
            match f input with
            | true -> (true, input)
            | _ -> (false, message)
        inner
   
    let isAscii (s:string) =
        s |> Seq.fold (fun s c -> s && Char.IsAscii(c)) true
        
    let isAsciiValidation =
        addMessage isAscii "Is Not ASCII"

module Example =
    
    open Framework
    
    type InputCriteria =
        {
            nameCriteria: string
            anotherNameCriteria: string
        }
        
    let validateNameCriteria =
        validate (fun i -> i.nameCriteria) "nameCriteria" isAsciiValidation
    let validateAnotherNameCriteria =
        validate (fun i -> i.anotherNameCriteria) "anotherNameCriteria" isAsciiValidation
    
    let validateAll input =
        validateNameCriteria input >>= validateAnotherNameCriteria