[<AutoOpen>]
module Common

let splitIntoLines (multiLineString : string) = multiLineString.Split([|'\n'|]) |> List.ofArray

let cartesianProduct n g = List.map (fun (n,g)->(n,g)) (List.allPairs n g)

let printHeader day part= 
    printfn ""
    printfn "--------------------------" 
    printfn "Day %i - Part %i" day part
    printfn "--------------------------" 

let Inspect a = printfn "Type: %A - Value %A" (a.GetType()) a
                a