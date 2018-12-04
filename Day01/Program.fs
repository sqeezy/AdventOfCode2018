open System

let (|Prefix|_|) (p:string) (s:string) =
    if s.StartsWith(p) then
        Some(s.Substring(p.Length))
    else
        None


let addLineToSum sum line =
    match line with
    | Prefix "-" rest -> sum - (rest |> int)
    | Prefix "+" rest -> sum + (rest |> int) 
    | _               -> sum


[<EntryPoint>]
let main argv =
    let result = dataLines |> Seq.fold addLineToSum 0
    printfn "%i" result
    0 // return an integer exit code
