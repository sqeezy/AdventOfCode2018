module Day01

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
    | _               -> raise (ArgumentException("There can not be any non ['-','+'] symbols here."))

type State = {Sum : int; PastFreqs : Set<int>}

let rec findFirstDoubleFreq {Sum = sum; PastFreqs = pastFreqs} currentLines =
    match currentLines with
    | line :: remainingLines -> 
        let nextFreq = addLineToSum sum line
        if Set.contains nextFreq pastFreqs then nextFreq
        else 
            let newPastFreqs = Set.add nextFreq pastFreqs
            findFirstDoubleFreq {Sum=nextFreq; PastFreqs= newPastFreqs } remainingLines
    | _ -> raise (Exception("Challange states this never happens!"))

let solve =
    let result1 = Day01Input.dataLines |> Seq.fold addLineToSum 0
    printHeader 1 1
    printfn "%i" result1

    let loop = seq {while true do yield! Day01Input.dataLines} |> Seq.take 1000000 |> List.ofSeq
    let result2 = findFirstDoubleFreq {Sum=0;PastFreqs=Set.empty} loop
    printHeader 1 2
    printfn "%i" result2
