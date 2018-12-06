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

let rec findFirstDoubleFreq {Sum=sum;PastFreqs = pastFreqs} currentLines =
    let line :: lines = currentLines
    let nextFreq = addLineToSum sum line
    if Set.contains nextFreq pastFreqs then nextFreq
    else 
        let newPastFreqs = Set.add nextFreq pastFreqs
        findFirstDoubleFreq {Sum=nextFreq; PastFreqs= newPastFreqs } lines

[<EntryPoint>]
let main argv =
    let result1 = dataLines |> Seq.fold addLineToSum 0
    printfn "Part One Result: %i" result1

    let loop = seq {while true do yield! dataLines} |> Seq.take 1000000 |> List.ofSeq
    let result2 = findFirstDoubleFreq {Sum=0;PastFreqs=Set.empty} loop
    printfn "Part Two Result: %i" result2
    0 // return an integer exit code
