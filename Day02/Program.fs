// Learn more about F# at http://fsharp.org

open System

let containsTwoThree (s:string) = 
    let countOfChars = s.ToCharArray()
                        |> Seq.groupBy (id)
                        |> Seq.map (fun (_, cs)-> cs |> Seq.length)
                        |> Seq.distinct
    let containsTwo = countOfChars |> Seq.contains 2
    let containsThree = countOfChars |> Seq.contains 3
    containsTwo, containsThree

let solvePartOne list = 
    let twosAndThreesPresentPerLine = list |> Seq.map containsTwoThree
    let twos = twosAndThreesPresentPerLine |> Seq.filter (fun (b, _) -> b) |> Seq.length
    let threes = twosAndThreesPresentPerLine |> Seq.filter (fun (_, b) -> b) |> Seq.length
    twos * threes

[<EntryPoint>]
let main argv =
    printfn "Part One Result: %d" (solvePartOne Input.dataLines)
    0 // return an integer exit code
