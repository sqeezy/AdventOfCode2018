// Learn more about F# at http://fsharp.org
module Day02

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

let differentByOne (a, b) = 
    let compareElementwise = Seq.zip a b 
                              |> Seq.map (fun (a,b)-> if a=b then Some a else None)

    let differentElements = compareElementwise
                                  |> Seq.filter(Option.isNone)
    let sameElements = compareElementwise
                          |> Seq.filter Option.isSome
                          |> Seq.map    Option.get

    let numOfDifferentElements = differentElements |> Seq.length

    match numOfDifferentElements with
    | 1 -> Some sameElements
    | _ -> None

let solvePartTwo (list : string list) = cartesianProduct list list
                                            |> Seq.map    (fun (a, b) -> (a.ToCharArray(), b.ToCharArray()))
                                            |> Seq.map    differentByOne
                                            |> Seq.filter Option.isSome
                                            |> Seq.head 
                                            |> Option.get 
                                            |> Array.ofSeq 
                                            |> System.String

let solve =
    printHeader 2 1
    printfn "%A" (solvePartOne Day02Input.dataLines)
    printHeader 2 2
    printfn "%A" (solvePartTwo Day02Input.dataLines )
