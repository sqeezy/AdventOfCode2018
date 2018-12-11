// Learn more about F# at http://fsharp.org
module Day03

open System

let sizeOfField = 1000

type Claim = {
    ID       : int;
    LeftDist : int;
    TopDist  : int;
    Width    : int;
    Height   : int;
}

type Position = (int * int)
type Field = Map<Position, int>

let claimOfArray (arr: int array) = {ID = arr.[0]; LeftDist=arr.[1]; TopDist=arr.[2]; Width=arr.[3]; Height=arr.[4]}

let parseClaim (s:string) = s.Split [|'#'; '@';' '; ','; 'x'; ':'|]
                                |> Array.filter (fun s -> (String.IsNullOrEmpty s) |> not)
                                |> Array.map int
                                |> claimOfArray

let positionRange = [0 .. 1000]

let initField = cartesianProduct positionRange positionRange 
                |> List.map (fun p -> (p, 0))
                |> Map.ofList

let positionsOfClaim {LeftDist = ld; TopDist = td; Width = w; Height = h;} =
    cartesianProduct [ld .. (ld+w)] [td .. (td+h)]

let applyClaimPositionToField (field : Field) pos =
    Map.add pos (field.[pos]+1) field

let applyClaimPositionsToField (field : Field) claim = claim
                                                        |> positionsOfClaim
                                                        |> List.fold applyClaimPositionToField field
let solvePartOne lines = lines
                            |> List.map parseClaim
                            |> List.fold applyClaimPositionsToField initField
                            |> Map.filter (fun _ claims -> claims > 1)
                            |> Map.count
let solve =
    printHeader 3 1
    printfn "%A" (solvePartOne Day03Input.dataLines)
