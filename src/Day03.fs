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
    cartesianProduct [ld .. (ld+w - 1)] [td .. (td+h - 1)]

let applyClaimPositionToField (field : Field) pos =
    Map.add pos (field.[pos]+1) field

let applyClaimPositionsToField (field : Field) claim = claim
                                                        |> positionsOfClaim
                                                        |> List.fold applyClaimPositionToField field

let claimValid (field:Field) (claim:Claim) =
    let positions = claim |> positionsOfClaim
    List.forall (fun p -> field.[p] < 2) positions

let solveBoth lines = 
    let claims = lines |> List.map parseClaim
    let fieldWithClaims = claims |> List.fold applyClaimPositionsToField initField
    let overusedFieldSectionsNum = fieldWithClaims |> Map.filter (fun _ claims -> claims > 1) |> Map.count
    let validClaimId = (claims |> List.filter (claimValid fieldWithClaims) |> List.head).ID
    (overusedFieldSectionsNum, validClaimId)
let solve =
    let (r1, r2) = (solveBoth Day03Input.dataLines) 
    printHeader 3 1
    printfn "%A" r1
    printHeader 3 2
    printfn "%A" r2
