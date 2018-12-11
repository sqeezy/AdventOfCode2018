// Learn more about F# at http://fsharp.org
module Day03

open System

let sizeOfCloth = 1000

type Claim = {
    ID       : int;
    LeftDist : int;
    TopDist  : int;
    Width    : int;
    Height   : int;
}

type Position = (int * int)
type Cloth = Map<Position, int>

let claimOfArray (arr: int array) = {ID = arr.[0]; LeftDist=arr.[1]; TopDist=arr.[2]; Width=arr.[3]; Height=arr.[4]}

let parseClaim (s:string) = s.Split [|'#'; '@';' '; ','; 'x'; ':'|]
                                |> Array.filter (fun s -> (String.IsNullOrEmpty s) |> not)
                                |> Array.map int
                                |> claimOfArray

let positionRange = [0 .. 1000]

let initCloth = cartesianProduct positionRange positionRange 
                |> List.map (fun p -> (p, 0))
                |> Map.ofList

let positionsOfClaim {LeftDist = ld; TopDist = td; Width = w; Height = h;} =
    cartesianProduct [ld .. (ld+w - 1)] [td .. (td+h - 1)]

let applyClaimPositionToCloth (cloth : Cloth) pos =
    Map.add pos (cloth.[pos]+1) cloth

let applyClaimPositionsToCloth (cloth : Cloth) claim = claim
                                                        |> positionsOfClaim
                                                        |> List.fold applyClaimPositionToCloth cloth

let claimValid (cloth:Cloth) (claim:Claim) =
    let positions = claim |> positionsOfClaim
    List.forall (fun p -> cloth.[p] < 2) positions

let solveBoth lines = 
    let claims = lines |> List.map parseClaim
    let clothWithClaims = claims |> List.fold applyClaimPositionsToCloth initCloth
    let overusedClothSquareInchesAmount = clothWithClaims |> Map.filter (fun _ claims -> claims > 1) |> Map.count
    let validClaimId = (claims |> List.filter (claimValid clothWithClaims) |> List.head).ID
    (overusedClothSquareInchesAmount, validClaimId)
let solve =
    let (r1, r2) = (solveBoth Day03Input.dataLines) 
    printHeader 3 1
    printfn "%i" r1
    printHeader 3 2
    printfn "%i" r2
