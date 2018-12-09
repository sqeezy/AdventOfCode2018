// Learn more about F# at http://fsharp.org

open System

let sizeOfField = 1000

type Claim = {
    ID       : int;
    LeftDist : int;
    TopDist  : int;
    Width    : int;
    Height   : int;
}

let claimOfArray (arr: int array) = {ID = arr.[0]; LeftDist=arr.[1]; TopDist=arr.[2]; Width=arr.[3]; Height=arr.[4]}

let parseClaim (s:string) = s.Split [|'#'; '@';' '; ','; 'x'; ':'|]
                                |> Array.filter (fun s -> (String.IsNullOrEmpty s) |> not)
                                |> Array.map int
                                |> claimOfArray

let cartesianProduct n g = List.map (fun (n,g)->(n,g)) (List.allPairs n g)

let x = [1 .. 1000]

let field = cartesianProduct x x 
                |> List.map (fun p -> (p, 0))
                |> Map.ofList

let positionsOfClaim {LeftDist = ld; TopDist = td; Width = w; Height   = h;} =
    cartesianProduct [ld .. w] [td .. h]

[<EntryPoint>]
let main argv =

    printfn "%A" (Input.dataLines |> Seq.map parseClaim)

    0 // return an integer exit code
