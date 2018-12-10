[<AutoOpen>]
module Common

let splitIntoLines (multiLineString : string) = multiLineString.Split([|'\n'|]) |> List.ofArray