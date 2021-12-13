module D11

open System.Text.RegularExpressions
open Tools.GridTools

let parseInput (input: string) = 
    Regex.Split(input.Trim(), @"(\r?\n\s*){1}") |> Array.map (fun f -> f.Trim()) |> Array.filter (fun f -> f.Length > 0);

let getNeighborIndices index size = (get8Neighbors (indexToPoint index size.X) size) |> Array.map (fun p -> pointToIndex p size.X)

let toString state width = state |> Seq.chunkBySize width |> Seq.map (fun f -> f |> Seq.map string |> String.concat "") |> String.concat "\n"

let performStep vals gridSize =
    let rec loop v alreadyTriggered =
        let triggeredIndices = v |> Array.mapi (fun i f -> (i, f)) |> Array.filter (fun tupl -> (snd tupl) > 9) |> Array.map (fun tupl -> fst tupl)
                                        |> Array.except alreadyTriggered
        if triggeredIndices.Length = 0 then v
        else
            let allNeighbors = triggeredIndices |> Array.map (fun f -> getNeighborIndices f gridSize) |> Array.reduce Array.append
            let numIncreasesPerIndex = Map (allNeighbors |> Array.groupBy (fun f -> f) |> Array.map (fun f -> ((fst f), (snd f).Length)))
            let newV = v |> Array.mapi (fun i f -> 
                if numIncreasesPerIndex.ContainsKey i then f + numIncreasesPerIndex.[i]
                else f)
            loop newV (triggeredIndices |> Array.append alreadyTriggered)

    let plus1 = vals |> Array.map (fun f -> f + 1)
    let result = loop plus1 [||]
    result |> Array.map (fun f -> if f > 9 then 0 else f)

let prepare input =
    let rows = parseInput input
    {| Size = { X = rows.[0].Length; Y = rows.Length };
    Values = rows |> String.concat "" |> Seq.toArray |> Array.map string |> Array.map int; |}

let part1 input =
    let data = prepare input
    let (_, numFlashes) = [|1..100|] |> Array.fold (fun agg curr -> 
                                    let state = performStep (fst agg) data.Size
                                    let numFlashes = state |> Array.filter (fun f -> f = 0) |> Array.length
                                    (state, (snd agg) + numFlashes)
                                    ) (data.Values, 0)
    numFlashes

let part2 input =
    let data = prepare input
    let rec folder vals step =
        let state = performStep vals data.Size
        if state |> Array.filter (fun f -> f > 0) |> Array.length = 0 then step
        else folder state step + 1
    folder data.Values 1
