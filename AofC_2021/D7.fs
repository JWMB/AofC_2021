module D7

open System.Text.RegularExpressions

let parseInput (input: string) = Regex.Split(input.Trim(), @",") |> Array.map int
let calc data costFunction = 
    let allPositions = [| data |> Array.min .. data |> Array.max |]
    let costs = allPositions |> Array.map (fun f -> {| Position = f; Cost = data |> Array.map (fun f2 -> costFunction f f2) |> Array.sum |})
    // printf "%O" (costs |> Array.map (fun f -> $"{f.Position}:{f.Cost}") |> String.concat "\n")
    let itemWithMinCost = costs |> Array.minBy (fun f -> f.Cost)
    itemWithMinCost.Cost

let part1 input =
    calc (parseInput input) (fun p1 p2 -> abs(p1 - p2))

let part2 input =
    calc (parseInput input) (fun p1 p2 -> 
        let diff = abs(p1 - p2)
        diff * (diff + 1) / 2
    )
