module D7

open System.Text.RegularExpressions

let main =
    let input = "16,1,2,0,4,2,7,1,2,14"

    let input = System.IO.File.ReadAllText("D7.txt")

    let data = Regex.Split(input.Trim(), @",") |> Array.map int
    
    let costFunctionPart1 p1 p2 = abs(p1 - p2)
    let costFunctionPart2 p1 p2 = 
        let diff = abs(p1 - p2)
        diff * (diff + 1) / 2
    let costFunction = costFunctionPart2

    let allPositions = [| data |> Array.min .. data |> Array.max |]
    let costs = allPositions |> Array.map (fun f -> {| Position = f; Cost = data |> Array.map (fun f2 -> costFunction f f2) |> Array.sum |})
    // printf "%O" (costs |> Array.map (fun f -> $"{f.Position}:{f.Cost}") |> String.concat "\n")
    let itemWithMinCost = costs |> Array.minBy (fun f -> f.Cost)
    itemWithMinCost.Cost
