module D1

open System.Text.RegularExpressions

let parseInput (input: string) = Regex.Split(input.Trim(), @"\n") |> Array.map (fun f -> f.Trim()) |> Array.map int
let countIncreases data = data |> Array.windowed 2 |> Array.filter (fun f -> f.[0] < f.[1]) |> Array.length

let part1 input =
    countIncreases (parseInput input)
    
let part2 input =
    countIncreases ((parseInput input) |> Array.windowed 3 |> Array.map (fun f -> f |> Array.sum))
