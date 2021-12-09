module D1

open System.Text.RegularExpressions

let main =
    let input = "
    199
    200
    208
    210
    200
    207
    240
    269
    260
    263
    "
    let input = System.IO.File.ReadAllText("D1.txt")

    let data = Regex.Split(input.Trim(), @"\n") |> Array.map (fun f -> f.Trim()) |> Array.map int

    let countIncreases data = data |> Array.windowed 2 |> Array.filter (fun f -> f.[0] < f.[1]) |> Array.length
    
    // part 1:
    //countIncreases data
    // part 2:
    countIncreases (data |> Array.windowed 3 |> Array.map (fun f -> f |> Array.sum))
