module D7

open System.Text.RegularExpressions

let main =
    let inputX = "16,1,2,0,4,2,7,1,2,14"

    let input = System.IO.File.ReadAllText("D7.txt")

    let data = Regex.Split(input.Trim(), @",") |> Array.map int
    //data |> Array.map (fun f -> (float f)) |>Array.average
    let sorted = data |> Array.sort
    let median = sorted.[sorted.Length / 2]
    sorted |> Array.map (fun f -> abs(f - median)) |> Array.sum