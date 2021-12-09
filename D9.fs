module D9

open System.Text.RegularExpressions

type Point = { X: int; Y: int; }

let add pt1 pt2 =
    { X = pt1.X + pt2.X; Y = pt1.Y + pt2.Y; }

let main =
    let input = "
    2199943210
    3987894921
    9856789892
    8767896789
    9899965678"

    let input = System.IO.File.ReadAllText("D9.txt")

    let data = Regex.Split(input.Trim(), @"(\r?\n\s*){1}") |> Array.map (fun f -> f.Trim()) |> Array.filter (fun f -> f.Length > 0);
    let size = { X = data.[0].Length; Y = data.Length }

    let getAtPoint pt =
        int (data.[pt.Y].[pt.X].ToString())

    let isLowPoint pt =
        let offsets = [| 1; -1; |] |> Array.map (fun f -> [| { X = f * 1; Y = 0;}; { X = 0; Y = f * 1;}|]) |> Array.reduce Array.append
        let isValid pt = pt.X >= 0 && pt.X < size.X && pt.Y >= 0 && pt.Y < size.Y
        let samplePts = offsets |> Array.map (fun f -> add pt f) |> Array.filter isValid
        let sampleVals = samplePts |> Array.map getAtPoint
        sampleVals |> Array.min > getAtPoint pt

    let allPoints = [|0..size.X - 1|] |> Array.map (fun x -> [|0..size.Y - 1|] |> Array.map (fun y -> { X = x; Y = y; })) |> Array.reduce Array.append

    allPoints |> Array.filter isLowPoint |> Array.map getAtPoint |> Array.map (fun f -> f + 1) |> Array.sum
