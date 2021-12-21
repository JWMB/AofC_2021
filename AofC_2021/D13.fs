module D13

open System.Text.RegularExpressions
open Tools.GridTools

let pp = "
6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5
"

let parseInput (input: string) = 
    let sections = Regex.Split(input.Trim(), @"(\r?\n\s*){2}") |> Array.map (fun r -> r.Trim()) |> Array.filter (fun r -> r.Length > 0) |> Array.map (fun f -> Regex.Split(f, @"\r?\n"))
    {| 
        Points = sections.[0] |> Array.map (fun row -> 
            let s = row.Split(",") |> Array.map int
            { X = s.[0]; Y = s.[1]});
        Folds = sections.[1] |> Array.map (fun row -> 
            let m = Regex.Match(row, @"(?<Axis>x|y)=(?<Value>\d+)")
            if m.Success then
                let v = int m.Groups.["Value"].Value
                match m.Groups.["Axis"].Value with
                | "x" -> { X = v; Y = 0; } 
                | "y" -> { X = 0; Y = v; } 
                | _ -> raise (new System.Exception("uu"))
            else raise (new System.Exception("uu")))
    |}

let fold ptFold points =
    let toModify = points |> Array.filter (fun f -> if ptFold.X <> 0 then f.X > ptFold.X else f.Y > ptFold.Y)
    let unmodified = points |> Array.except toModify
    let modified = toModify |> Array.map (fun f -> 
        if ptFold.X <> 0 then { X = ptFold.X * 2 - f.X; Y = f.Y }
        else { X = f.X; Y = ptFold.Y * 2 - f.Y }
    )
    modified |> Array.append unmodified |> Array.distinct

let part1 input =
    let data = parseInput input
    let folded = fold data.Folds.[0] data.Points
    folded |> Array.length

let part2 input =
    let data = parseInput input
    let folded = data.Folds |> Array.fold (fun agg curr -> fold curr agg) data.Points

    let grid = Grid2D.FromCoordinates folded
    let mutable bm = [|0..grid.Size.Y-1|] |> Array.map (fun y -> [|0..grid.Size.X-1|] |> Array.map (fun x -> '.'))

    for pt in folded |> Array.map (fun pt -> { X = pt.X - grid.TopLeft.X; Y = pt.Y - grid.TopLeft.Y }) do
        bm.[pt.Y].[pt.X] <- '#'

    bm |> Array.map (fun r -> r |> Array.map string |> String.concat "") |> String.concat "\n"
