module D9

open System.Text.RegularExpressions

type Point = { X: int; Y: int; }

let parseInput (input: string) = Regex.Split(input.Trim(), @"(\r?\n\s*){1}") |> Array.map (fun f -> f.Trim()) |> Array.filter (fun f -> f.Length > 0);

let getSurrounding pt size = 
    let add pt1 pt2 = { X = pt1.X + pt2.X; Y = pt1.Y + pt2.Y; }
    let isValid pt = pt.X >= 0 && pt.X < size.X && pt.Y >= 0 && pt.Y < size.Y
    let offsets = [| 1; -1; |] |> Array.map (fun f -> [| { X = f * 1; Y = 0;}; { X = 0; Y = f * 1;}|]) |> Array.reduce Array.append
    offsets |> Array.map (fun f -> add pt f) |> Array.filter isValid

type DepthMap = { Size: Point; Map: string[] } with
    member this.getAtPoint pt = int (this.Map.[pt.Y].[pt.X].ToString())
    member this.isLowPoint pt =
        let sampleVals = getSurrounding pt this.Size |> Array.map this.getAtPoint
        sampleVals |> Array.min > this.getAtPoint pt
    member this.allPointsNested =
         [|0..this.Size.Y - 1|] |> Array.map (fun y -> [|0..this.Size.X - 1|] |> Array.map (fun x -> { X = x; Y = y; }))
    member this.findLowPoints =
        let allPoints = this.allPointsNested |> Array.reduce Array.append
        allPoints |> Array.filter this.isLowPoint

let createDepthMap input = 
    let data = parseInput input
    { Size = { X = data.[0].Length; Y = data.Length }; Map = data }

let part1 input =
    let map = createDepthMap input
    map.findLowPoints |> Array.map map.getAtPoint |> Array.map (fun f -> f + 1) |> Array.sum

let exploreBasin (grid:DepthMap) pt (visited: System.Collections.Generic.List<Point>) =
    let rec loop (pt) = seq {
        visited.Add(pt)
        if grid.getAtPoint pt = 9 then ()
        else
            yield pt
            for pt2 in (getSurrounding pt grid.Size) do
                if visited.Contains(pt2) = false then yield! loop pt2
    }
    loop pt |> Seq.toList

let basinsToHtml (map: DepthMap) (basinsBySize: Point[][]) =
    let ptToBasin = Map (basinsBySize |> Array.mapi (fun i f -> f |> Array.map (fun p -> (p, i))) |> Array.reduce Array.append)

    let renderAsHtml numToColor = 
        let css = basinsBySize |> Array.mapi (fun i f -> 
            $".b{string i} {{ background-color: hsl({string (i * 360 / basinsBySize.Length)}, 100#, {if i < numToColor then 70 else 90}#); }}".Replace("#", "%")) 
                                |> String.concat "\n"
    
        let createElements row = row |> Array.map (fun p -> 
                                        let value = map.getAtPoint p
                                        match ptToBasin.TryFind(p) with
                                        | Some x -> $"<span class='b{x}'>{value}</span>"
                                        | None -> $"{value}"
                                    ) |> String.concat "\n"
    
        let elements = map.allPointsNested |> Array.map createElements |> String.concat "<br/>"
    
        $"<!DOCTYPE html><html>
        <head>
            <title>Basins!</title>
            <style>{css}</style>
        </head>
        <body>{elements}</body>
        </html> "
    renderAsHtml 3

let part2 input =
    let map = createDepthMap input

    let visited = new System.Collections.Generic.List<Point>()
    let basinsBySize = map.findLowPoints |> Array.map (fun f -> exploreBasin map f visited |> List.toArray) |> Array.sortBy (fun f -> f.Length) |> Array.rev

    let html = basinsToHtml map basinsBySize
    System.IO.File.WriteAllText(@"Basins.html", html);
    let top3 = basinsBySize |> Array.take 3
    top3 |> Array.map (fun f -> f.Length) |> Array.reduce (fun agg curr -> agg * curr)
