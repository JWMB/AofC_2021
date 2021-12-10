module D9

open System.Text.RegularExpressions

type Point = { X: int; Y: int; }

let parseInput (input: string) = Regex.Split(input.Trim(), @"(\r?\n\s*){1}") |> Array.map (fun f -> f.Trim()) |> Array.filter (fun f -> f.Length > 0);

let add pt1 pt2 = { X = pt1.X + pt2.X; Y = pt1.Y + pt2.Y; }

let getSurrounding pt size = 
    let offsets = [| 1; -1; |] |> Array.map (fun f -> [| { X = f * 1; Y = 0;}; { X = 0; Y = f * 1;}|]) |> Array.reduce Array.append
    let isValid pt = pt.X >= 0 && pt.X < size.X && pt.Y >= 0 && pt.Y < size.Y
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

let getBasins map =
    let exploreBasin ptStart =
        [|0..1000|] |> Array.fold (fun (agg: {| Explored: Point []; InBasin: Point []; ToExplore: Point [] |}) curr -> 
                        if agg.ToExplore.Length = 0 then agg
                        else
                        let pt = agg.ToExplore |> Array.head
                        let newUnexplored = getSurrounding pt map.Size |> Array.except agg.ToExplore |> Array.except agg.Explored
                        let byStopping = Map (newUnexplored |> Array.groupBy (fun f -> map.getAtPoint f = 9)) // Hm, works but only b/c all basins happen to be surrounded by 9's
                        let getMapValueElseEmpty key = if byStopping.ContainsKey key then byStopping.[key] else Array.zeroCreate 0
                        {| 
                            ToExplore = agg.ToExplore |> Array.tail |> Array.append (getMapValueElseEmpty false);
                            Explored = agg.Explored |> Array.append [| pt|] |> Array.append (getMapValueElseEmpty true);
                            InBasin = agg.InBasin |> Array.append [| pt|]
                        |}
                    ) {| ToExplore = [| ptStart; |]; Explored = Array.zeroCreate<Point> 0; InBasin = Array.zeroCreate<Point> 0; |}

    let lowPoints = map.findLowPoints
    lowPoints |> Array.map exploreBasin |> Array.map (fun f -> f.InBasin)

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
    let basinsBySize = getBasins map |> Array.sortBy (fun f -> f.Length) |> Array.rev

    let html = basinsToHtml map basinsBySize
    System.IO.File.WriteAllText(@"Basins.html", html);
    let top3 = basinsBySize |> Array.take 3
    top3 |> Array.map (fun f -> f.Length) |> Array.reduce (fun agg curr -> agg * curr)
    
