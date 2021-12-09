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

    let getSurrounding pt = 
        let offsets = [| 1; -1; |] |> Array.map (fun f -> [| { X = f * 1; Y = 0;}; { X = 0; Y = f * 1;}|]) |> Array.reduce Array.append
        let isValid pt = pt.X >= 0 && pt.X < size.X && pt.Y >= 0 && pt.Y < size.Y
        offsets |> Array.map (fun f -> add pt f) |> Array.filter isValid

    let isLowPoint pt =
        let sampleVals = getSurrounding pt |> Array.map getAtPoint
        sampleVals |> Array.min > getAtPoint pt

    let allPointsNested = [|0..size.Y - 1|] |> Array.map (fun y -> [|0..size.X - 1|] |> Array.map (fun x -> { X = x; Y = y; }))
    let allPoints = allPointsNested |> Array.reduce Array.append

    let lowPoints = allPoints |> Array.filter isLowPoint

    // part 2:
    let exploreBasin ptStart =
        [|0..1000|] |> Array.fold (fun (agg: {| Explored: Point []; InBasin: Point []; ToExplore: Point [] |}) curr -> 
                        if agg.ToExplore.Length = 0 then agg
                        else
                        let pt = agg.ToExplore |> Array.head
                        let newUnexplored = getSurrounding pt |> Array.except agg.ToExplore |> Array.except agg.Explored
                        let byStopping = Map (newUnexplored |> Array.groupBy (fun f -> getAtPoint f = 9)) // Hm, works but only b/c all basins happen to be surrounded by 9's
                        let getMapValueElseEmpty key = if byStopping.ContainsKey key then byStopping.[key] else Array.zeroCreate 0
                        {| 
                            ToExplore = agg.ToExplore |> Array.tail |> Array.append (getMapValueElseEmpty false);
                            Explored = agg.Explored |> Array.append [| pt|] |> Array.append (getMapValueElseEmpty true);
                            InBasin = agg.InBasin |> Array.append [| pt|]
                        |}
                    ) {| ToExplore = [| ptStart; |]; Explored = Array.zeroCreate<Point> 0; InBasin = Array.zeroCreate<Point> 0; |}

    let basinsBySize = lowPoints |> Array.map exploreBasin |> Array.map (fun f -> f.InBasin) |> Array.sortBy (fun f -> f.Length) |> Array.rev
    let ptToBasin = Map (basinsBySize |> Array.mapi (fun i f -> f |> Array.map (fun p -> (p, i))) |> Array.reduce Array.append)
    
    let renderAsHtml numToColor = 
        let css = basinsBySize |> Array.mapi (fun i f -> 
            $".b{string i} {{ background-color: hsl({string (i * 360 / basinsBySize.Length)}, 100#, {if i < numToColor then 70 else 90}#); }}".Replace("#", "%")) 
                               |> String.concat "\n"

        let createElements row = row |> Array.map (fun p -> 
                                        let value = getAtPoint p
                                        match ptToBasin.TryFind(p) with
                                        | Some x -> $"<span class='b{x}'>{value}</span>"
                                        | None -> $"{value}"
                                    ) |> String.concat "\n"

        let elements = allPointsNested |> Array.map createElements |> String.concat "<br/>"

        $"<!DOCTYPE html><html>
        <head>
            <title>Basins!</title>
            <style>{css}</style>
        </head>
        <body>{elements}</body>
        </html> "
    
    System.IO.File.WriteAllText(@"C:\Users\jonas\Desktop\Basins.html", renderAsHtml 3);

    let top3 = basinsBySize |> Array.take 3
    top3 |> Array.map (fun f -> f.Length) |> Array.reduce (fun agg curr -> agg * curr)

    // part 1:
    //lowPoints |> Array.map getAtPoint |> Array.map (fun f -> f + 1) |> Array.sum
