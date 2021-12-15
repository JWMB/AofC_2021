module D15

open Tools.GridTools
open System.Text.RegularExpressions
open System.Collections.Generic

let mutateMap flatData increase =
    flatData |> Array.map (fun f -> ((f + increase - 1) % 8) + 1)

let printGrid grid (flattened: 'a[]) =
    [|0..grid.Size.Y-1|] |> Array.map (fun y -> 
        [|0..grid.Size.X-1|] |> Array.map (fun x ->
            let pt = { X = x; Y = y; }
            let index = grid.PointToIndex pt
            flattened[index]
        ) |> Array.map (fun c -> c.ToString().PadLeft(1, ' ')) |> String.concat ""
    ) |> String.concat "\n"

let printCosts grid shortest =
    [|0..grid.Size.Y-1|] |> Array.map (fun y -> 
        [|0..grid.Size.X-1|] |> Array.map (fun x ->
            let pt = { X = x; Y = y; }
            let found = shortest[pt]
            fst found
        ) |> Array.map (fun c -> c.ToString().PadLeft(3, ' ')) |> String.concat " "
    ) |> String.concat "\n"

let parseInput (input: string) =
    Regex.Split(input.Trim(), "\r?\n") |> Array.map (fun row -> row |> Seq.toArray |> Array.map string |> Array.map int)

let mutateGrid (data: int[][]) (bigMapSizeFactors: Vector2D) = //multiplyFactors
    let orgDataSize = { X = data.Length; Y = data.[0].Length; }
    let finalSize = { X = orgDataSize.X * bigMapSizeFactors.X; Y = orgDataSize.Y * bigMapSizeFactors.Y; }
    let mutated = [|0..finalSize.Y-1|] |> Array.map(fun y ->
                        [|0..finalSize.X-1|] |> Array.map(fun x ->
                            let orgPt = { X = x % orgDataSize.X; Y = y % orgDataSize.Y; }
                            let bigSectionPt = { X = x / orgDataSize.X; Y = y / orgDataSize.Y; }
                            let increase = bigSectionPt.X + bigSectionPt.Y
                            ((data.[orgPt.Y].[orgPt.X] + increase - 1) % 9) + 1
                        )
                )
    mutated

let getCostPerCoordinate (data: int[][]) grid ptStart ptEnd =
    let getValueAtPoint pt = data.[pt.Y].[pt.X]

    let shortest = new Dictionary<Vector2D, (int * Vector2D)>()
    // for each coordinate, keep shortest path to goal
    let unexplored = new SortedDictionary<int, HashSet<Vector2D>>()
    let addUnexplored pt cost =
        let (found, hashset) = unexplored.TryGetValue cost
        if found then
            hashset.Add(pt) |> ignore
        else
            let newHashset = new HashSet<Vector2D>()
            newHashset.Add(pt) |> ignore
            unexplored.Add(cost, newHashset)
    let removeUnexplored pt cost =
        let hashset = unexplored[cost]
        hashset.Remove(pt) |> ignore
        if hashset.Count = 0 then
            unexplored.Remove(cost) |> ignore

    shortest.Add(ptStart, (0, ptStart))
    addUnexplored ptStart 0

    while unexplored.Count > 0 do
        let xxx = unexplored |> Seq.head
        let minUnexploredCost = xxx.Key
        let pt = xxx.Value |> Seq.head
        let neighbors = get4Neighbors pt grid.Size
        let nbsInfo = neighbors |> Array.map(fun p -> 
            let found = match shortest.TryGetValue p with
            | true, value -> Some(value)
            | _           -> None
            (p, found)
        )
        let explored = nbsInfo |> Array.filter(fun n -> (snd n).IsSome) |> Array.map (fun n -> (fst n, (snd n).Value))

        let totalCost =
            if explored.Length > 0 then //
                let best = explored |> Array.sortBy (fun n -> fst (snd n)) |> Array.head;
                let totalCost = (fst (snd best)) + getValueAtPoint(pt)
                if shortest.ContainsKey(pt) then
                    if (fst shortest[pt]) > totalCost then
                        shortest[pt] <- (totalCost, fst best)
                else
                    shortest.Add(pt, (totalCost, fst best))
                totalCost
            else
                getValueAtPoint(pt)
        removeUnexplored pt minUnexploredCost

        let newUnexplored = nbsInfo |> Array.filter(fun n -> (snd n).IsNone) |> Array.map (fun n -> fst n)
        for unx in newUnexplored do
            addUnexplored unx totalCost
    shortest

let part1 (input: string) =
    let data = parseInput input
    let grid = { Size = { X = data.Length; Y = data.[0].Length; }; TopLeft = Vector2D.Zero }

    let allCosts = getCostPerCoordinate data grid grid.TopLeft grid.BottomRight
    let (found, info) = allCosts.TryGetValue(grid.BottomRight)
    if found = false then raise (new System.Exception("No path to end node!"))

    fst info

let part2 (input: string) =
    let data = mutateGrid (parseInput input) { X = 5; Y = 5;}
    let grid = { Size = { X = data.Length; Y = data.[0].Length; }; TopLeft = Vector2D.Zero }

    let allCosts = getCostPerCoordinate data grid grid.TopLeft grid.BottomRight
    let (found, info) = allCosts.TryGetValue(grid.BottomRight)
    if found = false then raise (new System.Exception("No path to end node!"))

    fst info
