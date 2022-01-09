module D22

open System.Text.RegularExpressions
open Tools.Tools3D
let parseInput (input: string) =
    let parseLine (line : string) =
        let m = Regex.Match(line.Trim(), @"(?<action>(on|off))\s+(?<ranges>(?<axis>(x|y|z))=(?<min>-?\d+)\.\.(?<max>-?\d+),?){3}")

        let byAxis = Map (
            [0..2] |> List.map(fun i -> 
                let axis = m.Groups["axis"].Captures[i].Value
                let minMax = [int m.Groups["min"].Captures[i].Value; int m.Groups["max"].Captures[i].Value] |> List.sort
                (axis, minMax)
            ))
        let getV i = { X = byAxis["x"][i]; Y = byAxis["y"][i];  Z = byAxis["z"][i]; }
        let min = getV 0
        let max = getV 1

        ( m.Groups["action"].Value = "on", (min, max) )


    Regex.Split(input.Trim(), @"\r?\n") |> Array.map parseLine |> Array.toList

//let getInsideOther (c1: Grid3D) (c2: Grid3D) =
//    let intersect = c2.Intersect c1
//    if intersect.Size = c2.Size then
//        Some((0, c1))
//    else if intersect.Size = c1.Size then
//        Some((1, c2))
//    else None

//let calcNestedCubeVolume (cubeLists: Grid3D list list) =
//    let volumes = cubeLists |> List.map (fun cubes -> cubes |> List.map (fun cube -> bigint cube.Volume) |> List.sum)
//    let signed = volumes |> List.mapi (fun i f -> ((if i % 2 = 0 then 1 else -1), f))
//    let total = signed |> List.fold (fun agg (sign, value) -> 
//                        if sign = 1 then agg + value
//                        else agg - value
//                    ) 0I
//    total

//let consolidate (cubes: Grid3D list) =
//    let toRemove = [0..cubes.Length-2] |> List.map (fun i1 ->
//                                                let c1 = cubes[i1]
//                                                [i1+1..cubes.Length-1] |> List.map (fun i2 ->
//                                                                    let c2 = cubes[i2]
//                                                                    let intersect = c2.Intersect c1
//                                                                    if intersect.Size = c2.Size then
//                                                                        Some(i1)
//                                                                    else if intersect.Size = c1.Size then
//                                                                        Some(i2)
//                                                                    else None
//                                                                ) |> List.filter(fun f -> f.IsSome) |> List.map (fun f -> f.Value)
//                                            )

//    if toRemove.Length > 0 then
//        let flattened = toRemove |> List.reduce List.append |> List.distinct
//        cubes |> List.mapi(fun i c -> (i, c)) |> List.filter(fun (i, _) -> flattened |> List.contains i) |> List.map (fun (_, c) -> c)
//    else
//        cubes

let performCubeActions (actionCubes: (bool * Grid3D) list) =
    let folder (agg: Grid3D list) (action, (cube: Grid3D)) =
        let indexed = agg |> List.mapi (fun i c -> (i, c))
        let indexedMap = Map indexed
        let intersections = indexed |> List.map (fun (i, c) -> (i, c.Intersect cube))
        let byHasVolume = Map (intersections |> List.groupBy (fun (i, c) -> c.Volume > 0))

        let ooo =
            if byHasVolume.ContainsKey true then
                let non0intersects = byHasVolume[true]
                // filter out all that are completely inside cube
                let partialIntersect = non0intersects |> List.filter (fun (i, c) -> c.Volume <> indexedMap[i].Volume)
                if partialIntersect.Length = 0 then []
                else
                    let originals = partialIntersect |> List.map (fun (i, _) -> indexedMap[i])
                    let subtractedFromOriginals = originals |> List.map (fun c -> c.Subtract cube) |> List.reduce List.append
                    subtractedFromOriginals
            else
                []

        let nonIntersected = if byHasVolume.ContainsKey(false) then byHasVolume[false] |> List.map (fun (i, _) -> indexedMap[i]) else []
        let subtracted = nonIntersected |> List.append ooo
        if action then
            subtracted |> List.append [cube]
        else
            subtracted

    actionCubes |> List.fold (fun agg curr -> folder agg curr) []

let performCubeActionsNaive (actionCubes: (bool * Grid3D) list) =
    let ons = (Array.zeroCreate<Vector3D> 0) |> Array.toList

    let folded = actionCubes |> List.fold (fun agg (action, cube) -> //(min, max)
                        let min = cube.TopLeft
                        let max = cube.BottomRight
                        let points = [min.X..max.X] |> List.map(fun x -> //min.X..max.X
                                            [min.Y..max.Y] |> List.map(fun y -> //min.Y..max.Y
                                                [min.Z..max.Z] |> List.map(fun z -> //min.Z..max.Z
                                                    { X = x; Y = y; Z = z; }
                                                )
                                            ) |> List.reduce List.append
                                       ) |> List.reduce List.append
                        if action then
                            (agg |> List.append points) |> List.distinct
                        else
                            agg |> List.except points
                    ) ons
    folded.Length

type PartitionedSpace = { Bounds: Grid3D; Partitions: Grid3D list; PartitionSize: Vector3D; Splits: int list} with
    member this.getPartitionIndex (v: Vector3D) =
        let index = (v.Sub this.Bounds.TopLeft).Div this.PartitionSize
        { X = System.Math.Min(index.X, this.Splits[0]-1); Y = System.Math.Min(index.Y, this.Splits[1]-1); Z = System.Math.Min(index.Z, this.Splits[2]-1); }

    member this.getPartitionCubeFromIndex (index: Vector3D) =
        this.Partitions[index.X * this.Splits[2] * this.Splits[1] + index.Y * this.Splits[2] + index.Z]

    member this.splitIntoPartitions (cube: Grid3D) =
        let partitionTL = this.getPartitionIndex cube.TopLeft
        let partitionBR = this.getPartitionIndex cube.BottomRight
        if partitionBR = partitionTL then
            [(partitionTL, cube)]
        else
            let allPartitions = Tools.Combinatorics.listCombos [[partitionTL.X..partitionBR.X];[partitionTL.Y..partitionBR.Y];[partitionTL.Z..partitionBR.Z];] |> Seq.toList
            let split = allPartitions |> List.map (fun part -> 
                let vpart = Vector3D.FromList part
                let pcube = this.getPartitionCubeFromIndex vpart
                let intersection = cube.Intersect pcube
                (vpart, intersection)
            )
            split |> List.filter (fun (_, intr) -> intr.Volume > 0)

    static member Create (bounding: Grid3D, splits_: Vector3D) =
        if bounding.Size.X < splits_.X || bounding.Size.Y < splits_.Y || bounding.Size.Z < splits_.Z then
            raise (new System.Exception($"Cannot split {bounding}"))
        let splits = splits_.ToList
        let createPartitions (splits: int list) =
            let partitionSize = bounding.Size.ToList |> List.mapi (fun i f ->  f / splits[i])
            let leftovers = bounding.Size.ToList |> List.mapi (fun i f ->  f % splits[i])
            let combos = Tools.Combinatorics.listCombos (splits |> List.map (fun f -> [0..f-1])) |> Seq.toList |> List.sortBy(fun l -> l[0], l[1], l[2])
            let cubes = combos |> List.map (fun lst ->
                let offset = partitionSize |> List.mapi (fun i v -> v * lst[i])
                let size = 
                    partitionSize |> List.mapi (fun i v -> v + (if lst[i] = splits[i]-1 then leftovers[i] else 0))
                { TopLeft = bounding.TopLeft.Add (Vector3D.FromList offset); Size = Vector3D.FromList size; }
            )
            cubes
        let partitions = createPartitions splits
        let partitionSize = bounding.Size.ToList |> List.mapi (fun i f ->  f / splits[i]) |> Vector3D.FromList

        { Bounds = bounding; Partitions = partitions; PartitionSize = partitionSize; Splits = splits; }


type PartitionedData = { Space: Grid3D; ActionCubes: (bool * Grid3D) list; Subspaces: PartitionedData list; } with
    member this.GetFlattened =
        let rec loop pd = seq {
            if pd.ActionCubes.Length > 0 then yield pd.ActionCubes
            else
                for s in pd.Subspaces do
                    yield! loop s
        }
        loop this |> Seq.toList

    static member Create (actionCubes: (bool * Grid3D) list) limit bounds =
        let byCube = actionCubes |> List.groupBy (fun (a, c) -> c)

        let tooSmallToDivide = bounds.Size.X < 2 || bounds.Size.Y < 2 || bounds.Size.Z < 2
        if actionCubes.Length < limit || byCube.Length = 1 || tooSmallToDivide then
            if byCube.Length = 1 && fst (byCube |> List.head) = bounds then
                { ActionCubes = [ actionCubes |> List.last ]; Subspaces = []; Space = bounds; }
            else 
                { ActionCubes = actionCubes; Subspaces = []; Space = bounds; }
        else
            let partitioning = PartitionedSpace.Create(bounds, { X = 2; Y = 2; Z = 2; })
            let partitioned = actionCubes |> List.map(fun (action, cube) ->
                                                let split = partitioning.splitIntoPartitions cube
                                                split |> List.map (fun (part, c) -> (part, (action, c)))
                                            ) |> List.reduce List.append
            let grouped = partitioned |> List.groupBy (fun (pt, _) -> pt)
                                |> List.map (fun (key, data) -> (partitioning.getPartitionCubeFromIndex(key), data |> List.map (fun (_, d) -> d)))
            let xxx = grouped |> List.filter (fun (k, v) -> v.Length > 0) |> List.map (fun (k, v) -> PartitionedData.Create v limit k)
            { ActionCubes = []; Subspaces = xxx; Space = bounds; }

let part1 input =
    let raw = parseInput input
    let validCube = Grid3D.FromCoordinates [| { X = -50; Y = -50; Z = -50; }; { X = 50; Y = 50; Z = 50; } |]
    //let valid = raw |> List.filter (fun (action, (min, max)) -> validCube.IsValid min && validCube.IsValid max)

    let actionCubes = raw |> List.map (fun (action, (min, max)) -> (action, Grid3D.FromCoordinates [| min;max |])) 
                        |> List.filter (fun (_, cube) -> (cube.Intersect validCube).Volume = cube.Volume)

    //let bounding = validCube //Grid3D.GetBounding (actionCubes |> List.map (fun (_, cube) -> cube)) 
    //let partitioning = PartitionedSpace.Create(bounding, {X = 2; Y = 2; Z = 2;})
    //let partitioned = actionCubes |> List.map(fun (action, cube) ->
    //                                    let split = partitioning.splitIntoPartitions cube
    //                                    split |> List.map (fun (part, c) -> (part, (action, c)))
    //                                ) |> List.reduce List.append
    //let byPartition = partitioned |> List.groupBy (fun f -> fst f)
    //let volumes = byPartition |> List.map (fun (_, f) -> f |> List.map(fun (_, f2) -> f2)) |> List.map (fun f -> performCubeActionsNaive f)
    
    let cubes = performCubeActions actionCubes
    let volumes = cubes |> List.map (fun f -> f.Volume)

    let total = int (volumes |> List.map (fun v -> bigint v) |> List.sum)

    //let naiveResult = performCubeActionsNaive actionCubes
    //naiveResult //598616
    total

let part2 input =
    let raw = parseInput input

    let actionCubes = raw |> List.map (fun (action, (min, max)) -> (action, Grid3D.FromCoordinates [| min;max |]))

    let addCubes = actionCubes |> List.filter (fun (action, _) -> action) |> List.map(fun (_, cube) -> cube)
    let bounding = Grid3D.GetBounding addCubes

    let partitioned = (PartitionedData.Create actionCubes 20 bounding).GetFlattened
    let cubes = partitioned |> List.map (fun f -> performCubeActions f)
    let volumes = cubes |> List.map (fun r -> r |> List.map (fun cube -> cube.Volume)) |> List.reduce List.append
    let total = volumes |> List.map (fun v -> bigint v) |> List.sum
    uint64 total
