module D19

open System.Text.RegularExpressions
open Tools.Tools3D
let parseInput (input: string) =
    let commentsRemoved = Regex.Split(input, @"\r?\n") |> Array.map (fun l -> (Regex.Split(l, @"\/\/")[0]).Trim()) |> String.concat "\n"
    let sections = Regex.Split(commentsRemoved, @"\r?\n?--- ") |> Array.filter (fun s -> s.Length > 0)
    let parsed = sections |> Array.map (fun section -> 
        let lines = Regex.Split(section.Trim(), "\r?\n")
        let scannerIdMatch = Regex.Match(lines |> Array.head, @"(?<=scanner\s*)\d+")
        let scannerId = int scannerIdMatch.Value 
        let coords = lines |> Array.tail |> Array.map(fun line -> 
            let items = line.Split(',') |> Array.map int
            { X = items[0]; Y = items[1]; Z = items[2]; }
        )
        (scannerId, coords |> Array.toList)
    )
    Map(parsed)

let rec distribute e = function
| [] -> [[e]]
| x::xs' as xs -> (e::xs)::[for xs in distribute e xs' -> x::xs]

let rec permute = function
| [] -> [[]]
| e::xs -> List.collect (distribute e) (permute xs)

let permutations depth aList =
    let rec loop currDepth generated = seq {
        for item in aList do
            let x = [item] |> List.append generated
            if currDepth = depth then yield x
            else yield! loop (currDepth+1) x
    }
    loop 1 [] |> Seq.toList

let combinations size aList = 
    let rec pairHeadAndTail acc bList = 
        match bList with
        | [] -> acc
        | x::xs -> pairHeadAndTail (List.Cons ((x,xs),acc)) xs
    let remainderAfter = aList |> pairHeadAndTail [] |> Map.ofList
    let rec comboIter n acc = 
        match n with
        | 0 -> acc
        | _ -> 
            acc
            |> List.fold (fun acc alreadyChosenElems ->
                match alreadyChosenElems with
                | [] -> aList //Nothing chosen yet, therefore everything remains.
                | lastChoice::_ -> remainderAfter.[lastChoice]
                |> List.fold (fun acc elem ->
                    List.Cons (List.Cons (elem,alreadyChosenElems),acc)
                ) acc
            ) []
            |> comboIter (n-1)
    comboIter size [[]]

type Scanner(points: Vector3D list) =
    let distancePairs =
        let ids = points |> List.mapi (fun i f -> i)
        let idCombos = ids |> combinations 2
        let idsToDistance = idCombos |> List.map(fun pair -> 
            let tuple = (pair[0], pair[1])
            (tuple, (points[fst tuple].Sub points[snd tuple]).LengthSquare)
        )
        idsToDistance
    member this.Points = points
    member this.DistancePairs = distancePairs
    member this.AllDistances = distancePairs |> List.map(fun (_, dist) -> dist)
    member this.DistancesOneToOthers =
        let expanded1 = distancePairs |> List.map (fun ((i1, i2), dist) -> (i1, (i2, dist)))
        let expanded2 = distancePairs |> List.map (fun ((i1, i2), dist) -> (i2, (i1, dist)))
        let expanded = (expanded1 |> List.append expanded2)
        expanded |> List.groupBy(fun (id, _) -> id) |> List.map (fun f -> (fst f, snd f |> List.map(fun sub -> snd sub) |> List.sortBy (fun (a, b) -> b)))// |> List.sortBy (fun f -> f)

//let rotatePoints (pts: Vector3D list) num90Steps =
let intersect l1 l2 = Set.intersect (Set.ofList l1) (Set.ofList l2) |> Set.toList

let pointsToString pts = pts |> List.map (fun pt -> $"{pt.X},{pt.Y},{pt.Z}") |> String.concat "\n"

let allTransformationCombos =
    let axisSignVariations = (permutations 3 [-1;1;]) |> List.map (fun f -> { X = f[0]; Y = f[1]; Z = f[2]; })
    let rotations = permute [0;1;2;] //Stupid - yields several identical results
    axisSignVariations |> List.map (fun signs -> rotations |> List.map (fun rot -> (rot, signs))) |> List.reduce List.append
    
let transformer rotation signs (pt: Vector3D) =
    let signed = (pt.Mult signs).ToList
    rotation |> List.map (fun i -> signed[i]) |> Vector3D.FromList

let getDistances lst =
    let start = lst |> List.head
    lst |> List.map (fun (pt: Vector3D) -> (pt.Sub start).Length)

let part1 input =
    let data = parseInput input
    let scanners = Map (data |> Map.toList |> List.map (fun (id, points) -> (id, Scanner points)))

    let align scannerId1 scannerId2 numOverlappingRequired =
        let pts1ToOthers = scanners[scannerId1].DistancesOneToOthers
        let pts2ToOthers = scanners[scannerId2].DistancesOneToOthers

        let ptsWithSimilarDistances =
            pts1ToOthers |> List.map (fun (i1, dists1) -> 
                    let overlaps = pts2ToOthers |> List.map (fun (i2, dists2) -> 
                        let overlap = intersect (dists1 |> List.map(fun f -> snd f)) (dists2 |> List.map(fun f -> snd f))
                        //printfn $"{i1}->{i2} {overlap.Length}"
                        if overlap.Length >= numOverlappingRequired-1 then
                            let getOverlappingPoints ptIndexDistPairs =
                                ptIndexDistPairs |> List.filter(fun (_, dist) -> overlap |> List.contains dist) |> List.map (fun (ptIndex, _) -> ptIndex)

                            Some((getOverlappingPoints dists1 |> List.append [i1], getOverlappingPoints dists2 |> List.append [i2]))
                        else None
                    )
                    let found = overlaps |> List.filter (fun f -> f.IsSome) |> List.map (fun f -> f.Value)
                    found
                ) |> List.reduce List.append

        if ptsWithSimilarDistances.Length >= numOverlappingRequired-1 then
            printfn $"Similars: {scannerId1}->{scannerId2} {ptsWithSimilarDistances.Length}"
            // Might have "false positives" - filter out those 
            let countById nested = nested |> List.reduce List.append |> List.groupBy (fun f -> f) |> List.map (fun (key, grp) -> (key, grp.Length))
            let getMajorityIds nested =
                let byId = countById nested
                let idsWithHighestCount = byId |> List.groupBy (fun (id, cnt) -> cnt) |> List.map(fun (cnt, grp) -> (cnt, grp |> List.map(fun x -> fst x))) 
                                        |> List.sortBy (fun (cnt, ids) -> cnt) |> List.rev |> List.head
                let (_, ids) = idsWithHighestCount
                ids

            let pts1 = ptsWithSimilarDistances |> List.map (fun f -> fst f) |> getMajorityIds 

            let matchedIndices = ptsWithSimilarDistances |> List.tryFind (fun (p1, p2) -> 
                let intersect = intersect pts1 p1
                if intersect.Length <> p1.Length then false
                else
                    let x1 = getDistances (p1 |> List.map (fun i -> scanners[scannerId1].Points[i]))
                    let x2 = getDistances (p2 |> List.map (fun i -> scanners[scannerId2].Points[i]))
                    x1 = x2
            )

            if matchedIndices.IsNone then None
            else
                let mapToNewOrigo (ptNewOrigo: Vector3D) pts = pts |> List.map (fun (pt: Vector3D) -> pt.Sub ptNewOrigo)

                let (indices1, indices2) = matchedIndices.Value
                let points1 = indices1 |> List.map(fun p -> scanners[scannerId1].Points[p])
                let points2 = indices2 |> List.map(fun p -> scanners[scannerId2].Points[p])

                let translated1 = mapToNewOrigo points1[0] points1
                let translated2 = mapToNewOrigo points2[0] points2

                let found = allTransformationCombos
                            |> List.toSeq
                            |> Seq.map (fun (rot, signs) -> 
                                let transform = transformer rot signs
                                let transformed = translated2 |> List.map (fun pt -> transform pt)
                                ((rot, signs), transformed))
                            |> Seq.tryFind (fun (_, transformed) -> 
                                transformed = translated1)

                if found.IsNone then None
                else
                    let ((rot, signs), transformed) = found.Value
                    let transformFunction2to1 (pt: Vector3D) = (transformer rot signs (pt.Sub points2[0])).Add points1[0]

                    //let invSigns = signs.Mult (Vector3D.One.MultScalar -1)
                    let invRot = [0;1;2;] |> List.map (fun f-> rot |> List.findIndex (fun ff -> ff = f))
                    //e.g. XYZ -> [2,0,1]-> ZXY -> [1,2,0]

                    let transformerInverse rotation signs (pt: Vector3D) =
                        let tmp = pt.ToList
                        let rotated = rotation |> List.map (fun i -> tmp[i]) |> Vector3D.FromList
                        rotated.Mult signs
                    
                    let transformFunction1to2 (pt: Vector3D) = (transformerInverse invRot signs (pt.Sub points1[0])).Add points2[0]

                    if (points1 |> List.map transformFunction1to2) <> points2 then
                        let org1 = pointsToString points1
                        let org2 = pointsToString points2
                        let x2 = pointsToString (points2 |> List.map transformFunction2to1)
                        let x1 = pointsToString (points1 |> List.map transformFunction1to2)
                        printfn "sad"
                    Some((matchedIndices.Value, (transformFunction1to2, transformFunction2to1)))
        else None

    //// Possible optimization:
    //let numScanners = scanners.Keys.Count
    //let allOverlaps = [0..numScanners-2] |> List.map (fun id1 ->
    //    let id1Dists = scanners[id1].AllDistances
    //    let overlaps = [(id1+1)..numScanners-1] |> List.map (fun id2 -> 
    //        let id2Dists = scanners[id2].AllDistances
    //        let overlap = intersect id1Dists id2Dists
    //        (id2, overlap.Length)
    //    )
    //    (id1, overlaps |> List.sortBy (fun f -> snd f) |> List.rev)
    //)
    //let orderedByBestOverlap = allOverlaps |> List.sortBy (fun (id, overlaps) -> snd (overlaps |> List.head)) |> List.rev
    //let mppp = orderedByBestOverlap |> List.map (fun (id1, overlaps) -> 
    //    let found = overlaps |> List.tryFind (fun (id2, _) -> 
    //        let xxx = align id1 id2 12
    //        xxx.IsSome
    //    )
    //    (id1, found)
    //)

    let scannerCombos = scanners.Keys |> Seq.toList |> combinations 2
    let matchedScannerPoints = scannerCombos
                                |> List.map (fun ls -> 
                                    let id1 = ls[0]
                                    let id2 = ls[1]
                                    let aligned = align id1 id2 12
                                    //printfn "%A %O" ls aligned.IsSome
                                    if aligned.IsSome then
                                        let ((indices1, indices2), (transformFunction1to2, transformFunction2to1)) = aligned.Value
                                        Some((id1, indices1, transformFunction1to2), (id2, indices2, transformFunction2to1))
                                    else
                                        None
                                ) |> List.filter(fun f -> f.IsSome) |> List.map(fun f -> f.Value)

    let findConnections id = 
        matchedScannerPoints |> List.filter (fun ((id1, _, _), (id2, _, _)) -> 
            id1 = id || id2 = id) |> List.map (fun ((id1, indices1, x1to2), (id2, indices2, x2to1)) -> if id1 = id then (id2, indices2, x2to1) else (id1, indices1, x1to2))
    
    let rec loop id visited xforms =
        seq {
            let connections = (findConnections id) |> List.filter (fun (idSub, _, _) -> (visited |> List.contains idSub) = false)
            let newVisited = [id] |> List.append visited
            for conn in connections do
                let (idSub, indices, xform) = conn
                let points = indices |> List.map (fun i -> scanners[idSub].Points[i])
                let newXforms = [xform] |> List.append xforms
                let xformed = newXforms |> List.fold(fun agg curr -> agg |> List.map curr ) points
                yield xformed
                yield! loop idSub newVisited newXforms
        }

    let allPoints = (loop 0 [] []) |> Seq.toList |> List.reduce List.append |> List.distinct |> List.sortBy (fun pt -> pt.X)
    let sss = pointsToString allPoints
    //let reduced = matchedScannerPoints 
    //                //|> List.map (fun ((scannerId1, scannerId2), (pointIds1, pointIds2)) ->
    //                |> List.map (fun ((scannerId1, pointIds1, x1to2), (scannerId2, pointIds2, x2to1)) ->
    //                    pointIds1 |> List.map (fun p -> (scannerId1, p)) |> List.append (pointIds2 |> List.map (fun p -> (scannerId2, p)))
    //                ) |> List.reduce List.append |> List.distinct |> List.sortBy (fun (scannerId, _) -> scannerId)

    allPoints.Length