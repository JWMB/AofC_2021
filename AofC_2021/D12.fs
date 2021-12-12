module D12

open System.Text.RegularExpressions

let parseInput (input: string) = Regex.Split(input.Trim(), @"(\r?\n\s*){1}") |> Array.map (fun f -> f.Trim()) |> Array.filter (fun f -> f.Length > 0)
                                |> Array.map (fun f -> f.Split('-') |> Array.sort) |> Array.distinct |> Array.sortBy (fun f -> f.[0])

let getNodes pairs = pairs |> Array.reduce Array.append |> Array.distinct

let mapNodeToNeighbors pairs =
    let findOthers item = pairs |> Array.filter (fun f -> f |> Array.contains item) |> Array.map (fun f -> f |> Array.except [|item|]) |> Array.reduce Array.append |> Array.distinct
    Map((getNodes pairs) |> Array.map (fun f -> (f, findOthers f)))

let evaluate pairs fGetMaxVisits =
    let nodeToNeighbors = mapNodeToNeighbors pairs
    let nodes = getNodes pairs
    let maxVisitsPerNode = Map(nodes |> Array.map(fun (f: string) -> (f, fGetMaxVisits f)))

    let traverse startNode endNode =
        let rec loop node path = seq {
            let newPath = path |> Array.append [|node|]

            if path |> Array.length > 1000 then
                raise (System.Exception("In too deep!"))
                yield newPath
            else if node = endNode then
                yield newPath
            else
                let neighbors = nodeToNeighbors.[node]
                let numVisits = Map (path |> Array.filter (fun f -> neighbors |> Array.contains f) |> Array.groupBy (fun f -> f) |> Array.map (fun f -> (fst f, (snd f) |> Array.length)))
                let availableNeighbors = neighbors |> Array.filter (fun f -> 
                    if maxVisitsPerNode.[f] = -1 then true
                    else if numVisits.ContainsKey(f) = false then true
                    else maxVisitsPerNode.[f] > numVisits.[f]
                )
                for neighbor in availableNeighbors do
                    yield! loop neighbor newPath
            }
        loop startNode [||] |> Seq.toArray

    traverse "start" "end" |> Array.distinct

let part1 input =
    let pairs = parseInput input
    let paths = evaluate pairs (fun f ->  if f.ToUpper() = f then -1 else 1)
    paths.Length

let part2 input =
    let pairs = parseInput input
    let allSmall = (getNodes pairs) |> Array.filter (fun f -> f.ToLower() = f) |> Array.except [| "start"; "end" |]
    let allPaths = allSmall |> Array.map (fun small -> 
        evaluate pairs (fun f -> if f.ToUpper() = f then -1 else if f = small then 2 else 1)
    )

    let distinct = allPaths |> Array.reduce Array.append |> Array.distinct
    distinct.Length
