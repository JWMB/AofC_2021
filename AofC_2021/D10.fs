module D10

open System.Text.RegularExpressions

let parseInput (input: string) = Regex.Split(input.Trim(), @"(\r?\n\s*){1}") |> Array.map (fun f -> f.Trim()) |> Array.filter (fun f -> f.Length > 0);


type Kind =
    | Open = 0
    | Close = 1
    | Invalid = 2

let pairs = "[] () <> {}"

let getKind (c: char) = match pairs.IndexOf(c) with
    | val1 when val1 % 3 = 0 -> Kind.Open
    | val1 when val1 % 3 = 1 -> Kind.Close
    | _ -> Kind.Invalid

let getMatchingChar (c: char) = match pairs.IndexOf(c) with
    | val1 when val1 % 3 = 0 -> pairs.[val1 + 1]
    | val1 when val1 % 3 = 1 -> pairs.[val1 - 1]
    | _ -> raise (System.ArgumentException("Invalid char"))

let modifyStack c stack = 
    let kind = getKind c
    if kind = Kind.Invalid then None
    else
        if kind = Kind.Close then
            if stack |> List.head = getMatchingChar c then Some(stack |> List.tail)
            else None
        else Some(stack |> List.append [c])

let findCorrupt line = 
    line |> Seq.toArray |> Array.fold (fun (agg: {| Corrupt: bool; LastIndex: int; Stack: char list |}) curr ->
            if agg.Corrupt then agg
            else
                let newStack = modifyStack curr agg.Stack
                if newStack.IsNone then {| agg with Corrupt = true; |}
                else {| Stack = newStack.Value; LastIndex = agg.LastIndex+1; Corrupt = false; |}
        ) {| Stack = []; LastIndex = 0; Corrupt = false;|}

let part1 (input: string) =
    let data = parseInput input
    let analyzed = data |> Array.map (fun f -> {| Line = f; Analysis = findCorrupt f;|})
    let pointsPt1 = Map([(')', 3); (']', 57); ('}', 1197); ('>', 25137)])
    analyzed
        |> Array.filter (fun f -> f.Analysis.Corrupt)
        |> Array.map (fun f -> f.Line.[f.Analysis.LastIndex])
        |> Array.map (fun f -> pointsPt1.[f])
        |> Array.sum

let part2 (input: string) =
    let data = parseInput input
    let analyzed = data |> Array.map (fun f -> {| Line = f; Analysis = findCorrupt f;|})
    let pointsPt2 = Map([(')', 1); (']', 2); ('}', 3); ('>', 4)])
    let part2 = analyzed
                |> Array.filter (fun f -> f.Analysis.Corrupt = false)
                |> Array.map (fun f -> f.Analysis.Stack |> List.map (fun c -> pointsPt2.[getMatchingChar c]) |> List.map int64 |> List.reduce (fun agg c -> agg * 5L + c))
                |> Array.sort
    part2.[part2.Length / 2]
