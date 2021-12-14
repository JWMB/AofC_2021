module D14

open System.Text.RegularExpressions

let parseInput (input: string) =
    let sections = Regex.Split(input.Trim(), @"(\r?\n\s*){2}") |> Array.map (fun section -> section.Trim()) |> Array.filter (fun f -> f.Length > 0)
    let template = sections.[0]
    let rules = Map(
                    Regex.Split(sections.[1], @"\r?\n") |> Array.map (fun line -> line.Trim().Split("->") |> Array.map (fun s -> s.Trim())) |> Array.map (fun f -> (f.[0], f.[1]))
                )
    let rulesLookup = rules 
                    |> Map.toArray 
                    |> Array.map (fun rule -> 
                        (fst rule, $"{(fst rule).[0]}{snd rule}{(fst rule).[1]}" |> Seq.toArray |> Array.windowed 2 |> Array.map (fun f2 -> f2 |> Array.map string |> String.concat "")))
    (template, Map(rulesLookup))

let windowedStrings windowSize str =
    str |> Seq.toArray |> Array.windowed windowSize |> Array.map (fun chars -> chars |> Array.map string |> String.concat "")

let part1 input =
    let (template, rules) = parseInput input

    let folder template (rules: Map<string, string[]>) =
        let pairs = windowedStrings 2 (template + " ")
        let expand pair = 
            if rules.ContainsKey pair then $"{rules[pair].[0]}"
            else $"{pair.[0]}"
        pairs |> Array.map (fun f -> expand f) |> String.concat ""

    let folded = [|1..10|] |> Array.fold (fun agg _ -> folder agg rules) template

    let cntByCharSorted = folded |> Seq.toArray |> Array.groupBy (fun f -> f) |> Array.map (fun f -> (fst f, (snd f).Length)) |> Array.sortBy (fun f -> snd f)
    (snd (Array.last cntByCharSorted) - (snd cntByCharSorted.[0]))

let part2 input =
    let (template, rules) = parseInput input

    let initiateFromTemplate str =
        str |> Seq.toArray |> Array.windowed 2 |> Array.map (fun f -> f |> Array.map string |> String.concat "")
            |> Array.filter (fun f-> rules.ContainsKey(f))
            |> Array.map (fun f -> (f, 1UL))
            |> Array.groupBy (fun f -> fst f) |> Array.map (fun f -> (fst f, (snd f) |> Array.sumBy (fun p -> snd p)))

    let folder cntPerRule = 
        cntPerRule |>
            Array.map (fun kv ->
                let xx = fst kv
                let currCnt = snd kv
                if currCnt > 0UL && rules.ContainsKey(xx) then
                    rules[xx] |> Array.map (fun mutation -> (mutation, currCnt))
                else [||]
            ) |> Array.reduce Array.append |> Array.groupBy(fun f-> fst f) |> Array.map (fun f -> (fst f, (snd f) |> Array.sumBy (fun p -> snd p)))

    let folded = [|1..40|] |> Array.fold (fun agg _ -> folder agg) (initiateFromTemplate template)

    let cntByChar = folded 
                        |> Array.map (fun f -> ((fst f).[0], snd f))
                        |> Array.groupBy (fun f -> fst f) |> Array.map (fun f -> (fst f, (snd f) |> Array.sumBy (fun p -> snd p)))

    // last char in template is never first in a pair - add 1 for its count
    let lastTemplateChar = template |> Seq.toArray |> Array.last
    let withExtraFinal = cntByChar |> Array.map (fun f -> (fst f, if (fst f) = lastTemplateChar then 1UL + (snd f) else snd f))
    let sorted = withExtraFinal |> Array.sortBy (fun f -> snd f)
    (snd (Array.last sorted) - (snd sorted.[0]))
