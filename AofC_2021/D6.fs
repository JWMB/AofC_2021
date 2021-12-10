module D6

open System.Text.RegularExpressions

let parseInput (input: string) = Regex.Split(input.Trim(), @",") |> Array.map int

let part1 input =
    let school = parseInput input
    let finalDay = 80

    let final = [|1 .. finalDay|] |> Array.fold (fun agg curr -> 
                let ready = agg |> Array.filter (fun f -> f = 0);
                let aged = agg |> Array.filter (fun f -> f > 0) |> Array.map (fun f -> f - 1)
                let spawned = ready |> Array.map (fun f -> 8)
                let newSchool = aged |> Array.append spawned |> Array.append (ready |> Array.map(fun f -> 6))
                newSchool
                ) school
    final.Length

let part2 input =
    let data = parseInput input
    let finalDay = 256

    let mutable numPerCounter = Array.zeroCreate 9 |> Array.map (fun f -> 0UL)
    for item in data do
        numPerCounter.[item] <- numPerCounter.[item] + 1UL
    for day in [|1 .. finalDay|] do
        let numMature = numPerCounter.[0]
        for index in [|1..numPerCounter.Length-1|] do
            numPerCounter.[index - 1] <- numPerCounter.[index]
        numPerCounter.[8] <- numMature // new fish
        numPerCounter.[6] <- numPerCounter.[6] + numMature

    numPerCounter |> Array.sum
