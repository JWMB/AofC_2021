module D6

open System.Text.RegularExpressions

type Fish = { Timer: int; }

let main =
    let input = "
    3,4,3,1,2
    "
    let input = System.IO.File.ReadAllText("D6.txt")
    let finalDay = 256

    let data = Regex.Split(input.Trim(), @",") |> Array.map int

    // Part 1: naïve implementation:
    //let school = data |> Array.map (fun i -> { Timer = i; })
    //let final = [|1 .. finalDay|] |> Array.fold (fun agg curr -> 
    //            let ready = agg |> Array.filter (fun f -> f.Timer = 0);
    //            let aged = agg |> Array.filter (fun f -> f.Timer > 0) |> Array.map (fun f -> { Timer = f.Timer - 1; })
    //            let spawned = ready |> Array.map (fun f -> { Timer = 8; })
    //            let newSchool = aged |> Array.append spawned |> Array.append (ready |> Array.map(fun f -> { Timer = 6; }))
    //            // printf "%A\n" (total |> Array.map (fun f -> f.Timer) |> Array.toList)
    //            newSchool
    //            ) school
    //final.Length

    let mutable numPerCounter = Array.zeroCreate 9 |> Array.map (fun f -> 0UL) // [|0..8|]
    for item in data do
        numPerCounter.[item] <- numPerCounter.[item] + 1UL
    for day in [|1 .. finalDay|] do
        let numMature = numPerCounter.[0]
        for index in [|1..numPerCounter.Length-1|] do
            numPerCounter.[index - 1] <- numPerCounter.[index]
        numPerCounter.[8] <- numMature // new fish
        numPerCounter.[6] <- numPerCounter.[6] + numMature

    numPerCounter |> Array.sum
