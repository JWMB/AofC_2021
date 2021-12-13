module D2

open System.Text.RegularExpressions
open Tools.GridTools

let parseInput (input: string) = Regex.Split(input.Trim(), @"\n") |> Array.map (fun f -> f.Trim().Split(' ')) |> Array.map (fun f -> {| Direction = f.[0]; Amount = int f.[1]|})

let directionToVector direction amount =
    match direction with
    | "forward" -> { X = amount; Y = 0 }
    | "up" -> { X = 0; Y = -1 * amount }
    | "down" -> { X = 0; Y = amount }
    | _ -> raise (System.ArgumentException("Incorrect direction input"))

let part1 input =
    let data = parseInput input
    let final = data |> Array.fold (fun (agg: Vector2D) curr -> 
                    let vector = directionToVector curr.Direction curr.Amount
                    agg.Add vector
                ) Vector2D.Zero
    final.X * final.Y

let part2 input =
    let data = parseInput input
    let final = data |> Array.fold (fun (agg: {| Aim: int; Position: Vector2D |}) curr -> 
                    let vector = directionToVector curr.Direction curr.Amount
                    if vector.X = 0 then {| Aim = agg.Aim + vector.Y; Position = agg.Position |}
                    else {| Aim = agg.Aim; Position = { X = agg.Position.X + vector.X; Y = agg.Position.Y + agg.Aim * vector.X } |}
                ) {| Aim = 0; Position = Vector2D.Zero |}

    final.Position.X * final.Position.Y
