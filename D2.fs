module D2

open System.Text.RegularExpressions

type Point = { X: int; Y: int; }

let main =

    let input = "
    forward 5
    down 5
    forward 8
    up 3
    down 8
    forward 2
    "
    let input = System.IO.File.ReadAllText("D2.txt")

    let directionToVector direction amount =
        match direction with
        | "forward" -> { X = amount; Y = 0 }
        | "up" -> { X = 0; Y = -1 * amount }
        | "down" -> { X = 0; Y = amount }
        | _ -> raise (System.ArgumentException("Incorrect direction input"))

    let data = Regex.Split(input.Trim(), @"\n") |> Array.map (fun f -> f.Trim().Split(' ')) |> Array.map (fun f -> {| Direction = f.[0]; Amount = int f.[1]|})

    // part 1:
    //let final = data |> Array.fold (fun agg curr -> 
    //                let vector = directionToVector curr.Direction curr.Amount
    //                { X = agg.X + vector.X; Y = agg.Y + vector.Y; }
    //            ) { X = 0; Y = 0; }
    // final.X * final.Y

    // part 2:
    let final = data |> Array.fold (fun agg curr -> 
                    let vector = directionToVector curr.Direction curr.Amount
                    if vector.X = 0 then {| Aim = agg.Aim + vector.Y; Position = agg.Position |}
                    else  {| Aim = agg.Aim; Position = { X = agg.Position.X + vector.X; Y = agg.Position.Y + agg.Aim * vector.X } |}
                ) {| Aim = 0; Position = { X = 0; Y = 0; } |}

    final.Position.X * final.Position.Y
