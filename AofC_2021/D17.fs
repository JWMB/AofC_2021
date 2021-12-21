module D17

open System.Text.RegularExpressions
open System
open Tools.GridTools

let parseInput (input: string) = 
    let ms = Regex.Matches(input, @"(?<axis>(x|y))=(?<min>-?\d+)\.\.(?<max>-?\d+)")
    if ms.Count <> 2 then raise (new System.Exception("Input corrupt"))
    let grpToInt (m: Match) (grp: string) = Convert.ToInt32(m.Groups[grp].Value)
    let tuples = ms |> Seq.cast<Match> |> Seq.map (fun m -> (m.Groups["axis"].Value, [|grpToInt m "min"; grpToInt m "max"; |])) |> Seq.toArray
    let byAxis = Map(tuples)
    Grid2D.FromCoordinates([| 
        { X =  byAxis["x"] |> Array.min; Y = byAxis["y"] |> Array.min; };
        { X = byAxis["x"] |> Array.max; Y = byAxis["y"] |> Array.max; };
    |])


let part2 (input: string) = 
    let target = parseInput input

    let velocityAfterStep initial step =
        let x = [| initial.X - step; 0 |] |> Array.max
        let y = initial.Y - step
        { X = x; Y = y; }
    
    let simulate pos vel =
        let rec loop (p: Vector2D) v =
            let newP = p.Add(v)
            if newP.X > target.BottomRight.X || newP.Y < target.TopLeft.Y then None //TopLeft.Y - Y is inverted
            else if target.IsValid newP then Some(newP)
            else
                let newV = velocityAfterStep v 1
                loop newP newV
        loop pos vel

    let getTriangularNumber step = ((step+1) * step) / 2
    //TODO: equation instead of brute-force?
    //let maxXToReach = [|1..target.BottomRight.X|] |> Array.filter(fun f ->
    //    let tri = getTriangularNumber f
    //    if tri < target.BottomRight.X && tri >= target.TopLeft.X then true else false)

    let possibleXs = [|1..target.BottomRight.X|] |> Array.filter(fun velX ->
        let isInside x = x >= target.Left && x <= target.Right
        let endpoints = [|0..velX-1|] |> Array.map (fun numSteps -> 
            let tri1 = getTriangularNumber velX
            let tri2 = getTriangularNumber (velX - numSteps)
            tri1 - tri2)
        let valid = endpoints |> Array.filter(fun v -> isInside v)
        valid.Length > 0
    )

    let combos = possibleXs |> Array.map (fun velX ->
        let velYsAndHits = [|-1000..1000|] |> Array.map (fun velY -> 
            let r = simulate Vector2D.Zero { X = velX; Y = velY }
            (velY, r)
        )
        let found = velYsAndHits |> Array.filter(fun f -> (snd f).IsSome) |> Array.map(fun f -> fst f)
        found |> Array.map (fun velY -> { X = velX; Y = velY; })
    )                               
    let flattened = combos |> Array.reduce Array.append
    flattened.Length
    //let s = getTriangularNumber sss