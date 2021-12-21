module D20

open System.Text.RegularExpressions
open Tools.GridTools

let parseInput (input:string) =
    let sections = Regex.Split(input.Trim(), @"(\r?\n\s*){2}") |> Array.map(fun f -> f.Trim()) |> Array.filter(fun f -> f.Length > 0)
    let algo = Regex.Replace(sections[0], @"\r?\n", "")
    let image = Regex.Split(sections[1], @"\r?\n") |> Array.map (fun row -> row |> Seq.toArray |> Array.map (fun f -> if f = '#' then 1 else 0))
    (algo, image)

let imgToString img = 
    img |> Array.map (fun row -> row |> Array.map (fun v -> if v = 1 then "#" else ".") |> String.concat "") |> String.concat "\n"

let getPerformPass (algo: string) =
    let window = { Size = { X = 3; Y = 3; }; TopLeft = { X = -1; Y = -1; }}
    let windowOffsets = [window.Top..window.Bottom] |> List.map (fun yOff -> [window.Left..window.Right] |> List.map(fun xOff -> { X = xOff; Y = yOff; })) |> List.reduce List.append
        
    let getAlgoValue windowSum = if algo[windowSum] = '#' then 1 else 0
        
    let getPixel (img: int[][]) pt = img[pt.Y][pt.X]
    
    let getWindowValue (img: int[][]) (pt: Vector2D) (imgGrid: Grid2D) outsideValue =
        let pixels = windowOffsets |> List.map (fun offset -> 
            let ptAdd = pt.Add offset
            if imgGrid.IsValid ptAdd then getPixel img ptAdd else outsideValue
        )
        let asBinString = pixels |> List.map string |> String.concat ""
        let value = System.Convert.ToInt32(asBinString, 2)
        getAlgoValue value //(pixels |> List.sum)
    
    
    let performPass (img: int[][]) outsideValue =
        let grid = { Size = { X = img[0].Length; Y = img.Length;}; TopLeft = Vector2D.Zero }
    
        let tl = grid.TopLeft.Add window.TopLeft
        let br = grid.BottomRight.Add window.BottomRight
    
        let newImg = [tl.Y..br.Y] |> List.map (fun y -> 
                            [tl.X..br.X] |> List.map(fun x ->
                                getWindowValue img {X = x; Y = y;} grid outsideValue
                            ) |> List.toArray
                        ) |> List.toArray
    
        let newOutsideBin = [0..window.Size.Y-1] |> List.map (fun y -> [0..window.Size.X-1] |> List.map(fun x -> outsideValue)) |> List.reduce List.append |> List.map string |> String.concat ""
        let newOutsideVal = System.Convert.ToInt32(newOutsideBin, 2)
        (newImg, getAlgoValue newOutsideVal)

    performPass
    
let performPasses numPasses algo image =
    let performPass = getPerformPass algo
    let (final, _) = [0..numPasses-1] |> List.fold (fun (img, outsideValue) _ -> 
                                        performPass img outsideValue
                                    ) (image, 0)
    final

let part1 input =
    let (algo, image) = parseInput input
    let final = performPasses 2 algo image

    let num1s = final |> Array.map (fun row -> row |> Array.filter (fun v -> v = 1) |> Array.length) |> Array.sum
    num1s

let part2 input =
    let (algo, image) = parseInput input

    let final = performPasses 50 algo image

    let num1s = final |> Array.map (fun row -> row |> Array.filter (fun v -> v = 1) |> Array.length) |> Array.sum
    num1s
