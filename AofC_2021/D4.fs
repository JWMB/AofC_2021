module D4

open System.Text.RegularExpressions

let parseInput (input: string) =
    let xx = System.Text.RegularExpressions.Regex.Split(input.Trim(), @"(\r?\n\s*){2}") |> Array.map (fun r -> r.Trim()) |> Array.filter (fun r -> r.Length > 0)
    let draws = (xx.[0]).Split(',') |> Array.map (fun s -> s.Trim()) //int
    let boards = xx.[1..] |> Array.map (fun b -> b.Split('\n') |> Array.map (fun r -> " " + r.Trim() + " ") |> String.concat "\n")
    (draws, boards)

let markedBoard (board: string) item = board.Replace(" " + item + " ", " * ")

let toArray (b: string) =
    b.Split('\n') |> Array.map (fun r ->
        r.Split(' ') |> Array.filter (fun item -> item.Trim().Length > 0))

let rotateArray (arr: string [] []) =
    [|0 .. (arr.[0]).Length - 1|] |> Array.map (fun xIndex ->
        [|0 .. (arr.Length - 1)|] |> Array.map (fun yIndex -> arr.[yIndex].[xIndex]))

let toString (arr: string [] []) =
    arr |> Array.map (fun r -> 
        r |> Array.map (fun c -> " " + c + " ") |> String.concat "")
            |> String.concat "\n"

let hasBoardWon (board: string) =
    let fullRows (b: string) =
        b.Split('\n') |> Array.filter (fun r -> Regex.Match(r, @"\d").Success = false)
    if (fullRows board).Length > 0 then true else 
        if (board |> toArray |> rotateArray |> toString |> fullRows).Length > 0 then true else false

let getSumOfLeftovers board =
    let flat = board |> toArray |> Array.reduce Array.append
    flat |> Array.filter (fun x -> x <> "*") |> Array.map int |> Array.sum

let part1 input =
    let (draws, boards) = parseInput input

    let findWinningBoard boards = boards |> Array.tryFind hasBoardWon
    let (firstWinner, draw) = draws |> Array.fold (fun agg curr -> 
                                    let winning = findWinningBoard (fst agg)
                                    if winning = None then
                                        ((fst agg) |> Array.map (fun b -> markedBoard b curr), curr)
                                    else
                                        ([|winning.Value|], snd agg)
                                ) (boards, "")
    (getSumOfLeftovers firstWinner.[0]) * (int draw)

let part2 input =
    let (draws, boards) = parseInput input
    
    let (notWon, won, draw) = draws |> Array.fold (fun agg curr -> 
                                    let (notWon, won, preDraw) = agg
                                    let mods = notWon |> Array.map (fun b -> markedBoard b curr)
                                    let newWons = mods |> Array.filter hasBoardWon
                                    if newWons.Length = 0 then (mods, won, preDraw)
                                    else (mods |> Array.except newWons, won |> Array.append newWons, curr)
                                ) (boards, Array.zeroCreate<string> 0, "!")

    if won.Length = 0 then 0
    else
        let lastWinner = won.[0] // reversed order
        let finalDraw = int draw
        let sumOfLeftovers = getSumOfLeftovers lastWinner
        sumOfLeftovers * finalDraw
