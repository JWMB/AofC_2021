module D5

open System.Text.RegularExpressions

type Point = { X: int; Y: int; }
type Line = { Start: Point; End: Point; }

type BoardCoordinates = { Size: Point; TopLeft: Point; }

let getBoardIndex (pt:Point) (boardCoordinates:BoardCoordinates) =
    pt.X - boardCoordinates.TopLeft.X + (pt.Y - boardCoordinates.TopLeft.Y) * boardCoordinates.Size.X

let addAtBoard board (pt:Point) (boardCoordinates:BoardCoordinates) =
    let index = getBoardIndex pt boardCoordinates
    board |> List.mapi (fun i el -> if i = index then el + 1 else el)

let addAtBoardMutable board (pt:Point) (boardCoordinates:BoardCoordinates) =
    let index = getBoardIndex pt boardCoordinates
    Array.set board index (board.[index] + 1)

let getPoints (line: Line) =
    let diff = { X = line.End.X - line.Start.X; Y = line.End.Y - line.Start.Y }
    let numSteps = [|abs diff.X; abs diff.Y|] |> Array.max
    let delta = {| X = (float diff.X) / (float numSteps); Y = (float diff.Y) / (float numSteps) |}
    [|0 .. numSteps|] |> Array.map (fun step -> { X = line.Start.X + (int ((float step) * delta.X)); Y = line.Start.Y + (int ((float step) * delta.Y)); }) 

let drawLine board boardCoordinates line =
    getPoints line |> Array.fold (fun agg curr -> addAtBoard agg curr boardCoordinates) board

let drawLineMutable board boardCoordinates line =
    let points = getPoints line 
    for point in points do
        addAtBoardMutable board point boardCoordinates
    
let boardToString (board: List<int>) width =
    let chunks = board |> Seq.chunkBySize width
    chunks |> Seq.map (fun sublist -> sublist |> Array.map (fun el -> el.ToString()) |> String.concat ", ") |> String.concat "\n"

let createBoardList size =
     [0 .. (size.X * size.Y)] |> List.map (fun el -> 0)

let main =

    let exampleData = "
    0,9 -> 5,9
    8,0 -> 0,8
    9,4 -> 3,4
    2,2 -> 2,1
    7,0 -> 7,4
    6,4 -> 2,0
    0,9 -> 2,9
    3,4 -> 1,4
    0,0 -> 8,8
    5,5 -> 8,2
    "

    let data = System.IO.File.ReadAllText("D5.txt")

    let split = Regex.Split(data.Trim(), @"(\r?\n\s*){1}")
    let rows = split |> Array.map (fun r -> r.Trim()) |> Array.filter (fun r -> r.Length > 0)
    let items = rows |> Array.map (fun r -> Regex.Split(r, @" -> ") |> Array.map (fun item -> 
        let ints = item.Split(',') |> Array.map int
        { X = ints.[0]; Y = ints.[1]  }
    ))
    let lines = items |> Array.map (fun el -> { Start = el.[0]; End = el.[1]; })
                // Part 1: |> Array.filter (fun el -> el.Start.X = el.End.X || el.Start.Y = el.End.Y)

    let allX = items |> Array.map (fun r -> r |> Array.map (fun x -> x.X)) |> Array.reduce Array.append
    let allY = items |> Array.map (fun r -> r |> Array.map (fun x -> x.X)) |> Array.reduce Array.append

    let boardRect = {| left = allX |> Array.min; top = allY |> Array.min; right = allX |> Array.max; bottom = allY |> Array.max |}
    let size =  { X = boardRect.right - boardRect.left + 1; Y = boardRect.bottom - boardRect.top + 1; }
    let topleft = { X = boardRect.left; Y = boardRect.top; }
    let boardCoords = { Size = size; TopLeft = topleft; }

    let boardList = createBoardList boardCoords.Size
    let boardArray = boardList |> List.toArray // Aha, Arrays are mutable - we need that now since all those folds on a 1000*1000 List will take a *lot* of time
    //let board2 = lines |> Array.fold (fun agg curr -> drawLine agg boardCoords curr ) board
    for line in lines do
        drawLineMutable boardArray boardCoords line

    //printf "%O\n" (boardToString board2 size.X)
    boardArray |> Array.filter (fun el -> el > 1) |> Array.length
