module Common

type Point = { X: int; Y: int; }

let get4Neighbors pt size = 
    let add pt1 pt2 = { X = pt1.X + pt2.X; Y = pt1.Y + pt2.Y; }
    let isValid pt = pt.X >= 0 && pt.X < size.X && pt.Y >= 0 && pt.Y < size.Y

    let offsets = [| 1; -1; |] |> Array.map (fun f -> [| { X = f * 1; Y = 0;}; { X = 0; Y = f * 1;}|]) |> Array.reduce Array.append
    offsets |> Array.map (fun f -> add pt f) |> Array.filter isValid

let get8Neighbors pt size = 
    let range = [|-1 .. 1 |]
    let add pt1 pt2 = { X = pt1.X + pt2.X; Y = pt1.Y + pt2.Y; }
    let isValid pt = pt.X >= 0 && pt.X < size.X && pt.Y >= 0 && pt.Y < size.Y

    let offsets = range |> Array.map (fun x -> range |> Array.map (fun y -> { X = x; Y = y; } ))
                     |> Array.reduce Array.append |> Array.except [| { X = 0; Y = 0; }; |]
    offsets |> Array.map (fun f -> add pt f) |> Array.filter isValid


let pointToIndex pt rowWidth = pt.Y * rowWidth + pt.X
let indexToPoint index rowWidth = { X = index % rowWidth; Y = index / rowWidth }
let getAtPoint pt rowWidth (array: 'a[]) = array.[pointToIndex pt rowWidth]