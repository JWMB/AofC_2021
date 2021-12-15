namespace Tools

module GridTools =
    type Vector2D = { X: int; Y: int; } with
        member this.Add pt = { X = this.X + pt.X; Y = this.Y + pt.Y; }
        static member Zero = { X = 0; Y = 0 } 

    type Grid2D = { Size: Vector2D; TopLeft: Vector2D; } with
        member this.BottomRight = this.TopLeft.Add({ X = this.Size.X-1; Y = this.Size.Y-1;})
        member this.IsValid pt = pt.X >= this.TopLeft.X && pt.Y >= this.TopLeft.Y && pt.X <= this.BottomRight.X && pt.Y <= this.BottomRight.Y
        member this.IndexToPoint index = { X = index % this.Size.X; Y = index / this.Size.X }
        member this.PointToIndex pt = pt.Y * this.Size.X + pt.X
        static member FromCoordinates pts = 
            let xs = pts |> Array.map (fun f -> f.X)
            let ys = pts |> Array.map (fun f -> f.Y)
            let minmax = [| Array.min; Array.max |] |> Array.map (fun f -> { X = xs |> f; Y = ys |> f; })
            let topleft = { X = minmax.[0].X; Y = minmax.[0].Y; }
            let size = { X = minmax.[1].X - topleft.X; Y = minmax.[1].Y - topleft.Y }
            { Size = size; TopLeft = topleft; }
    
    let isValid pt size = pt.X >= 0 && pt.X < size.X && pt.Y >= 0 && pt.Y < size.Y
    let filterValids pts size = pts |> Array.filter (fun pt -> isValid pt size)

    let get4Neighbors (pt: Vector2D) size = 
        let grid = { Size = size; TopLeft = Vector2D.Zero; }
        let offsets = [| 1; -1; |] |> Array.map (fun f -> [| { X = f * 1; Y = 0;}; { X = 0; Y = f * 1;}|]) |> Array.reduce Array.append
        offsets |> Array.map (fun f -> pt.Add f) |> Array.filter (fun f -> grid.IsValid f)
    
    let get8Neighbors (pt: Vector2D) size = 
        let range = [|-1 .. 1 |]
        let grid = { Size = size; TopLeft = Vector2D.Zero; }
        let offsets = range |> Array.map (fun x -> range |> Array.map (fun y -> { X = x; Y = y; } ))
                         |> Array.reduce Array.append |> Array.except [| { X = 0; Y = 0; }; |]
        offsets |> Array.map (fun f -> pt.Add f) |> Array.filter (fun f -> grid.IsValid f)
    
    let pointToIndex pt rowWidth = pt.Y * rowWidth + pt.X
    let indexToPoint index rowWidth = { X = index % rowWidth; Y = index / rowWidth }
    let getAtPoint pt rowWidth (array: 'a[]) = array.[pointToIndex pt rowWidth]

