namespace Tools

module Tools3D = 
    type Vector3D = { X: int; Y: int; Z: int; } with
        member this.Add pt = { X = this.X + pt.X; Y = this.Y + pt.Y; Z = this.Z + pt.Z; }
        member this.Sub pt = { X = this.X - pt.X; Y = this.Y - pt.Y; Z = this.Z - pt.Z; }
        member this.Mult pt = { X = this.X * pt.X; Y = this.Y * pt.Y; Z = this.Z * pt.Z; }
        member this.MultScalar s = { X = this.X * s; Y = this.Y * s; Z = this.Z * s; }
        member this.Div pt = { X = this.X / pt.X; Y = this.Y / pt.Y; Z = this.Z / pt.Z; }
        member this.LengthSquare = this.X * this.X + this.Y * this.Y + this.Z * this.Z
        member this.Length = System.Math.Sqrt(float this.LengthSquare)
        member this.ToList = [this.X;this.Y;this.Z;]
        member this.AxisProduct = (int64 this.X) * (int64 this.Y) * (int64 this.Z);
        static member Zero = { X = 0; Y = 0; Z = 0; } 
        static member One = { X = 1; Y = 1; Z = 1; } 
        static member FromList (lst: int list) = { X = lst[0]; Y = lst[1]; Z = lst[2]; } //Hm, not "safe" (we don't know at compile time if it's len >= 3)

    type Grid3D = { Size: Vector3D; TopLeft: Vector3D; } with
        member this.Left = this.TopLeft.X
        member this.Right = this.BottomRight.X
        member this.Top = this.TopLeft.Y
        member this.Bottom = this.BottomRight.Y
        member this.BottomRight = this.TopLeft.Add({ X = this.Size.X-1; Y = this.Size.Y-1; Z = this.Size.Z-1;})
        member this.IsValid pt = pt.X >= this.TopLeft.X && pt.Y >= this.TopLeft.Y && pt.X <= this.BottomRight.X && pt.Y <= this.BottomRight.Y
        member this.Volume = this.Size.AxisProduct
        member this.Intersect g2 =
                let tl = { 
                    X = [this.TopLeft.X; g2.TopLeft.X] |> List.max;
                    Y = [this.TopLeft.Y; g2.TopLeft.Y] |> List.max;
                    Z = [this.TopLeft.Z; g2.TopLeft.Z] |> List.max;
                    }
                let br = {
                    X = [this.BottomRight.X; g2.BottomRight.X] |> List.min;
                    Y = [this.BottomRight.Y; g2.BottomRight.Y] |> List.min;
                    Z = [this.BottomRight.Z; g2.BottomRight.Z] |> List.min;
                }
                if (tl.X > br.X || tl.Y > br.Y || tl.Z > br.Z) then
                    { TopLeft = tl; Size = Vector3D.Zero }
                else
                    Grid3D.FromCoordinates [| tl;br; |]

        member this.Subtract g2 =
            let inter = this.Intersect g2
            if inter.Size.AxisProduct <= 0 then
                [this]
            else if inter.Size = this.Size then
                []
            else
                //let thisSizeList = this.Size.ToList
                //let sameSizes = inter.Size.ToList |> List.mapi (fun i v -> (i, v = thisSizeList[i])) |> List.filter(fun (_, b) -> b) |> List.map (fun (i, _) -> i)
                //if sameSizes.Length = 2 then
                //    if this.Size.Z <> inter.Size.Z then
                //        [{ Size = { this.Size with Z = this.Size.Z - inter.Size.Z }; TopLeft = { this.TopLeft with Z = [this.TopLeft.Z;inter.TopLeft.Z] |> List.min }}]
                //    else if this.Size.Y <> inter.Size.Y then
                //        [{ Size = { this.Size with Y = this.Size.Y - inter.Size.Y }; TopLeft = { this.TopLeft with Y = [this.TopLeft.Y;inter.TopLeft.Y] |> List.min }}]
                //    else if this.Size.X <> inter.Size.X then
                //        [{ Size = { this.Size with X = this.Size.X - inter.Size.X }; TopLeft = { this.TopLeft with X = [this.TopLeft.X;inter.TopLeft.X] |> List.min }}]
                //    else
                //        []
                //else if sameSizes.Length = 1 then
                //    if this.Size.X = inter.Size.X then
                //        [{ Size = { inter.Size with X = this.Size.X }; TopLeft = { this.TopLeft with Y = [this.TopLeft.Y;inter.TopLeft.Y] |> List.min; Z = [this.TopLeft.Z;inter.TopLeft.Z] |> List.min }}]
                //    else if this.Size.Y = inter.Size.Y then
                //        [{ Size = { inter.Size with Y = this.Size.Y }; TopLeft = { this.TopLeft with X = [this.TopLeft.X;inter.TopLeft.X] |> List.min; Z = [this.TopLeft.Z;inter.TopLeft.Z] |> List.min }}]
                //    else if this.Size.Z = inter.Size.Z then
                //        [{ Size = { inter.Size with Z = this.Size.Z }; TopLeft = { this.TopLeft with X = [this.TopLeft.X;inter.TopLeft.X] |> List.min; Y = [this.TopLeft.Y;inter.TopLeft.Y] |> List.min }}]
                //    else
                //        []
                //else
                    let sections =
                        [
                             // Top shelf
                            { Size = { this.Size with Y = inter.TopLeft.Y - this.TopLeft.Y; }; TopLeft = this.TopLeft };
                             // Bottom shelf
                            { Size = { this.Size with Y = this.BottomRight.Y - inter.BottomRight.Y; }; TopLeft = { this.TopLeft with Y = inter.BottomRight.Y + 1 } };
                            // Left-side chunk
                            { Size = { this.Size with X = inter.TopLeft.X - this.TopLeft.X; Y = inter.Size.Y; }; TopLeft = { this.TopLeft with Y = inter.TopLeft.Y; }};
                            // Right-side chunk
                            { Size = { this.Size with X = this.BottomRight.X - inter.BottomRight.X; Y = inter.Size.Y; }; TopLeft = { this.TopLeft with Y = inter.TopLeft.Y; X = inter.BottomRight.X + 1; }};//{ inter.TopLeft with X = inter.BottomRight.X + 1; }};
                            // Back-side chunk
                            { Size = { inter.Size with Z = inter.TopLeft.Z - this.TopLeft.Z; }; TopLeft = { inter.TopLeft with Z = this.TopLeft.Z; }};
                            // Front-side chunk
                            { Size = { inter.Size with Z = this.BottomRight.Z - inter.BottomRight.Z; }; TopLeft = { inter.TopLeft with Z = inter.BottomRight.Z + 1; } };
                        ]
                        // TODO: consolidate - what if we only really have e.g. back side? With above code we'll still have 3 resulting cubes instead of just 1
                    sections |> List.filter (fun g -> g.Size.AxisProduct > 0)

        //member this.IndexToPoint index = { X = index % this.Size.X; Y = index / this.Size.X }
        //member this.PointToIndex pt = pt.Y * this.Size.X + pt.X
        static member FromCoordinates pts = 
            let xs = pts |> Seq.map (fun f -> f.X)
            let ys = pts |> Seq.map (fun f -> f.Y)
            let zs = pts |> Seq.map (fun f -> f.Z)

            let minmax = [| Seq.min; Seq.max |] |> Seq.map (fun f -> { X = xs |> f; Y = ys |> f; Z = zs |> f; }) |> Seq.toArray
            let topleft = { X = minmax.[0].X; Y = minmax.[0].Y; Z = minmax.[0].Z }
            let size = { X = minmax.[1].X - topleft.X + 1; Y = minmax.[1].Y - topleft.Y + 1; Z = minmax.[1].Z - topleft.Z + 1 } //inclusive
            { Size = size; TopLeft = topleft; }

        static member GetBounding cubes =
            let lists = cubes |> List.map (fun c -> [c.TopLeft.ToList; c.BottomRight.ToList];) |> List.reduce List.append
            let valsByIndex = [0..2] |> List.map (fun index ->
                lists |> List.map(fun l -> l[index])
            )
            let mins = valsByIndex |> List.map (fun l -> l |> List.min)
            let maxs = valsByIndex |> List.map (fun l -> l |> List.max)
            Grid3D.FromCoordinates [| Vector3D.FromList mins; Vector3D.FromList maxs;|]


module GridTools =

    type Vector2D = { X: int; Y: int; } with
        member this.Add pt = { X = this.X + pt.X; Y = this.Y + pt.Y; }
        static member Zero = { X = 0; Y = 0 } 


    type Grid2D = { Size: Vector2D; TopLeft: Vector2D; } with
        member this.Left = this.TopLeft.X
        member this.Right = this.BottomRight.X
        member this.Top = this.TopLeft.Y
        member this.Bottom = this.BottomRight.Y
        member this.BottomRight = this.TopLeft.Add({ X = this.Size.X-1; Y = this.Size.Y-1;})
        member this.IsValid pt = pt.X >= this.TopLeft.X && pt.Y >= this.TopLeft.Y && pt.X <= this.BottomRight.X && pt.Y <= this.BottomRight.Y
        member this.IndexToPoint index = { X = index % this.Size.X; Y = index / this.Size.X }
        member this.PointToIndex pt = pt.Y * this.Size.X + pt.X
        static member FromCoordinates pts = 
            let xs = pts |> Array.map (fun f -> f.X)
            let ys = pts |> Array.map (fun f -> f.Y)
            let minmax = [| Array.min; Array.max |] |> Array.map (fun f -> { X = xs |> f; Y = ys |> f; })
            let topleft = { X = minmax.[0].X; Y = minmax.[0].Y; }
            let size = { X = minmax.[1].X - topleft.X + 1; Y = minmax.[1].Y - topleft.Y + 1; } //inclusive
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

