module D18

open System.Text.RegularExpressions;

type Side = | Left | Right

type Pair = { Left: Number; Right: Number; }
and Number = 
    | Single of int
    | Pair of Pair
    with
    member this.ToNestedString =
        let seq2Str s = s |> Seq.toArray |> String.concat ""
        let rec loop n = seq {
            match n with
            | Single s -> yield s.ToString()
            | Pair p ->
                let m1 = seq2Str (loop p.Left)
                let m2 = seq2Str (loop p.Right)
                yield $"[{m1},{m2}]"
        }
        seq2Str (loop this)
        

let parseNumber (input: string) =
    let rxDigits = new Regex(@"\d+");

    let rec getNumber index =
        let c = input.Chars index
        if c = '[' then
            let assertNext i c = if input.Chars i <> c then raise (new System.Exception($"Expected '{c}' - got '{input.Chars i}'"))
            let (i1, e1) = getNumber (index + 1)
            assertNext i1 ',' |> ignore
            let (i2, e2) = getNumber (i1 + 1)
            assertNext i2 ']' |> ignore
            let (n: Number) = Pair { Left = e1; Right = e2; }
            (i2 + 1, n)
        else
            let m = rxDigits.Match(input, index)
            if m.Success = false then raise (new System.Exception("Non-digit encountered"))
            let (n: Number) = Single (int m.Value)
            (index + m.Length, n)
    let result = getNumber 0
    snd result

let add (n1: Number) (n2: Number) = 
    let intIfSingle n = match n with | Single s -> Some s | _ -> None
    let s1 = intIfSingle n1
    let s2 = intIfSingle n2
    if s1.IsSome && s2.IsSome then 
        Single (s1.Value + s2.Value)
    else
        let pair = Pair { Left = n1; Right = n2}
        let (result: Number) = pair
        result

let calcMagnitude (number: Number) =
    let rec loop n =
        match n with
        | Single s -> s
        | Pair p ->
            let m1 = loop p.Left
            let m2 = loop p.Right
            3 * m1 + 2 * m2
    loop number

let getRegularPaths (number: Number) =
    let rec loop n path = seq {
        match n with
        | Single s -> yield path
        | Pair p ->
            yield! loop p.Left ([|Left|] |> Array.append path )
            yield! loop p.Right ([|Right|] |> Array.append path )
    }

    (loop number (Array.zeroCreate<Side> 0)) |> Seq.toArray

let getByPath (number: Number) (path: Side array) =
    let rec loop n (subPath: Side array) =
        if subPath.Length = 0 then n
        else
            let head = subPath |> Array.head
            match n with
            | Single s -> raise (new System.Exception("Incorrect pah"))
            | Pair p ->
                loop (if head = Right then p.Right else p.Left) (subPath |> Array.tail)
    loop number path

let getPairByPath (number: Number) (path: Side array) =
    let a = getByPath number path
    match a with
    | Pair p -> p
    | _ -> raise (new System.Exception("Not a pair"))

let replace (number: Number) (substitutes: Map<Side[], Number>) =
    let rec loop n (path: Side array) =
        if substitutes.ContainsKey path then
            substitutes[path]
        else
            match n with
            | Single s -> Single s
            | Pair p ->
                let newPair =
                    Pair
                        {
                            Left = loop p.Left ([|Left|] |> Array.append path);
                            Right = loop p.Right ([|Right|] |> Array.append path);
                        }
                newPair
    loop number (Array.zeroCreate<Side> 0)

let explode (number: Number) =
    let exceptLastN n (arr :'a[]) = arr[0..arr.Length-1-n]
    let exceptLast = exceptLastN 1

    let paths = getRegularPaths number

    let indexAndNumberByParentPath =
        paths |> Array.mapi (fun i p -> ( p |> exceptLast, (i, p)))
                |> Array.groupBy (fun f -> fst f)
                |> Array.map (fun f -> (fst f, (snd f) |> Array.map (fun g -> snd g)))

    let optionX = indexAndNumberByParentPath |> Array.tryFind (fun f -> (fst f).Length >= 4 && (snd f).Length = 2)
    if optionX.IsNone then None
    else
    let (explodePath, item) = optionX.Value

    let pair = getPairByPath number explodePath
    let explodedSideInParent = explodePath |> Array.last

    let modifiedAdjacent = 
        let index = 
            match explodedSideInParent with
            | Left -> -1 + fst item.[0]
            | Right -> 1 + fst item.[1]

        if index < 0 || index > paths.Length - 1 then
            None
        else
            let explodedToAdd = 
                match explodedSideInParent with
                | Left -> pair.Left
                | Right -> pair.Right

            let adjacentParentPath = exceptLast paths[index]
            let adjacentParent = getPairByPath number adjacentParentPath
            let adjacentSide = paths[index] |> Array.last
            match adjacentSide with
            | Left ->
                Some (adjacentParentPath, Pair { Left = add adjacentParent.Left explodedToAdd; Right = adjacentParent.Right; })
            | Right ->
                Some (adjacentParentPath, Pair { Left = adjacentParent.Left; Right = add adjacentParent.Right explodedToAdd; })

    let mod1 = if modifiedAdjacent.IsSome then replace number (Map([|modifiedAdjacent.Value|])) else number

    let parent = getPairByPath mod1 (exceptLast explodePath)
    let newParent =
        match explodedSideInParent with
        | Left -> { Left = Single 0; Right = add pair.Right parent.Right } //[[1,2],3] -> [0,4]
        | Right -> { Left = add parent.Left pair.Left; Right = Single 0; } //[1,[2,3],[4,5]] -> [[3,0],[4,5]]

    Some(
        replace mod1 (Map([|(exceptLast explodePath, Pair newParent)|]))
    )

let findAndSplit (number: Number) =
    let split (n: int) =
        let half = (float n) / 2.0
        Pair { Left = Single (int (System.Math.Floor half)); Right = Single (int (System.Math.Ceiling half)); }

    let rec loop n (path: Side array) =
        match n with
        | Single s -> if s >= 10 then Some((path, s)) else None
        | Pair p ->
            let l = loop p.Left ([|Left|] |> Array.append path);
            if l.IsSome then l
            else 
                loop p.Right ([|Right|] |> Array.append path);

    match (loop number (Array.zeroCreate<Side> 0)) with
    | Some (path, single) -> 
        let newNumber = split single
        let replaced = replace number (Map( [|(path, newNumber)|] ))
        (true, replaced)
    | None -> (false, number)


let reduce (number: Number) =
    let rec loop (n: Number) =
        //printfn "%O" (n.ToNestedString)
        match (explode n) with
        | Some n1 -> loop n1
        | None ->
            let (c2, n2) = findAndSplit n
            if c2 then
                loop n2
            else
                n2
    loop number

let part1 (input: string) =
    let numbers = Regex.Split(input.Trim(), @"\r?\n\s*") |> Array.map (fun r -> r.Trim()) |> Array.map parseNumber

    // part 1:
    let final = numbers |> Array.reduce (fun p c -> 
        let next = reduce (add p c)
        //printfn "%O" (next.ToNestedString)
        next
    )
    let mag = calcMagnitude final

    // part 2:    // [0..numbers.Length-2] |> List.map (fun a -> [(a+1)..numbers.Length-1]
    let combos = [0..numbers.Length-1] |> List.map (fun a -> [0..numbers.Length-1] |> List.map (fun b -> (numbers[a], numbers[b]))) |> List.reduce List.append
    let mags = combos |> List.map (fun f -> 
                                    let added = add (fst f) (snd f)
                                    calcMagnitude (reduce added))
                                    |> List.sort 
    let diff = mags |> List.last
    // 4549 too low
    diff