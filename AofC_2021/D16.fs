module D16

open System.Text.RegularExpressions
open System

type Operator = 
    | BitLength of int
    | PacketCount of int

type PacketHeader = {
    Version: int;
    TypeId: int;
    }

type Packet = {
    Header: PacketHeader;
    Content: PacketContent;
}
and LiteralContent = {
    Value: uint64;
} with
    static member Parse str index = ""

and OperatorContent = {
    Operator: Operator;
    Children: Packet array;
}
and PacketContent = 
    | Value of LiteralContent
    | Op of OperatorContent

let hexToInt (s: string) = Convert.ToInt32(s, 16)
let intToBinString (value: int) padToLength = Convert.ToString(value, 2).PadLeft(padToLength, '0')
let binToInt (s: string) = Convert.ToInt32(s, 2)
let binToUInt64 (s: string) = Convert.ToUInt64(s, 2)

let part1 (input:string) =
    let data = Regex.Split(input.Trim(), @"\r?\n\s*")
                |> Array.map (fun f -> f.Trim())
                |> Array.head
    let ints = data |> Seq.map string |> Seq.map hexToInt |> Seq.toArray
    let bin = ints |> Array.map (fun f -> intToBinString f 4) |> String.concat ""

    let parseLiteralChunk (s: string) i  =
        let chunk = s.Substring(i, 5)
        let value = binToInt (chunk.Substring(1))
        let lastIndex = i + chunk.Length - 1;
        let proceed = chunk.Substring(0, 1) = "1"
        //let padding = if proceed = true || lastIndex % 4 = 0 then 0 else 4 - (lastIndex % 4)
        //let nextIndex = lastIndex + padding + 1
        let nextIndex = lastIndex + 1
        (value, nextIndex, proceed)

    let parseLiteral (s: string) i =
        let folder index values =
            let (value, newIndex, shouldProceed) = parseLiteralChunk s index
            (newIndex, values |> Array.append [|value|], shouldProceed)

        let rec loop index values =
            let (nIndex, nValues, proceed) = folder index values
            if proceed then loop nIndex nValues 
            else (nIndex, nValues)

        let (index, values) = loop i [||]
        let binValue = values |> Array.map (fun f -> intToBinString f 4) |> Array.rev |> String.concat ""
        (index, { Value = binToUInt64 binValue; })

    let parseOperator (s: string) i =
        if s.Substring(i, 1) = "1" then
            (i + 12, PacketCount (binToInt (s.Substring(i + 1, 11))))
        else
            (i + 16, BitLength (binToInt (s.Substring(i + 1, 15))))

    let rec parsePackage (str: string) index =
        let version = binToInt (str.Substring(index, 3))
        let typeId = binToInt (str.Substring(index + 3, 3))
        let contentIndex = index + 6

        let (nextIndex, content) =
            if typeId = 4 then
                let rx = LiteralContent.Parse str contentIndex
                let r = parseLiteral str contentIndex
                (fst r, Value(snd r))
            else
                let (newIndex, result) = parseOperator str contentIndex
                match result with
                | PacketCount pc ->
                    let subpacks = [|1..pc|] |> Array.fold (fun agg _ -> 
                                                let (nI, pk) = parsePackage str (fst agg)
                                                (nI, (snd agg) |> Array.append [| pk |] )
                                            ) (newIndex, Array.zeroCreate<Packet> 0)
                    (fst subpacks, Op { Operator = result; Children = (snd subpacks) |> Array.rev; })
                | BitLength bl -> 
                    let mutable subi = newIndex
                    let subpacks = new System.Collections.Generic.List<Packet>()
                    while subi < newIndex+bl do
                        let r = parsePackage str subi
                        subi <- fst r
                        subpacks.Add(snd r)
                    (subi, Op{ Operator = result; Children = (subpacks |> Seq.toArray); })
                

        (nextIndex, { Header = { Version = version; TypeId = typeId; }; Content = content });

    let (ix, root) = parsePackage bin 0

    let flatten (packet) = 
        let rec loop (p: Packet) = seq {
            yield p
            match p.Content with
            | Value v -> ()
            | Op o ->
                for c in o.Children do
                    yield! loop c
        }

        loop packet |> Seq.toArray

        //let blo = flatten root
        //let versionSum = blo |> Array.sumBy (fun f -> f.Info.Version)
        //versionSum

    let rec reducer node =
        match node.Content with
        | Value v -> v.Value
        | Op o ->
            let childValues = o.Children |> Array.map (fun f -> reducer f)
            match node.Header.TypeId with
            | 0 -> childValues |> Array.sum
            | 1 -> childValues |> Array.reduce (fun p c -> p * c)
            | 2 -> childValues |> Array.min
            | 3 -> childValues |> Array.max
            | 5 -> if childValues.[0] > childValues.[1] then 1UL else 0UL
            | 6 -> if childValues.[0] < childValues.[1]  then 1UL else 0UL
            | 7 -> if childValues.[0] = childValues.[1]  then 1UL else 0UL
            | _ -> raise (new System.Exception("Unknown type"))
            
    let result = reducer root
    result

let part2 input =
    0