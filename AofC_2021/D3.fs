module D3

open System.Text.RegularExpressions

let parseInput (input: string) = Regex.Split(input.Trim(), @"\n") |> Array.map (fun f -> f.Trim())

let toDecimal str = str |> Seq.toArray |> Array.rev |> Array.mapi (fun i f -> if f = '1' then int (2.0 ** (float i)) else 0) |> Array.sum
let goForthAndMultiply funcs calculator = funcs |> Array.map calculator |> Array.map toDecimal |> Array.reduce (fun agg c -> agg * c)


let part1 input =
    let data = parseInput input
    let numItems = data.Length
    let indexArray =  [| 0 .. data.[0].Length - 1|]

    let num1PerColumn = indexArray |> Array.map (fun col -> data |> Array.filter (fun cell -> cell.Chars(col) = '1') |> Array.length)

    let calc func =indexArray |> Array.map (fun columnIndex ->
        func (int (num1PerColumn.[columnIndex]))) |> Array.map (fun f -> if f then "1" else "0") |> String.concat ""

    let fGamma value = value > (numItems / 2)
    let fEpsilon value = value < (numItems / 2)
    goForthAndMultiply [| fGamma; fEpsilon;|] calc

let part2 input =
    let data = parseInput input
    let indexArray =  [| 0 .. data.[0].Length - 1|]

    let sieve bitCriterium index (rows: string[]) =
        let num1s = rows |> Array.filter (fun cell -> cell.Chars(index) = '1') |> Array.length
        let filterValue = if (bitCriterium num1s rows.Length) then '1' else '0'
        rows |> Array.filter (fun row -> row.[index] = filterValue)

    let lifeSupportCalc bitCriterium = 
        indexArray |> Array.fold (fun (agg: string []) curr -> 
            if agg.Length = 1 then agg else sieve bitCriterium curr agg ) data
            |> String.concat ""

    let oxy = (fun num1s total -> num1s * 2 >= total)
    let co2 = (fun num1s total -> (num1s * 2 >= total) = false)

    goForthAndMultiply [| oxy; co2;|] lifeSupportCalc
