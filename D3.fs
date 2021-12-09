module D3

open System.Text.RegularExpressions

let main =
    let input = "
    00100
    11110
    10110
    10111
    10101
    01111
    00111
    11100
    10000
    11001
    00010
    01010
    "

    let data = Regex.Split(input.Trim(), @"\n") |> Array.map (fun f -> f.Trim())

    // part 1:
    let itemLength = data.[0].Length
    let numItems = data.Length

    let gamma = [| 0 .. itemLength - 1|] |> Array.map (fun index -> data |> Array.filter (fun item -> item.Chars(index) = '1') |> Array.length > (numItems / 2))
    //let epsilon = [| 0 .. itemLength - 1|] |> Array.map (fun index -> data |> Array.filter (fun item -> item.Chars(index) = '1') |> Array.length > (numItems / 2))

    //let toDecimal arr =
    //    arr |> Array.reduce (fun agg curr -> ) 
    gamma |> Array.map (fun f -> if f then "1" else "0") |> String.concat ""