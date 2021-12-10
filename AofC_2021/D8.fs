module D8

open System.Text.RegularExpressions

// TODO: how to use modules?
let mapToString arr func = arr |> Array.map func |> Array.map string |> String.concat ""
let removeCharsFromString (str: string) (chars: string) = Regex.Replace(str, chars |> Seq.toList |> List.map string |> String.concat "|", "")

let allCombinations lst =
    let rec comb accLst elemLst =
        match elemLst with
        | h::t ->
            let next = [h]::List.map (fun el -> h::el) accLst @ accLst
            comb next t
        | _ -> accLst
    comb [] lst

let getCombinationsString (str: string) = allCombinations (str |> Seq.toList) |> List.map (fun f -> f |> List.map string |> String.concat "")

type DigSeg = { Digit: int; Segments: string }

let getDigitsWithSegments (definition: string) =
    let limitLastIndex (str: string) index = [| index; str.Length - 1 |] |> Array.min
    let getColumns (rows: string) startIndex endIndex =
        rows.Split('\n') |> Array.map (fun f -> f.Substring(startIndex, ((limitLastIndex f endIndex) - startIndex))) |> String.concat "\n"
    let getColumnIndices str =
        Regex.Matches(str, @"(\d):") |> Seq.cast<Match> 
        |> Seq.map (fun f -> {| Index = f.Index; Digit = int (f.Groups.[1].Value); |}) |> Seq.toArray

    let numberDefs = Regex.Split(definition.Trim(), @"(\r?\n\s*){2}") |> Array.map (fun f -> f.Trim()) |> Array.filter (fun f -> f.Length > 0)

    let digitChunks = numberDefs |> Array.map (fun section -> 
            let rows = section.Split('\n')
            let header = rows |> Array.head
            let nonHeaderRowLength = (rows |> Array.tail |> Array.head).Length
            let indices = [| {| Index = nonHeaderRowLength; Digit = -1 |} |] |> Array.append (getColumnIndices header)
            let ranges = indices |> Array.windowed 2 |> Array.map (fun f -> {| Start = f.[0].Index; End = f.[1].Index; Digit = f.[0].Digit |})
                        |> Array.filter (fun f -> f.Digit >= 0)
            let withoutHeader = rows |> Array.tail |> String.concat "\n"
            ranges |> Array.map(fun f -> {| Digit = f.Digit; Chunk = getColumns withoutHeader f.Start f.End; |})
            )
    let flattened = digitChunks |> Array.reduce Array.append // TODO: why can't I put this at the end of statement above?
    let digitsWithSegments = flattened |> Array.map (fun f -> 
        { Digit = f.Digit;
        Segments = Regex.Matches(f.Chunk, @"[a-g]") |> Seq.cast<Match> |> Seq.map (fun m -> m.Value) |> Seq.distinct |> Seq.sort |> String.concat ""
        })
    digitsWithSegments

let main =
    let input = "
    be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb |
    fdgacbe cefdb cefbgd gcbe
    edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec |
    fcgedb cgb dgebacf gc
    fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef |
    cg cg fdcagb cbg
    fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega |
    efabcd cedba gadfec cb
    aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga |
    gecf egdcabf bgf bfgea
    fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf |
    gebdcfa ecba ca fadegcb
    dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf |
    cefg dcbef fcge gbcadfe
    bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd |
    ed bcgafe cdgba cbgef
    egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg |
    gbdfcae bgc cg cgb
    gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc |
    fgae cfgab fg bagce
    "

    let segmentsInput = "
0:      1:      2:      3:      4:
 aaaa    ....    aaaa    aaaa    ....
b    c  .    c  .    c  .    c  b    c
b    c  .    c  .    c  .    c  b    c
 ....    ....    dddd    dddd    dddd
e    f  .    f  e    .  .    f  .    f
e    f  .    f  e    .  .    f  .    f
 gggg    ....    gggg    gggg    ....

5:      6:      7:      8:      9:
 aaaa    aaaa    aaaa    aaaa    aaaa
b    .  b    .  .    c  b    c  b    c
b    .  b    .  .    c  b    c  b    c
 dddd    dddd    ....    dddd    dddd
.    f  e    f  .    f  e    f  .    f
.    f  e    f  .    f  e    f  .    f
 gggg    gggg    ....    gggg    gggg"

    let digitsWithSegments = getDigitsWithSegments segmentsInput

    let groupBySegment arrWithSegments  =
        let all = arrWithSegments |> Array.map (fun f -> f.Segments) |> String.concat "" |> Seq.toArray |> Array.distinct
        all |> Array.map (fun char -> 
            let containedIn = arrWithSegments |> Array.filter (fun ds -> (ds.Segments).Contains(char)) |> Array.map (fun f -> f.Digit)
            {| Segment = char; ContainedIn = containedIn |}
        )

    let input = System.IO.File.ReadAllText("D8.txt")

    let signalToDigSegs signal =
        signal |> Array.map (fun f -> f |> Seq.toArray |> Array.sort |> Array.map string |> String.concat "") |> Array.distinct |> Array.mapi (fun i f -> { Digit = -1 * i - 1; Segments = f; })

    let data = Regex.Split(Regex.Replace(input.Trim(), @"\|\s*\n", "|"), @"\n")
                |> Array.map (fun f -> f.Split('|') |> Array.map (fun items -> items.Trim().Split(' ')))
                |> Array.map (fun f -> 
                    {| Signal = f.[0]; 
                    SignalNormalized = signalToDigSegs f.[0];
                    Output = f.[1]; |})
                
    // part 2: Wanted to brute force - no shortcuts, try to identify all segments first, then translate to digits.
    let getUniqueFreqs segFreqs =
        segFreqs |> Array.groupBy (fun (f: {| ContainedIn: 'a[]; Segment: char; |}) ->
            f.ContainedIn |> Array.length) |> Array.filter (fun f -> (snd f).Length = 1) |> Array.map (fun f -> (snd f).[0])

    let removeSegments digSeg segments =
        digSeg |> Array.map (fun f -> { f with Segments = removeCharsFromString f.Segments segments })

    let identifySegments sigsegs =
        let uniqueSegFreqs = getUniqueFreqs (groupBySegment sigsegs) |> Array.sortBy (fun f -> f.ContainedIn.Length)
        let identifiedSeqments = mapToString uniqueSegFreqs (fun f -> f.Segment)

        let withSingleAfterRemoval segmentsToRemove (ignoreSegments: string) =
            let withRemoved = removeSegments sigsegs segmentsToRemove
            withRemoved |> Array.filter (fun f -> f.Segments.Length = 1) 
                    |> Array.filter (fun f -> ignoreSegments.Contains(f.Segments) = false)
                    |> Array.tryExactlyOne

        let identifySegment segmentsToRemove =
            let combos = segmentsToRemove |> getCombinationsString
            let found = combos |> List.map (fun combo -> withSingleAfterRemoval combo segmentsToRemove) |> List.tryFind (fun f -> f.IsSome)
            let foundOption = if found.IsSome && found.Value.IsSome then Some(found.Value.Value) else None
            foundOption

        let foldResult = [|0..7|] |> Array.fold (fun (agg: {| IdentifiedSegments: string; Continue: bool; |}) curr -> 
                                if agg.Continue = false then agg
                                else
                                let found = identifySegment agg.IdentifiedSegments
                                let segments = agg.IdentifiedSegments + if found.IsNone then "" else found.Value.Segments.[0].ToString()
                                {| IdentifiedSegments = segments; Continue = found.IsSome; |}
                            ) {| IdentifiedSegments = identifiedSeqments;  Continue = true |}
        foldResult.IdentifiedSegments

    let realOrder = identifySegments digitsWithSegments |> Seq.toArray
    let segmentsToDigit = Map (Array.zip(digitsWithSegments |> Array.map (fun f -> f.Segments)) (digitsWithSegments |> Array.map (fun f -> f.Digit)))

    let translateInputToReal digSeg output =
        let inputOrder = identifySegments digSeg |> Seq.toArray
        let inputSegment2Real = Map (Array.zip inputOrder realOrder)
        let translatedInputSegments = output |> Array.map Seq.toArray 
                                        |> Array.map (fun f -> f |> Array.map (fun c -> inputSegment2Real.[c]) |> Array.sort |> Array.map string |> String.concat "")
        let digits = translatedInputSegments |> Array.map (fun f -> segmentsToDigit.[f])
        digits |> Array.map string |> String.concat "" |> int

    let translatedOutput = data |> Array.map (fun f -> translateInputToReal f.SignalNormalized f.Output)
    let sum = translatedOutput |> Array.sum
    sum
    
    //// part 1:
    //let numberToSegments = digitsWithSegments |> Array.map (fun f -> (f.Digit, f.Segments)) |> Map.ofArray
    //let byNumSegments = [| for (KeyValue(k, v)) in numberToSegments -> (k, v.Length) |] |> Array.groupBy (fun f -> snd f)
    //let singleInstances = byNumSegments |> Array.filter (fun f -> (snd f).Length = 1)
    //let singleInstanceSegmentCounts = singleInstances |> Array.map (fun f -> fst f)

    //let outputLengths = data |> Array.map (fun f -> f.Output |> Array.map (fun item -> item.Length))
    //                    |> Array.reduce Array.append

    //let counts = outputLengths |> Array.filter (fun f -> singleInstanceSegmentCounts |> Array.contains f ) |> Array.length
    //counts

