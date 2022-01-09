namespace Tools

module Combinatorics =

    let rec distribute e = function
    | [] -> [[e]]
    | x::xs' as xs -> (e::xs)::[for xs in distribute e xs' -> x::xs]

    let rec permute = function
    | [] -> [[]]
    | e::xs -> List.collect (distribute e) (permute xs)

    let permutations depth aList =
        let rec loop currDepth generated = seq {
            for item in aList do
                let x = [item] |> List.append generated
                if currDepth = depth then yield x
                else yield! loop (currDepth+1) x
        }
        loop 1 [] |> Seq.toList

    let combinations size aList = 
        let rec pairHeadAndTail acc bList = 
            match bList with
            | [] -> acc
            | x::xs -> pairHeadAndTail (List.Cons ((x,xs),acc)) xs
        let remainderAfter = aList |> pairHeadAndTail [] |> Map.ofList
        let rec comboIter n acc = 
            match n with
            | 0 -> acc
            | _ -> 
                acc
                |> List.fold (fun acc alreadyChosenElems ->
                    match alreadyChosenElems with
                    | [] -> aList //Nothing chosen yet, therefore everything remains.
                    | lastChoice::_ -> remainderAfter.[lastChoice]
                    |> List.fold (fun acc elem ->
                        List.Cons (List.Cons (elem,alreadyChosenElems),acc)
                    ) acc
                ) []
                |> comboIter (n-1)
        comboIter size [[]]

    let listCombos (lists: 'a list list) =
        let numLists = lists |> List.length
        let rec loop curr i =
            seq {
                if i = numLists then yield curr
                else
                    for item in lists[i] do
                        yield! loop ([item] |> List.append curr) (i+1)
            }
        loop [] 0
