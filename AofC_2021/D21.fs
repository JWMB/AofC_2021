module D21

open System.Text.RegularExpressions
open System.Diagnostics

type Player = { Position: int; Score: int }

let parseInput (input: string) =
    Regex.Split(input.Trim(), @"\r?\n") |> Array.map (fun r -> 
        let m = Regex.Match(r.Trim(), @"Player (?<player>\d+) starting position: (?<pos>\d+)")
        ( int m.Groups["player"].Value, { Position = int m.Groups["pos"].Value; Score = 0; })
    )

let update turn0 diceSum (players: Player list) =
    let player = turn0 % 2
    let current = players[player]
    let newPos = (current.Position + diceSum - 1) % 10 + 1
    let newP = { Position = newPos; Score = current.Score + newPos; }
    (newP, players |> List.mapi (fun i f -> if i = player then newP else f))

let part1 input =
    let data = parseInput input
    let players = data |> Array.map(fun (player, pos) -> pos) |> Array.toList

    let numThrowsPerTurn = 3
    let turnThrows turn =
        [1..numThrowsPerTurn] |> List.map(fun p -> (turn * 3 + p - 1) % 100 + 1)

    let rec loop turn0 (players: Player list) =
        let throws = turnThrows turn0
        let (newP, modified) = update turn0 (throws |> List.sum) players
        if newP.Score >= 1000 then (turn0, modified)
        else
            loop (turn0+1) modified

    let (turn0, final) = loop 0 players

    let losingPlayer = final |> List.sortBy(fun p -> p.Score) |> List.head
    let result = (turn0 + 1) * numThrowsPerTurn * losingPlayer.Score
    result

let permutations depth aList =
    let rec loop currDepth generated = seq {
        for item in aList do
            let x = [item] |> List.append generated
            if currDepth = depth then yield x
            else yield! loop (currDepth+1) x
    }
    loop 1 [] |> Seq.toList

let optimizedButBadly players (countsBySum: Map<int, int>) winAt =
    // attempt to optimize - if other player can reach goal in next turn, maybe we can calculate the rest more quickly..?
    // Seems to take twice as long...
    let maxSumPerTurn = countsBySum.Keys |> Seq.max

    let rec loop2 turn numUniverses (players: Player list) countsBySumToUse = seq {
            for (outcome: System.Collections.Generic.KeyValuePair<int,int>) in countsBySumToUse do
                let (newP, modified) = update (turn % 2) outcome.Key players
                if newP.Score >= winAt then
                    let winner = if modified[0].Score > modified[1].Score then 0 else 1
                    yield (winner, numUniverses * (uint64 outcome.Value))
                else
                    let otherIndex = ((turn + 1) % 2)
                    let other = players[otherIndex]
                    let toGo = winAt - other.Score
                    let directCombos = 
                        if toGo <= maxSumPerTurn then
                            let directHits = countsBySum 
                                                |> Seq.map (fun kv -> (kv, (other.Position + kv.Key - 1) % 10 + 1))
                                                |> Seq.filter (fun (_, points) -> points >= toGo) |> Seq.map (fun (kv, _) -> kv) |> Seq.toList
                            if directHits.Length > 0 then
                                Some(directHits)
                            else None
                        else None
                        
                    if directCombos.IsSome then 
                        let productCombos = directCombos.Value |> Seq.map(fun kv -> kv.Value) |> Seq.reduce (fun p c -> p * c)
                        yield (otherIndex, numUniverses * (uint64 productCombos))
                        let toExclude = directCombos.Value |> Seq.map(fun kv -> kv.Key) |> Seq.toList
                        let toExplore = countsBySum |> Seq.filter (fun kv -> toExclude |> List.contains kv.Key = false)
                        yield! loop2 (turn + 1) (numUniverses * (uint64 outcome.Value)) modified toExplore
                    else yield! loop2 (turn + 1) (numUniverses * (uint64 outcome.Value)) modified countsBySum
        }
    loop2 0 1UL players countsBySum

let part2 input = 
    let data = parseInput input
    let players = data |> Array.map(fun (_, pos) -> pos) |> Array.toList

    let diracDieSides = 3
    let numThrowsPerTurn = 3

    let allCombosPerTurn = permutations numThrowsPerTurn [1..diracDieSides]
    let countsBySum = Map (allCombosPerTurn |> List.groupBy (fun f -> f |> List.sum) |> List.map (fun (sum, org) -> (sum, org.Length)))
    let winAt = 21

    let rec loop turn numUniverses (players: Player list) = seq {
            for outcome in countsBySum do
                let (newP, modified) = update (turn % 2) outcome.Key players
                if newP.Score >= winAt then
                    let winner = if modified[0].Score > modified[1].Score then 0 else 1
                    yield (winner, numUniverses * (uint64 outcome.Value))
                else
                    yield! loop (turn + 1) (numUniverses * (uint64 outcome.Value)) modified
        }

    let seq = loop 0 1UL players

    let xxx = seq |> Seq.fold (fun (agg: uint64 list) (winner, numUniverses) ->
                                                    let ulWinner = uint64 winner
                                                    let newWins = [
                                                        agg[0] + ((1UL - ulWinner) * numUniverses);
                                                        agg[1] + (ulWinner * numUniverses);
                                                    ]
                                                    newWins
                                                    ) [0UL; 0UL;]
    let result = xxx |> List.max
    
    result
