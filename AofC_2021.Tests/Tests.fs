module Tests

open System
open Xunit

[<Fact>]
let ``D1`` () =
    let input = "
    199
    200
    208
    210
    200
    207
    240
    269
    260
    263
    "
    let pt1 = D1.part1 input
    Assert.Equal(7, pt1);

    let pt2 = D1.part2 input
    Assert.Equal(5, pt2);

[<Fact>]
let ``D2`` () =
    let input = "
forward 5
down 5
forward 8
up 3
down 8
forward 2
    "
    let pt1 = D2.part1 input
    Assert.Equal(150, pt1);

    let pt2 = D2.part2 input
    Assert.Equal(900, pt2);

[<Fact>]
let ``D3`` () =
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
    let pt1 = D3.part1 input
    Assert.Equal(198, pt1);

    let pt2 = D3.part2 input
    Assert.Equal(230, pt2);


[<Fact>]
let ``D4`` () =
    let input = "
7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7
    "
    let pt1 = D4.part1 input
    Assert.Equal(4512, pt1);

    let pt2 = D4.part2 input
    Assert.Equal(1924, pt2);

[<Fact>]
let ``D9`` () =
    let input = "
2199943210
3987894921
9856789892
8767896789
9899965678
    "
    let pt1 = D9.part1 input
    Assert.Equal(15, pt1);

    let pt2 = D9.part2 input
    Assert.Equal(1134, pt2);

[<Fact>]
let ``D10`` () =
    let input = "
    [({(<(())[]>[[{[]{<()<>>
    [(()[<>])]({[<{<<[]>>(
    {([(<{}[<>[]}>{[]{[(<()>
    (((({<>}<{<{<>}{[]{[]{}
    [[<[([]))<([[{}[[()]]]
    [{[{({}]{}}([{[{{{}}([]
    {<[[]]>}<{[{[{[]{()[[[]
    [<(<(<(<{}))><([]([]()
    <{([([[(<>()){}]>(<<{{
    <{([{{}}[<[[[<>{}]]]>[]]
    "
    let pt1 = D10.part1 input
    Assert.Equal(26397, pt1);

    let pt2 = D10.part2 input
    Assert.Equal(288957L, pt2);
