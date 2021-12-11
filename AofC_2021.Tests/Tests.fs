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
let ``D5`` () =
    let input = "
0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2
    "
    let pt1 = D5.part1 input
    Assert.Equal(5, pt1);

    let pt2 = D5.part2 input
    Assert.Equal(12, pt2);

[<Fact>]
let ``D6`` () =
    let input = "
3,4,3,1,2
    "
    let pt1 = D6.part1 input
    Assert.Equal(5934, pt1);

    let pt2 = D6.part2 input
    Assert.Equal(26984457539UL, pt2);

[<Fact>]
let ``D7`` () =
    let input = "
16,1,2,0,4,2,7,1,2,14
    "
    let pt1 = D7.part1 input
    Assert.Equal(37, pt1);

    let pt2 = D7.part2 input
    Assert.Equal(168, pt2);

[<Fact>]
let ``D8`` () =
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
    let pt1 = D8.part1 input
    Assert.Equal(26, pt1);

    let pt2 = D8.part2 input
    Assert.Equal(61229, pt2);

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

[<Fact>]
let ``D11`` () =
    let input = "
5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526
    "
    let pt1 = D11.part1 input
    Assert.Equal(1656, pt1);

    let pt2 = D11.part2 input
    Assert.Equal(195, pt2);