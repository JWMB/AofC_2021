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
    let items = D8.parseInput input
    Assert.Equal(10, items.Length);

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

[<Fact>]
let ``D12`` () =
    let input = "
fs-end
he-DX
fs-he
start-DX
pj-DX
end-zg
zg-sl
zg-pj
pj-he
RW-he
fs-DX
pj-RW
zg-RW
start-pj
he-WI
zg-he
pj-fs
start-RW
    "
    let pt1 = D12.part1 input
    Assert.Equal(226, pt1);

    let pt2 = D12.part2 input
    Assert.Equal(3509, pt2);

[<Fact>]
let ``D13`` () =
    let input = "
6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5
    "
    let pt1 = D13.part1 input
    Assert.Equal(17, pt1);

    let pt2 = D13.part2 input
    let expected = "
#####
#...#
#...#
#...#
#####"
    Assert.Equal(expected.Trim().Replace("\r", ""), pt2);


[<Fact>]
let ``D14`` () =
    let input = "
NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C
    "
    let pt1 = D14.part1 input
    Assert.Equal(1588, pt1);

    let pt2 = D14.part2 input
    Assert.Equal(2188189693529UL, pt2);
    
[<Fact>]
let ``D15`` () =
    let input = "
1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581
    "
    let pt1 = D15.part1 input
    Assert.Equal(40, pt1);
    
    let pt2 = D15.part2 input
    Assert.Equal(315, pt2);
  
[<Fact>]
let ``D16`` () =
    let p1Tests = [|
        ("8A004A801A8002F478", 16);
        ("620080001611562C8802118E34", 12);
        ("C0015000016115A2E0802F182340", 23);
        ("A0016C880162017C3686B18A3D4780", 31)
    |]
    for t in p1Tests do
        let r = D16.part1 (fst t)
        if r <> snd t then raise (new Exception($"Expected {snd t}, got {r} ({fst t})"))

    let p2Tests = [|
        ("C200B40A82", 3UL);
        ("04005AC33890", 54UL);
        ("880086C3E88112", 7UL);
        ("CE00C43D881120", 9UL);
        ("D8005AC2A8F0", 1UL);
        ("F600BC2D8F", 0UL);
        ("9C005AC2F8F0", 0UL);
        ("9C0141080250320F1802104A08", 1UL);
    |]
    for t in p2Tests do
        let r = D16.part2 (fst t)
        if r <> snd t then raise (new Exception($"Expected {snd t}, got {r} ({fst t})"))
  
[<Fact>]
let ``D17`` () =
    let input = "target area: x=20..30, y=-10..-5"
    //TODO: Assert.Equal(45, (D17.part1 input))
    Assert.Equal(112, (D17.part2 input))

[<Fact>]
let ``D18`` () =
    let input = "
[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
[[[5,[2,8]],4],[5,[[9,9],0]]]
[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
[[[[5,4],[7,7]],8],[[8,3],8]]
[[9,3],[[9,9],[6,[4,9]]]]
[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]
"
    Assert.Equal(4140, D18.part1 input)
    Assert.Equal(3993, D18.part2 input)
  
[<Fact>]
let ``D19`` () =
    let input = "
--- scanner 0 ---
404,-588,-901
528,-643,409
-838,591,734
390,-675,-793
-537,-823,-458
-485,-357,347
-345,-311,381
-661,-816,-575
-876,649,763
-618,-824,-621
553,345,-567
474,580,667
-447,-329,318
-584,868,-557
544,-627,-890
564,392,-477
455,729,728
-892,524,684
-689,845,-530
423,-701,434
7,-33,-71
630,319,-379
443,580,662
-789,900,-551
459,-707,401

--- scanner 1 ---
686,422,578
605,423,415
515,917,-361
-336,658,858
95,138,22
-476,619,847
-340,-569,-846
567,-361,727
-460,603,-452
669,-402,600
729,430,532
-500,-761,534
-322,571,750
-466,-666,-811
-429,-592,574
-355,545,-477
703,-491,-529
-328,-685,520
413,935,-424
-391,539,-444
586,-435,557
-364,-763,-893
807,-499,-711
755,-354,-619
553,889,-390

--- scanner 2 ---
649,640,665
682,-795,504
-784,533,-524
-644,584,-595
-588,-843,648
-30,6,44
-674,560,763
500,723,-460
609,671,-379
-555,-800,653
-675,-892,-343
697,-426,-610
578,704,681
493,664,-388
-671,-858,530
-667,343,800
571,-461,-707
-138,-166,112
-889,563,-600
646,-828,498
640,759,510
-630,509,768
-681,-892,-333
673,-379,-804
-742,-814,-386
577,-820,562

--- scanner 3 ---
-589,542,597
605,-692,669
-500,565,-823
-660,373,557
-458,-679,-417
-488,449,543
-626,468,-788
338,-750,-386
528,-832,-391
562,-778,733
-938,-730,414
543,643,-506
-524,371,-870
407,773,750
-104,29,83
378,-903,-323
-778,-728,485
426,699,580
-438,-605,-362
-469,-447,-387
509,732,623
647,635,-688
-868,-804,481
614,-800,639
595,780,-596

--- scanner 4 ---
727,592,562
-293,-554,779
441,611,-461
-714,465,-776
-743,427,-804
-660,-479,-426
832,-632,460
927,-485,-438
408,393,-506
466,436,-512
110,16,151
-258,-428,682
-393,719,612
-211,-452,876
808,-476,-593
-575,615,604
-485,667,467
-680,325,-822
-627,-443,-432
872,-547,-609
833,512,582
807,604,487
839,-516,451
891,-625,532
-652,-548,-490
30,-46,-14"
    Assert.Equal(79, D19.part1 input)
    Assert.Equal(3621, D19.part2 input)

[<Fact>]
let ``D20`` () =
    let input = "
..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..##
#..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###
.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#.
.#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#.....
.#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#..
...####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.....
..##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#

#..#.
#....
##..#
..#..
..###
"
    Assert.Equal(35, D20.part1 input)
    Assert.Equal(3351, D20.part2 input)

[<Fact>]
let ``D21`` () =
    let input = "
    Player 1 starting position: 4
    Player 2 starting position: 8
    "
    Assert.Equal(739785, D21.part1 input)

    // TODO: too slow! Assert.Equal(444356092776315UL, D21.part2 input)
