package ch.aamiguet.advent2022

final class Day10Test extends TestSuite:
  val d = Day10("input/test10.txt")

  val screen =
    """|##..##..##..##..##..##..##..##..##..##..
       |###...###...###...###...###...###...###.
       |####....####....####....####....####....
       |#####.....#####.....#####.....#####.....
       |######......######......######......####
       |#######.......#######.......#######.....""".stripMargin.split("\n").toList

  test("day 10") {
    expect(
      d.solvePart1 === 13140,
      d.screen === screen,
    )
  }
