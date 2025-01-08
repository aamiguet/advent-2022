package ch.aamiguet.advent2022

final class Day7Test extends TestSuite:
  val d = Day7("input/test7.txt")

  test("day 7"):
    expect(
      d.solvePart1 === 95437,
      d.solvePart2 === 24933642,
    )
