package ch.aamiguet.advent2022

final class Day17Test extends TestSuite:
  val d = Day17("input/test17.txt")

  test("day 17") {
    expect(
      d.solvePart1 === 3068,
      d.solvePart2 === 1514285714288L
    )
  }
