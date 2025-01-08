package ch.aamiguet.advent2022

final class Day15Test extends TestSuite:
  val d = Day15("input/test15.txt")

  test("day 15"):
    expect(
      d.coveredPositions(10) === 26,
      d.tuningFrequency(20) === 56000011L,
    )
