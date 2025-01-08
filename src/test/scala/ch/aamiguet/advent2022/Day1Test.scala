package ch.aamiguet
package advent2022

final class Day1Test extends TestSuite:

  val day1 = Day1("input/test1.txt")

  test("day 1"):
    expect(
      day1.solvePart1 === 24000,
      day1.solvePart2 === 45000,
    )
