package ch.aamiguet.advent2022

final class Day25Test extends TestSuite:
  val d = Day25("input/test25.txt")

  import d.*

  test("conversions decimal to Snafu"):
    expect(
      Snafu(1L).toString === "1",
      Snafu(2L).toString === "2",
      Snafu(3L).toString === "1=",
      Snafu(12345L).toString === "1-0---0",
      Snafu(314159265L).toString === "1121-1110-1=0",
    )

  test("day 25"):
    expect(
      d.solvePart1 === "2=-1=0",
    )
