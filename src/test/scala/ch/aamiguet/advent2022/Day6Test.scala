package ch.aamiguet.advent2022

final class Day6Test extends TestSuite:
  val d = Day6("input/test6.txt")

  test("day 6 - part1") {
    expect(
      d.firstStartOfPacketMarker("mjqjpqmgbljsphdztnvjfqwrcgsmlb") === 7,
      d.firstStartOfPacketMarker("bvwbjplbgvbhsrlpgdmjqwftvncz") === 5,
      d.firstStartOfPacketMarker("nppdvjthqldpwncqszvftbrmjlhg") === 6,
      d.firstStartOfPacketMarker("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg") === 10,
      d.firstStartOfPacketMarker("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw") === 11,
    )
  }

  test("day 6 - part2") {
    expect(
      d.firstStartOfMessageMarker("mjqjpqmgbljsphdztnvjfqwrcgsmlb") === 19,
      d.firstStartOfMessageMarker("bvwbjplbgvbhsrlpgdmjqwftvncz") === 23,
      d.firstStartOfMessageMarker("nppdvjthqldpwncqszvftbrmjlhg") === 23,
      d.firstStartOfMessageMarker("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg") === 29,
      d.firstStartOfMessageMarker("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw") === 26,
    )
  }