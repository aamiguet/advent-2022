package ch.aamiguet.advent2022

import scala.io.Source

final case class Day25(
    filename: String
  ):
  val MAX_PLACES = 20
  lazy val powersOf5 = LazyList.iterate(1L)(_ * 5).take(MAX_PLACES).toArray

  case class Snafu(
      places: Array[Long]
    ):
    def toLong: Long =
      places
        .zipWithIndex
        .map {
          case (place, index) =>
            place * powersOf5(index)
        }
        .sum

    override def toString: String =
      places
        .map {
          case 2L => "2"
          case 1L => "1"
          case 0L => "0"
          case -1L => "-"
          case -2L => "="
        }
        .mkString("")
        .reverse
        .dropWhile(_ == '0')

  case object Snafu:
    def apply(s: String): Snafu =
      Snafu(
        s.reverse
          .map {
            case '2' => 2L
            case '1' => 1L
            case '0' => 0L
            case '-' => -1L
            case '=' => -2L
          }
          .toArray
      )
    def apply(l: Long): Snafu =
      val places = scala.collection.mutable.ArrayBuffer.fill(MAX_PLACES)(0L)
      var rest = l
      (0 until MAX_PLACES).reverse.foreach { i =>
        if powersOf5(i) <= rest then
          val m = rest/powersOf5(i)
          rest = rest - m * powersOf5(i)
          if m > 2 then
            var j = i + 1
            while places(j) == 2 do
              places(j) = -2
              j = j + 1
            places(j) = places(j) + 1
            places(i) = m - 5
          else
            places(i) = m
      }
      Snafu(places.toArray)

  lazy val fuelRequierements = Source
    .fromFile(filename)
    .getLines()
    .toList
    .filter(_.nonEmpty)
    .map(Snafu(_))
    .map(_.toLong)
    .sum

  def solvePart1: String =
    Snafu(fuelRequierements).toString