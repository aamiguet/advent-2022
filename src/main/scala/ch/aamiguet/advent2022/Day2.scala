package ch.aamiguet.advent2022

import scala.io.Source

final case class Day2(
    filename: String
  ):
  trait Shape:
    def value: Int
    def score(s: Shape): Int
    def shape(result: String): Shape

  case object Rock extends Shape:
    val value = 1
    def score(s: Shape): Int = s match
      case Paper => 0
      case Scissors => 6
      case Rock => 3
    def shape(result: String): Shape = result match
      case "X" => Scissors
      case "Y" => Rock
      case "Z" => Paper

  case object Paper extends Shape:
    def value = 2
    def score(s: Shape): Int = s match
      case Paper => 3
      case Scissors => 0
      case Rock => 6
    def shape(result: String): Shape = result match
      case "X" => Rock
      case "Y" => Paper
      case "Z" => Scissors

  case object Scissors extends Shape:
    def value = 3
    def score(s: Shape): Int = s match
      case Paper => 6
      case Scissors => 3
      case Rock => 0
    def shape(result: String): Shape = result match
      case "X" => Paper
      case "Y" => Scissors
      case "Z" => Rock

  object Shape:
    def apply(s: String): Shape = s match
      case "A" => Rock
      case "B" => Paper
      case "C" => Scissors
      case "X" => Rock
      case "Y" => Paper
      case "Z" => Scissors

  lazy val lines = Source.fromFile(filename).getLines().toList.filter(_.nonEmpty)

  def solvePart1: Int =
    lazy val matches = lines.map(l => (Shape(l(0).toString), Shape(l(2).toString)))
    matches.map(m => m._2.value + m._2.score(m._1)).sum

  def solvePart2: Int =
    lazy val matches = lines.map { l =>
      val opponent = Shape(l(0).toString)
      val result = l(2).toString
      (opponent, opponent.shape(result))
    }
    matches.map(m => m._2.value + m._2.score(m._1)).sum
