package ch.aamiguet.advent2022

import scala.io.Source
import scala.collection.mutable.ArrayBuffer

final case class Day10(
    filename: String
  ):

  lazy val lines = Source.fromFile(filename).getLines().filter(_.nonEmpty).toList

  lazy val state: Array[Int] =
    val arr = ArrayBuffer(0, 1)
    lines foreach { _ match
      case "noop" => arr += arr.last
      case op => {
        arr += arr.last
        arr += arr.last + op.split(" ")(1).toInt
      }
    }
    arr.toArray

  lazy val screen: List[String] =
    (0 to 5).toList.map{ y =>
      (1 to 40).toList.map{ x => {
        val pixel = state(y * 40 + x)
        if Math.abs((x - 1) - pixel) <= 1 then "#"
        else "."
      } }.mkString
    }

  def signalStrength(arr: Array[Int], cycle: List[Int]): Int =
    cycle.map(c => arr(c) * c).sum

  def solvePart1: Int = signalStrength(state, List(20, 60, 100, 140, 180, 220))
  def solvePart2: Unit = screen foreach println
