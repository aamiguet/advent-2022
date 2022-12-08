package ch.aamiguet.advent2022

import scala.io.Source

final case class Day8(
    filename: String
  ):
  lazy val lines = Source.fromFile(filename).getLines().filter(_.nonEmpty)
  lazy val trees = lines.map(_.split("").map(_.toInt)).toArray

  lazy val width = trees(0).length
  lazy val height = trees.length

  def scenicScore(x: Int, y: Int): Int =
    def view(ts: List[Int], t: Int): Int =
      ts match
        case Nil => 0
        case head :: tail => if head < t then 1 + view(tail, t) else 1

    if x == 0 || x == width - 1 || y == 0 || y == height - 1 then 0
    else
      val t = trees(x)(y)
      val leftView = view((for (xx <- 0 until x) yield trees(xx)(y)).reverse.toList, t)
      val rightView = view((for (xx <- x + 1 until width) yield trees(xx)(y)).toList, t)
      val topView = view((for (yy <- 0 until y) yield trees(x)(yy)).reverse.toList, t)
      val bottomView = view((for (yy <- y + 1 until height) yield trees(x)(yy)).toList, t)
      leftView * rightView * topView * bottomView

  def isVisible(x: Int, y: Int): Boolean =
    if x == 0 || x == width - 1 || y == 0 || y == height - 1 then true
    else
      val t = trees(x)(y)
      t > (for (xx <- 0 until x) yield trees(xx)(y)).max ||
      t > (for (xx <- x + 1 until width) yield trees(xx)(y)).max ||
      t > (for (yy <- 0 until y) yield trees(x)(yy)).max ||
      t > (for (yy <- y + 1 until height) yield trees(x)(yy)).max

  def solvePart1: Int = (for {
    y <- 0 until height
    x <- 0 until width
    if isVisible(x, y)
  } yield 1).sum

  def solvePart2: Int = (for {
    y <- 0 until height
    x <- 0 until width
  } yield scenicScore(x, y)).max
