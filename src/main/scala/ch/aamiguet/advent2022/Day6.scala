package ch.aamiguet.advent2022

import scala.io.Source

final case class Day6(
    filename: String
  ):

  lazy val line = Source.fromFile(filename).getLines().mkString("")

  def firstMarker(line: String, window: Int): Int =
    line.sliding(window, 1).zipWithIndex.find(_._1.distinct.size == window).map(_._2).get + window

  def firstStartOfPacketMarker(line: String): Int = firstMarker(line, 4)
  def firstStartOfMessageMarker(line: String): Int = firstMarker(line, 14)

  def solvePart1: Int = firstStartOfPacketMarker(line)
  def solvePart2: Int = firstStartOfMessageMarker(line)
