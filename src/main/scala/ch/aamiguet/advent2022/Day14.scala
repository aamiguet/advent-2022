package ch.aamiguet.advent2022

import scala.io.Source

final case class Day14(
    filename: String
  ):

  lazy val lines = Source.fromFile(filename).getLines().toList

  lazy val structure: Map[(Int, Int), Char] =
    val m = scala.collection.mutable.Map[(Int, Int), Char]()
    lines.foreach(line =>
      val coords = line.split(" -> ").toList.map(c =>
        val cs = c.split(",").map(_.toInt)
        (cs(0), cs(1))
      )
      val segments = coords.zip(coords.tail)
      segments.foreach((start, end) =>
        for {
          x <- (start._1 to end._1) ++ (end._1 to start._1)
          y <- (start._2 to end._2) ++ (end._2 to start._2)
        } do
          m((x, y)) = '#'
      )
    )
    m.toMap

  lazy val abyss = structure.keySet.map(_._2).max + 1
  val sandSource = (500, 0)

  def solvePart1: Int =
    val struc = scala.collection.mutable.Map.from(structure)
    var unit = sandSource
    while unit._2 < abyss
    do
      if !struc.isDefinedAt((unit._1, unit._2 + 1)) then
        unit = (unit._1, unit._2 + 1)
      else if !struc.isDefinedAt((unit._1 - 1, unit._2 + 1)) then
        unit = (unit._1 - 1, unit._2 + 1)
      else if !struc.isDefinedAt((unit._1 + 1, unit._2 + 1)) then
        unit = (unit._1 + 1, unit._2 + 1)
      else
        struc(unit) = 'o'
        unit = sandSource
    struc.values.filter(_ == 'o').size

  def solvePart2: Int =
    val struc = scala.collection.mutable.Map.from(structure)
    var unit = sandSource
    while !struc.isDefinedAt(sandSource)
    do
      if unit._2 == abyss then
        struc(unit) = 'o'
        unit = sandSource
      else if !struc.isDefinedAt((unit._1, unit._2 + 1)) then
        unit = (unit._1, unit._2 + 1)
      else if !struc.isDefinedAt((unit._1 - 1, unit._2 + 1)) then
        unit = (unit._1 - 1, unit._2 + 1)
      else if !struc.isDefinedAt((unit._1 + 1, unit._2 + 1)) then
        unit = (unit._1 + 1, unit._2 + 1)
      else
        struc(unit) = 'o'
        unit = sandSource
    struc.values.filter(_ == 'o').size
