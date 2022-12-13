package ch.aamiguet.advent2022

import scala.io.Source

case class Day13(
    filename: String
  ):
  trait Tree
  final case class Leaf(n: Int) extends Tree
  final case class Branch(trees: List[Tree]) extends Tree

  trait Decision
  case object Undecided extends Decision
  case object Correct extends Decision
  case object Incorrect extends Decision

  def compare(left: Tree, right: Tree): Decision =
    (left, right) match
      case (Leaf(n1), Leaf(n2)) =>
        if n1 < n2 then Correct else if n1 == n2 then Undecided else Incorrect
      case (Branch(t1), Branch(t2)) =>
        if t1.isEmpty && t2.isEmpty then Undecided
        else if t1.isEmpty then Correct
        else if t2.isEmpty then Incorrect
        else
          val d = compare(t1.head, t2.head)
          if d == Undecided then compare(Branch(t1.tail), Branch(t2.tail))
          else d
      case (l: Leaf, r: Branch) => compare(Branch(List(l)), r)
      case (l: Branch, r: Leaf) => compare(l, Branch(List(r)))
      case _ => Undecided

  def parse(s: String): Tree =
    if s.startsWith("[") then
      val inner = s.drop(1).dropRight(1)
      if inner == "" then Branch(List())
      else
        val trees = inner.foldLeft((List(""), 0)) { (acc, c) =>
          if c == '[' then
            (acc._1.head + c :: acc._1.tail, acc._2 + 1)
          else if c == ']' then
            (acc._1.head + c :: acc._1.tail, acc._2 - 1)
          else if c == ',' && acc._2 == 0 then
            ("" :: acc._1, 0)
          else
            (acc._1.head + c :: acc._1.tail, acc._2)
        }._1.map(parse).reverse.toList
        Branch(trees)
    else
      Leaf(s.toInt)

  lazy val lines = Source.fromFile(filename).getLines().toList
  lazy val trees = lines.grouped(3).map(l => (parse(l(0)), parse(l(1)))).toList

  def solvePart1: Int =
    val decisions = trees.map(t => compare(t._1, t._2))
    decisions.zipWithIndex.filter(_._1 == Correct).map(_._2 + 1).sum

  def solvePart2: Int =
    val d2 = parse("[[2]]")
    val d6 = parse("[[6]]")
    val packets = lines.filter(_.nonEmpty).map(parse) :+ d2 :+ d6
    val sorted = packets.sortWith((l, r) => compare(l, r) == Correct)
    val i2 = sorted.indexWhere(_ == d2) + 1
    val i6 = sorted.indexWhere(_ == d6) + 1
    i2 * i6
