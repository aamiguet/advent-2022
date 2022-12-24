package ch.aamiguet.advent2022

import scala.io.Source

final case class Day20(
    filename: String
  ):
  case class Element(
      id: Int,
      value: Long,
    )

  lazy val numbers = Source
    .fromFile(filename)
    .getLines()
    .toList
    .filter(_.nonEmpty)
    .map(_.toLong)
    .zipWithIndex
    .map(e => Element(e._2, e._1))
  lazy val size = numbers.size
  val decryptionKey = 811589153L

  def mix(numbers: List[Element], acc: List[Element]): List[Element] =
    if numbers.isEmpty then acc
    else
      val current = numbers.head
      val move =
        val c = current.value % (size - 1)
        val l = if c < 0 then c + (size - 1) else c
        l.toInt
      val (left, right) = acc.splitAt(acc.indexOf(current))
      if move > right.tail.size then
        val (l, r) = left.splitAt(move - right.tail.size)
        mix(numbers.tail, l ++ List(current) ++ r ++ right.tail)
      else
        val (l, r) = right.tail.splitAt(move)
        mix(numbers.tail, left ++ l ++ List(current) ++ r)

  def coordinatesSum(numbers: List[Element]) =
    val zeroPos = numbers.takeWhile(_.value != 0).size
    List(1000, 2000, 3000).map(n => (n + zeroPos) % size).map(numbers(_).value).sum

  def solvePart1: Long =
    val mixed = mix(numbers, numbers)
    coordinatesSum(mixed)

  def solvePart2: Long =
    val decryptedNumbers = numbers.map(e => Element(e.id, e.value * decryptionKey))
    val finalNumbers = (0 until 10).foldLeft(decryptedNumbers)(
      (acc, _) => mix(decryptedNumbers, acc)
    )
    coordinatesSum(finalNumbers)
