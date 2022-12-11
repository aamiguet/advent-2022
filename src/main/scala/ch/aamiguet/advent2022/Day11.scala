package ch.aamiguet.advent2022

import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import scala.util.matching.Regex
import scala.collection.mutable

final case class Day11(
    filename: String
  ):
  type Id = Int
  type Item = Long

  final case class Monkey(
      id: Id,
      items: List[Item],
      operation: Item => Item,
      test: (Long, Id, Id),
      inspectionCount: Int = 0,
    ):
    def throwTo(item: Item): Id =
      if item % test._1 == 0 then test._2
      else test._3

  object Monkey:
    val numberRegex = "[0-9]+".r

    def parseFormula(s: String): (Item => Item) =
      def parseElem(s: String, item: Item): Item = s match
        case "old" => item
        case _ => s.toInt
      val arr = s.split(" ")
      val op = arr(1)
      (item: Item) => {
        val leftItem = parseElem(arr(0), item)
        val rightItem = parseElem(arr(2), item)
        op match
          case "+" => leftItem + rightItem
          case "*" => leftItem * rightItem
      }

    def apply(arr: Array[String]): Monkey =
      val id = numberRegex.findFirstIn(arr(0)).get.toInt
      val items = numberRegex.findAllMatchIn(arr(1)).toList.map(_.toString.toLong)
      val test = (
        numberRegex.findFirstIn(arr(3)).get.toLong,
        numberRegex.findFirstIn(arr(4)).get.toInt,
        numberRegex.findFirstIn(arr(5)).get.toInt,
      )
      val operation = parseFormula(arr(2).split(" = ")(1))

      Monkey(
        id,
        items,
        operation,
        test,
      )

  lazy val lines = Source.fromFile(filename).getLines().toArray
  lazy val startingMonkeys = lines.grouped(7).map(Monkey.apply).toArray

  def computeRoundWithDecreasingWorry(monkeys: Array[Monkey]): Array[Monkey] =
    val local: ArrayBuffer[Monkey] = ArrayBuffer.from(monkeys)
    for i <- 0 until local.length do
      val m = local(i)
      val items = m.items
      local.update(m.id, m.copy(items = Nil))
      items.foreach(i => {
        // inspection
        val newItem = local(m.id).operation(i) / 3
        local.update(m.id, local(m.id).copy(inspectionCount = local(m.id).inspectionCount + 1))
        // throw
        val targetId = local(m.id).throwTo(newItem)
        local.update(targetId, local(targetId).copy(items = local(targetId).items :+ newItem))
      })
    local.toArray

  def computeRoundsWithDecreasingWorry(monkeys: Array[Monkey], rounds: Int): Array[Monkey] =
    var local = monkeys
    for i <- 0 until rounds do
      local = computeRoundWithDecreasingWorry(local)
    local

  def computeRoundWithWorryManagement(monkeys: Array[Monkey], key: Long): Array[Monkey] =
    val local: ArrayBuffer[Monkey] = ArrayBuffer.from(monkeys)
    for i <- 0 until local.length do
      val m = local(i)
      val items = m.items
      local.update(m.id, m.copy(items = Nil))
      items.foreach(i => {
        // inspection
        val newItem = local(m.id).operation(i) % key
        local.update(m.id, local(m.id).copy(inspectionCount = local(m.id).inspectionCount + 1))
        // throw
        val targetId = local(m.id).throwTo(newItem)
        local.update(targetId, local(targetId).copy(items = local(targetId).items :+ newItem))
      })
    local.toArray

  def computeRoundsWithWorryManagement(monkeys: Array[Monkey], rounds: Int, key: Long): Array[Monkey] =
    var local = monkeys
    for i <- 0 until rounds do
      local = computeRoundWithWorryManagement(local, key)
    local

  def solvePart1: Int =
    val monkeys = computeRoundsWithDecreasingWorry(startingMonkeys, 20)
    monkeys.map(_.inspectionCount).toList.sorted.reverse.take(2).product

  def solvePart2: Long =
    val key = startingMonkeys.map(_.test._1).toList.product // we're multiplying primes here!
    val monkeys = computeRoundsWithWorryManagement(startingMonkeys, 10000, key)
    monkeys.map(_.inspectionCount.toLong).toList.sorted.reverse.take(2).product
