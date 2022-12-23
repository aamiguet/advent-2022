package ch.aamiguet.advent2022

import scala.io.Source
import java.beans.Expression

final case class Day21(
    filename: String
  ):

  trait Operator:
    def fn: (Long, Long) => Long
  case object Plus extends Operator:
    def fn = (x: Long, y: Long) => x + y
  case object Minus extends Operator:
    def fn = (x: Long, y: Long) => x - y
  case object Times extends Operator:
    def fn = (x: Long, y: Long) => x * y
  case object Div extends Operator:
    def fn = (x: Long, y: Long) => x / y

  trait Expression:
    val name: String
    def eval: Long
  case class Operation (
    name: String,
    left: Expression,
    right: Expression,
    op: Operator
  ) extends Expression:
    def eval = op.fn(left.eval, right.eval)

  case class Value(name: String, n: Long) extends Expression:
    val eval = n

  lazy val lines = Source.fromFile(filename).getLines().toList.filter(_.nonEmpty)
  lazy val monkeys: Map[String, String] = lines.map{ l =>
    val s = l.split(": ")
    s(0) -> s(1)
  }.toMap

  def parse(name: String, expr: String): Expression =
    val parts = expr.split(" ")
    if parts.length == 1 then
      Value(name, parts(0).toLong)
    else
      val left = parse(parts(0), monkeys(parts(0)))
      val right = parse(parts(2), monkeys(parts(2)))
      val op = parts(1) match
        case "+" => Plus
        case "-" => Minus
        case "*" => Times
        case "/" => Div
      Operation(name, left, right, op)

  def rootNumber: Long =
    val root = monkeys("root")
    val rootExpr = parse("root", root)
    rootExpr.eval

  def humnIsInExpr(expr: Expression): Boolean =
    expr match
      case Operation(_, left, right, _) =>
        humnIsInExpr(left) || humnIsInExpr(right)
      case Value(name, _) =>
        name == "humn"

  def findingHumnNumber(expr: Expression, expectedValue: Long): Long =
    expr match
      case Value(name, _) if name == "humn" =>
        expectedValue
      case Operation(_, left, right, op) =>
        if humnIsInExpr(left) then
          val ev = op match
            case Plus => expectedValue - right.eval
            case Minus => expectedValue + right.eval
            case Times => expectedValue / right.eval
            case Div => expectedValue * right.eval
          findingHumnNumber(left, ev)
        else
          val ev = op match
            case Plus => expectedValue - left.eval
            case Minus => left.eval - expectedValue
            case Times => expectedValue / left.eval
            case Div => left.eval / expectedValue
          findingHumnNumber(right, ev)
      case _ =>
        throw Error("Unexpected")

  def findingHumnNumber(leftExpr: Expression, rightExpr: Expression): Long =
    if humnIsInExpr(leftExpr) then
      findingHumnNumber(leftExpr, rightExpr.eval)
    else
      findingHumnNumber(rightExpr, leftExpr.eval)

  def findingHumnNumber: Long =
    val arr = monkeys("root").split(" ")
    val leftExpr = parse(arr(0), monkeys(arr(0)))
    val rightExpr = parse(arr(2), monkeys(arr(2)))
    findingHumnNumber(leftExpr, rightExpr)

  def solvePart1: Long =
    rootNumber

  def solvePart2: Long =
    findingHumnNumber