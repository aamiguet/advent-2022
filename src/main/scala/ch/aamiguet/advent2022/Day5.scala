package ch.aamiguet.advent2022

import scala.io.Source
import scala.util.matching.Regex

final case class Day5(
    filename: String
  ):
  val numberRegex = "[0-9]+".r

  lazy val lines = Source.fromFile(filename).getLines().toList

  lazy val (state: Vector[String], moves: List[String]): (Vector[String], List[String]) =
    val (statePart, movePart) = lines.span(_.nonEmpty)
    val indices = numberRegex.findAllIn(statePart.last).toList.map(_.toInt - 1)
    val stateContainers = statePart.take(statePart.size - 1)
    val s = indices.map { i =>
      stateContainers.foldLeft("")((acc, c) =>
        c.charAt(i * 4 + 1) match {
          case ' ' => acc
          case c => acc + c
        }
      )
    }.toVector
    (s, movePart.filter(_.nonEmpty))

  def singleMove(
      state: Vector[String],
      from: Int,
      to: Int,
      n: Int,
    ): Vector[String] =
    if (n == 0) state
    else
      val c = state(from).head
      val newState = state
        .updated(from, state(from).tail)
        .updated(to, c.toString + state(to))
      singleMove(newState, from, to, n - 1)

  def moveLine9000(state: Vector[String], move: String): Vector[String] =
    val n :: from :: to :: Nil = numberRegex.findAllIn(move).toList.map(_.toInt): @unchecked
    singleMove(state, from - 1, to - 1, n)

  def moveLine9001(state: Vector[String], move: String): Vector[String] =
    val n :: from :: to :: Nil = numberRegex.findAllIn(move).toList.map(_.toInt): @unchecked
    val (t, r) = state(from - 1).splitAt(n)
    state
      .updated(from - 1, r)
      .updated(to - 1, t + state(to - 1))

  def moveLines(
      state: Vector[String],
      moves: List[String],
      moveLineFn: (Vector[String], String) => Vector[String],
    ): Vector[String] = moves match
    case Nil => state
    case head :: tail => moveLines(moveLineFn(state, head), tail, moveLineFn)

  def result(state: Vector[String]): String =
    state.map(_.headOption).filter(_.nonEmpty).map(_.get).mkString

  def solvePart1: String =
    val finalState = moveLines(state, moves, moveLine9000)
    result(finalState)

  def solvePart2: String =
    val finalState = moveLines(state, moves, moveLine9001)
    result(finalState)
