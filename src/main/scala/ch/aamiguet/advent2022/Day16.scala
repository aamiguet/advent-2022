package ch.aamiguet.advent2022

import scala.io.Source
import cats.syntax.validated

case class Day16(
    filename: String
  ):
  opaque type Id = String

  case class Valve(
      id: Id,
      flowRate: Int,
      tunnels: Map[Id, Int],
    )

  case class State(
      pos: Id,
      pressureReleased: Int,
      remainingTime: Int,
      toVisit: Set[Id],
    )

  object Valve:
    def apply(line: String): Valve =
      // "Valve OM has flow rate=0; tunnels lead to valves AA, EZ"
      val id = line.slice(6, 8)
      val flowRate = line.drop(23).takeWhile(_ != ';').toInt
      val tunnels = line
        .reverse
        .takeWhile(c => c != 's' && c != 'e')
        .reverse
        .drop(1)
        .split(", ")
        .map(_ -> 1)
        .toMap
      Valve(id, flowRate, tunnels)

  lazy val valves = Source
    .fromFile(filename)
    .getLines()
    .filter(_.nonEmpty)
    .map(Valve(_))
    .toList
    .map(v => v.id -> v)
    .toMap
  lazy val valvesId = valves.keys.toSet

  def allTunnels(
      acc: Map[Id, Int],
      currentTime: Int,
      toVisit: Set[Id],
    ): Map[Id, Int] =
    val nextTunnels = acc
      .filter(_._2 == currentTime)
      .flatMap(tunnel =>
        valves
          .get(tunnel._1)
          .get
          .tunnels
          .filter(t => toVisit.contains(t._1))
          .map(t => t._1 -> (currentTime + 1))
      )
    if nextTunnels.isEmpty then acc
    else
      val newAcc = acc ++ nextTunnels
      allTunnels(newAcc, currentTime + 1, toVisit.filterNot(nextTunnels.keySet.contains))

  lazy val graph =
    valves.map {
      case (id, valve) =>
        val tunnels = allTunnels(Map(id -> 0), 0, valvesId - id)
        id -> valve.copy(tunnels = tunnels)
    }
  lazy val strippedGraph =
    graph.filter((id, valve) => id == "AA" || valve.flowRate > 0)
  lazy val relevantIds = strippedGraph.keys.toSet - "AA"
  lazy val initialState = State("AA", 0, 30, relevantIds)

  def allStates(states: List[State], acc: List[State]): List[State] =
    val nextStates =
      for {
        state <- states
        valve <- List(strippedGraph(state.pos))
        tunnel <- valve.tunnels
        if state.toVisit.contains(tunnel._1) && tunnel._2 < state.remainingTime
      } yield
        val newRemainingTime = state.remainingTime - (tunnel._2 + 1)
        State(
          tunnel._1,
          state.pressureReleased + strippedGraph(tunnel._1).flowRate * newRemainingTime,
          newRemainingTime,
          state.toVisit - tunnel._1,
        )
    if nextStates.isEmpty then acc
    else allStates(nextStates, acc ++ nextStates)

  def solvePart1: Int =
    val init = List(initialState)
    val finalStates = allStates(init, init)
    finalStates.map(_.pressureReleased).max

  def solvePart2: Int = ???
