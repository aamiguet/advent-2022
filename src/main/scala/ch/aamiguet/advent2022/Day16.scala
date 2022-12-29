package ch.aamiguet.advent2022

import scala.io.Source

case class Day16(
    filename: String
  ):
  opaque type Id = String

  case class Valve(
      id: Id,
      flowRate: Int,
      tunnels: Map[Id, Int],
    )

  case class Worker(
      name: Id,
      pos: Id,
      timeToNext: Int,
  )

  case class State(
      workers: List[Worker],
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

  @scala.annotation.tailrec
  private def goodStates(states: List[State], acc: List[State]): List[State] =
  // this is a bit of a hack, but it works
  // we are only looking at the best 20000 states at each step
  // it seems to be enough to find the solution, but of course
  // I guess there's a better way to do it
    val nextStates =
      (for {
        state <- states
        worker <- List(state.workers.head)
        valve <- List(strippedGraph(worker.pos))
        tunnel <- valve.tunnels
        if state.toVisit.contains(tunnel._1) && tunnel._2 < state.remainingTime
      } yield
        val newTimeToNext = tunnel._2 + 1
        val newWorkers = (worker.copy(pos = tunnel._1, timeToNext = newTimeToNext) :: state.workers.tail).sortBy(_.timeToNext)
        val newRemainingTime = state.remainingTime - newWorkers.head.timeToNext
        State(
          newWorkers.map(w => w.copy(timeToNext = w.timeToNext - newWorkers.head.timeToNext)),
          state.pressureReleased + strippedGraph(tunnel._1).flowRate * (state.remainingTime - newTimeToNext),
          newRemainingTime,
          state.toVisit - tunnel._1,
        )
      ).sortBy(_.pressureReleased).reverse.take(20000)
    if nextStates.isEmpty then acc
    else goodStates(nextStates, acc ++ nextStates)

  def solvePart1: Int =
    val workers =
      List(
        Worker("Me", "AA", 0),
      )
    val init = List(State(workers, 0, 30, relevantIds))
    val finalStates = goodStates(init, init)
    finalStates.map(_.pressureReleased).max

  def solvePart2: Int =
    val workers =
      List(
        Worker("Me", "AA", 0),
        Worker("Elephant", "AA", 0),
      )
    val init = List(State(workers, 0, 26, relevantIds))
    val finalStates = goodStates(init, init)
    finalStates.map(_.pressureReleased).max

