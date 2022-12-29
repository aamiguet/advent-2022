package ch.aamiguet.advent2022

import scala.io.Source

final case class Day19(
    filename: String
  ):
  case class Blueprint(
      id: Int,
      oreRobot: Int,
      clayRobot: Int,
      obsidianRobot: (Int, Int),
      geodeRobot: (Int, Int),
    )

  case class State(
      oreRobot: Int = 1,
      clayRobot: Int = 0,
      obsidianRobot: Int = 0,
      geodeRobot: Int = 0,
      ore: Int = 0,
      clay: Int = 0,
      obsidian: Int = 0,
      geode: Int = 0,
    )

  trait Decision
  case object DoNothing extends Decision
  case object BuildOreRobot extends Decision
  case object BuildClayRobot extends Decision
  case object BuildObsidianRobot extends Decision
  case object BuildGeodeRobot extends Decision

  def decisions(state: State, blueprint: Blueprint): List[Decision] =
    val ds = scala.collection.mutable.ListBuffer.empty[Decision]
    if state.ore >= blueprint.geodeRobot._1 && state.obsidian >= blueprint.geodeRobot._2 then
      ds += BuildGeodeRobot
    else
      if state.ore >= blueprint.obsidianRobot._1 && state.clay >= blueprint.obsidianRobot._2 then
        ds += BuildObsidianRobot
      if state.ore >= blueprint.clayRobot then ds += BuildClayRobot
      if state.ore >= blueprint.oreRobot then ds += BuildOreRobot
      ds += DoNothing
    ds.toList.take(2)

  def nextState(
      state: State,
      blueprint: Blueprint,
      decision: Decision,
    ): State =
    decision match
      case DoNothing => state
      case BuildOreRobot =>
        state.copy(oreRobot = state.oreRobot + 1, ore = state.ore - blueprint.oreRobot)
      case BuildClayRobot =>
        state.copy(clayRobot = state.clayRobot + 1, ore = state.ore - blueprint.clayRobot)
      case BuildObsidianRobot =>
        state.copy(
          obsidianRobot = state.obsidianRobot + 1,
          ore = state.ore - blueprint.obsidianRobot._1,
          clay = state.clay - blueprint.obsidianRobot._2,
        )
      case BuildGeodeRobot =>
        state.copy(
          geodeRobot = state.geodeRobot + 1,
          ore = state.ore - blueprint.geodeRobot._1,
          obsidian = state.obsidian - blueprint.geodeRobot._2,
        )

  def collect(state: State): State =
    state.copy(
      ore = state.ore + state.oreRobot,
      clay = state.clay + state.clayRobot,
      obsidian = state.obsidian + state.obsidianRobot,
      geode = state.geode + state.geodeRobot,
    )

  def nextStates(state: State, blueprint: Blueprint): List[State] =
    val afterCollectState = collect(state)
    decisions(state, blueprint).map(decision => nextState(afterCollectState, blueprint, decision))

  def finalStates(
      states: List[State],
      blueprint: Blueprint,
      time: Int,
    ): List[State] =
    if time == 0 then states
    else
      val ns = states.flatMap(nextStates(_, blueprint))
      finalStates(ns, blueprint, time - 1)

  def maxGeode(
    states: List[State]
  ): Int =
    states.map(_.geode).max

  val numberRegex = "[0-9]+".r
  lazy val blueprints = Source
    .fromFile(filename)
    .getLines()
    .toList
    .filter(_.nonEmpty)
    .map { line =>
      val ns = numberRegex.findAllIn(line).toList.map(_.toInt)
      Blueprint(ns(0), ns(1), ns(2), (ns(3), ns(4)), (ns(5), ns(6)))
    }

  def solvePart1: Int =
    blueprints.map(
      blueprint => blueprint.id * maxGeode(finalStates(List(State()), blueprint, 24))
    ).sum

  def solvePart2: Int =
    blueprints.take(3).map(
      blueprint => maxGeode(finalStates(List(State()), blueprint, 32))
    ).product