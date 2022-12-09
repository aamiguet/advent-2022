package ch.aamiguet.advent2022

import scala.io.Source

final case class Day7(
    filename: String
  ):
  trait Command
  final case class Cd(dir: String) extends Command
  final case class Ls(output: List[String]) extends Command

  object Command:
    def parse(lines: List[String]): List[Command] =
      def loop(lines: List[String], acc: List[Command]): List[Command] =
        if lines.isEmpty then acc
        else
          val l = lines.head
          if l.startsWith("$") then
            val arr = l.drop(2).split(" ")
            arr(0) match
              case "cd" => loop(lines.tail, Cd(arr(1)) :: acc)
              case "ls" =>
                val (output, rest) = lines.tail.span(!_.startsWith("$"))
                loop(rest, Ls(output) :: acc)
          else loop(lines.tail, acc)
      loop(lines, Nil).reverse

  trait Node:
    def name: String
    def children: List[Node]
    def size: Int

  final case class Dir(
      name: String,
      children: List[Node],
    ) extends Node:
    def size: Int = children.map(_.size).sum

  final case class File(
      name: String,
      size: Int,
    ) extends Node:
    def children: List[Node] = Nil

  object Node:
    def apply(description: String): Node =
      val arr = description.split(" ")
      arr(0) match
        case "dir" => Dir(arr(1), Nil)
        case size => File(arr(1), size.toInt)

  lazy val lines = Source.fromFile(filename).getLines().filter(_.nonEmpty).toList
  lazy val commands = Command.parse(lines)

  def execute(node: Node, commands: List[Command]): (Node, List[Command]) =
    if commands.isEmpty then (node, commands)
    else
      commands.head match
        case Cd(dir) =>
          dir match
            case "/" if node.name == "/" => execute(node, commands.tail)
            case "/" => (node, commands)
            case ".." => (node, commands.tail)
            case name =>
              node.children.find(_.name == name) match
                case Some(child) =>
                  val (newChild, newCommands) = execute(child, commands.tail)
                  val newNode = node
                    .asInstanceOf[Dir]
                    .copy(children = newChild :: node.children.filterNot(_.name == name))
                  execute(newNode, newCommands)
                case None => execute(node, commands.tail)
        case Ls(output) =>
          val children = output.map(Node.apply)
          val newNode = node.asInstanceOf[Dir].copy(children = children)
          execute(newNode, commands.tail)

  lazy val root = execute(Dir("/", Nil), commands)._1.asInstanceOf[Dir]

  def findSmallDirs(node: Dir, maxSize: Int): List[Dir] =
    val ds = node.children.collect { case dir: Dir => dir }.flatMap(findSmallDirs(_, maxSize))
    if node.size <= maxSize then node :: ds
    else ds

  def findLargeDirs(node: Dir, minSize: Int): List[Dir] =
    val ds = node.children.collect { case dir: Dir => dir }.flatMap(findLargeDirs(_, minSize))
    if node.size >= minSize then node :: ds
    else ds

  def solvePart1: Int =
    findSmallDirs(root, 100000).map(_.size).sum
  def solvePart2: Int =
    val unusedSpace = 70000000 - root.size
    val minSpace = 30000000 - unusedSpace
    findLargeDirs(root, minSpace).map(_.size).min

