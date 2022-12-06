package advent2022.day5

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source

object Day5 extends App {

  def readFile(): List[String] = {
    val bufferedSource = Source.fromFile("src/main/scala/advent2022/day5/input.txt")
    val listBuffer = ListBuffer[String]()
    for (line <- bufferedSource.getLines) {
      listBuffer += line
    }
    listBuffer.toList
  }

  def parseStacks(list: List[List[Char]]): List[List[String]] = {
    val stacks = ListBuffer[List[String]]()
    stacks.toList
  }

  def parseMoves(list: List[String]): List[(Int, Int, Int)] = {
    list.map{
      str =>
        str.split(" ")
    }.map(
      array =>
        (array(1).toInt, array(3).toInt, array(5).toInt)
    )
  }

  def executeMove(stacks: List[mutable.Stack[String]], move: (Int, Int, Int)): List[mutable.Stack[String]] = {
    for (x <- 0 until move._1) {
      val crate = stacks(move._2 - 1).pop()
      stacks(move._3 - 1).push(crate)
    }
    stacks
  }

  def executeMoves(stacks: List[mutable.Stack[String]], moves: List[(Int, Int, Int)]): List[mutable.Stack[String]] = {
    var outputStack = stacks
    moves.foreach{
      move =>
        outputStack = executeMove(outputStack, move)
    }
    outputStack
  }

  def part1(): String = {
    val parsedStacks = List(
    mutable.Stack("R", "P", "C", "D", "B", "G").reverse,
    mutable.Stack("H", "V", "G").reverse,
    mutable.Stack("N", "S", "Q", "D", "J", "P", "M").reverse,
    mutable.Stack("P", "S", "L", "G", "D", "C", "N", "M").reverse,
    mutable.Stack("J", "B", "N", "C", "P", "F", "L", "S").reverse,
    mutable.Stack("Q", "B", "D", "Z", "V", "G", "T", "S").reverse,
    mutable.Stack("B", "Z", "M", "H", "F", "T", "Q").reverse,
    mutable.Stack("C", "M", "D", "B", "F").reverse,
    mutable.Stack("F", "C", "Q", "G").reverse)
    val rawInput = readFile()
    val (stacks, moves) = rawInput.splitAt(9)
    val parsedMoves = parseMoves(moves.drop(1))
    val finalStack = executeMoves(parsedStacks, parsedMoves)
    val finalString = mutable.StringBuilder()
    finalStack.foreach { stack =>
      finalString ++= stack.pop()
    }
    finalString.toString()
  }

  println(part1())

  def executeMove9001(stacks: List[mutable.Stack[String]], move: (Int, Int, Int)): List[mutable.Stack[String]] = {
    val tempStack = mutable.Stack[String]()
    for (x <- 0 until move._1) {
      tempStack.push(stacks(move._2 - 1).pop())
    }
    for (x <- 0 until move._1) {
      stacks(move._3 - 1).push(tempStack.pop())
    }
    stacks
  }

  def executeMoves9001(stacks: List[mutable.Stack[String]], moves: List[(Int, Int, Int)]): List[mutable.Stack[String]] = {
    var outputStack = stacks
    moves.foreach {
      move =>
        outputStack = executeMove9001(outputStack, move)
    }
    outputStack
  }

  def part2(): String = {
    val parsedStacks = List(
    mutable.Stack("R", "P", "C", "D", "B", "G").reverse,
    mutable.Stack("H", "V", "G").reverse,
    mutable.Stack("N", "S", "Q", "D", "J", "P", "M").reverse,
    mutable.Stack("P", "S", "L", "G", "D", "C", "N", "M").reverse,
    mutable.Stack("J", "B", "N", "C", "P", "F", "L", "S").reverse,
    mutable.Stack("Q", "B", "D", "Z", "V", "G", "T", "S").reverse,
    mutable.Stack("B", "Z", "M", "H", "F", "T", "Q").reverse,
    mutable.Stack("C", "M", "D", "B", "F").reverse,
    mutable.Stack("F", "C", "Q", "G").reverse)
    val rawInput = readFile()
    val (stacks, moves) = rawInput.splitAt(9)
    val parsedMoves = parseMoves(moves.drop(1))
    val finalStack =  executeMoves9001(parsedStacks, parsedMoves)
    val finalString = mutable.StringBuilder()
    finalStack.foreach { stack =>
      finalString ++= stack.pop()
    }
    finalString.toString()
  }

  println(part2())

}
