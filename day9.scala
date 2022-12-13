import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source

object Day9 extends App {

  def readFile(): List[(String, Int)] = {
    val bufferedSource = Source.fromFile("src/main/scala/advent2022/day9/input.txt")
    val listBuffer = ListBuffer[(String, Int)]()
    for (line <- bufferedSource.getLines) {
      val tuple = line match
        case s"$direction $distance" => (direction, distance.toInt)
      listBuffer += tuple
    }
    listBuffer.toList
  }

  def executeInstruction(instruction: (String, Int), headPosition: (Int, Int), tailPosition:(Int, Int), positions: mutable.Set[(Int, Int)]): ((Int, Int), (Int, Int)) = {
    var newHeadPosition = headPosition
    var newTailPosition = tailPosition
    positions += newTailPosition
    for (x <- 0 until instruction._2) {
      val (head, tail) = moveHeadAndTailInDirection(instruction._1, newHeadPosition, newTailPosition)
      newHeadPosition = head
      newTailPosition = tail
      positions += newTailPosition
    }
    (newHeadPosition, newTailPosition)
  }

  def moveHeadAndTailInDirection(direction: String, headPosition: (Int, Int), tailPosition:(Int, Int)): ((Int, Int), (Int, Int)) = {
    direction match
      case "U" =>
        val newHeadPosition = (headPosition._1, headPosition._2 + 1)
        val newTailPosition = getNewTailPosition(newHeadPosition, tailPosition)
        (newHeadPosition, newTailPosition)
      case "D" =>
        val newHeadPosition = (headPosition._1, headPosition._2 - 1)
        val newTailPosition = getNewTailPosition(newHeadPosition, tailPosition)
        (newHeadPosition, newTailPosition)
      case "L" =>
        val newHeadPosition = (headPosition._1 - 1, headPosition._2)
        val newTailPosition = getNewTailPosition(newHeadPosition, tailPosition)
        (newHeadPosition, newTailPosition)
      case "R" =>
        val newHeadPosition = (headPosition._1 + 1, headPosition._2)
        val newTailPosition = getNewTailPosition(newHeadPosition, tailPosition)
        (newHeadPosition, newTailPosition)
  }

  def getNewTailPosition(newHeadPosition: (Int, Int), oldTailPosition: (Int, Int)): (Int, Int) = {
    val positionDifference = (newHeadPosition._1 - oldTailPosition._1, newHeadPosition._2 - oldTailPosition._2)
    positionDifference match
      case (0, 0) | (0, 1) | (0, -1) | (1, 0) | (-1, 0) | (1, 1) | (-1, -1) | (1, -1) | (-1, 1) => oldTailPosition
      case (0, yDiff) =>
        if (yDiff > 0 ) {
          (newHeadPosition._1, newHeadPosition._2 - 1)
        } else {
          (newHeadPosition._1, newHeadPosition._2 + 1)
        }
      case (xDiff, 0) =>
        if (xDiff > 0) {
          (newHeadPosition._1 - 1, newHeadPosition._2)
        } else {
          (newHeadPosition._1 + 1, newHeadPosition._2)
        }
      case (xDiff, yDiff) => handleDiagonalDifference(oldTailPosition, xDiff, yDiff)
  }

  def handleDiagonalDifference(oldTailPosition: (Int, Int), xDiff: Int, yDiff: Int): (Int, Int) = {
    if (xDiff > 0 && yDiff > 0) {
      (oldTailPosition._1 + 1, oldTailPosition._2 + 1)
    } else if (xDiff < 0 && yDiff < 0) {
      (oldTailPosition._1 - 1, oldTailPosition._2 - 1)
    } else if (xDiff > 0 && yDiff < 0) {
      (oldTailPosition._1 + 1, oldTailPosition._2 - 1)
    } else {
      (oldTailPosition._1 - 1, oldTailPosition._2 + 1)
    }
  }

  def getNumberOfPositionsVisitedByTail(instructions: List[(String, Int)]): Int = {
    val positions = mutable.Set[(Int, Int)]()
    var headPosition = (1,1)
    var tailPosition = (1,1)

    for (instruction <- instructions) {
      val (head, tail) = executeInstruction(instruction, headPosition, tailPosition, positions)
      headPosition = head
      tailPosition = tail
    }

    positions.size
  }

  def part1(): Unit = {
    val instructions = readFile()
    val numPositionsVisited = getNumberOfPositionsVisitedByTail(instructions)
    println(numPositionsVisited)
  }

  part1()

  def executeInstructionOnLongRope(instruction: (String, Int), ropePositions: List[(Int, Int)], tailPositions: mutable.Set[(Int, Int)]): List[(Int, Int)] = {
    var newRopePositions = ropePositions
    for (x <- 0 until instruction._2) {
      val (head, next) = moveHeadAndTailInDirection(instruction._1, newRopePositions(0), newRopePositions(1))
      newRopePositions = newRopePositions.updated(0, head)
      newRopePositions = newRopePositions.updated(1, next)
      for (i <- 2 until newRopePositions.length) {
        val newPos = getNewTailPosition(newRopePositions(i-1), newRopePositions(i))
        newRopePositions = newRopePositions.updated(i, newPos)
        if (i == newRopePositions.length - 1) {
          tailPositions += newPos
        }
      }
    }
    newRopePositions
  }

  def getNumberOfPositionsVisitedByLongRopeTail(instructions: List[(String, Int)]): Int = {
    val tailPositions = mutable.Set[(Int, Int)]()
    var ropePositions = List((1,1), (1,1), (1,1), (1,1), (1,1), (1,1), (1,1), (1,1), (1,1), (1,1))

    for (instruction <- instructions) {
      ropePositions = executeInstructionOnLongRope(instruction, ropePositions, tailPositions)
    }
    tailPositions.size
  }

  def part2(): Unit = {
    val instructions = readFile()
    val numPositionsVisited = getNumberOfPositionsVisitedByLongRopeTail(instructions)
    println(numPositionsVisited)
  }

  part2()

}
