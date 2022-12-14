import scala.collection.mutable.ListBuffer
import scala.io.Source

object Day10 extends App {

  def readLines(): List[String] = {
    val bufferedSource = Source.fromFile("src/main/scala/advent2022/day10/input.txt")
    val listBuffer = ListBuffer[String]()
    for (line <- bufferedSource.getLines) {
      listBuffer += line
    }
    listBuffer.toList
  }

  def runCycles(instructions: List[String]): List[Int] = {
    val listBuffer = ListBuffer[Int]()
    var register = 1
    for (instruction <- instructions) {
      instruction match
        case "noop" =>
          listBuffer += register
        case s"addx $num" =>
          listBuffer += register
          register += num.toInt
          listBuffer += register
    }
    listBuffer.toList
  }

  def part1(): Unit = {
    val instructions = readLines()
    val listOfRegisterValues = runCycles(instructions)
    var result = 0
    for (x <- 20 to 220 by 40) {
      result += x * listOfRegisterValues(x - 2)
    }
    println(result)
  }

  part1()

  def part2(): Unit = {
    val instructions = readLines()
    val listOfRegisterValues = runCycles(instructions)
    for (row <- 0 until 6) {
      for (col <- 0 until 40) {
        var index = 40 * row + col - 1
        if (index < 0) {
          index = 0
        }
        if ((col - listOfRegisterValues(index)).abs < 2) {
          print("#")
        } else {
          print(".")
        }
      }
      println()
    }
  }

  part2()

}
