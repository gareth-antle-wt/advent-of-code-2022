import scala.collection.mutable.ListBuffer
import scala.io.Source

object Day4 extends App {

  def readFile(): List[String] = {
    val bufferedSource = Source.fromFile("src/main/scala/advent2022/day4/input.txt")
    val listBuffer = ListBuffer[String]()
    for (line <- bufferedSource.getLines) {
      listBuffer += line
    }
    listBuffer.toList
  }

  def parseTuples(list: List[String]): List[((Int, Int), (Int, Int))] = {
    val splitList = list.map(str => str.split(","))
    val splitPairs = splitList.map(array => array.map(str => str.split("-")).map(arr => (arr.head.toInt, arr(1).toInt)))
    val listOfTuples = splitPairs.map(arr => (arr.head, arr(1)))
    listOfTuples
  }

  def fullyContains(tuple1: (Int, Int), tuple2: (Int, Int)): Boolean = {
    val lower1 = tuple1._1
    val higher1 = tuple1._2
    val lower2 = tuple2._1
    val higher2 = tuple2._2

    ((lower1 <= lower2 && higher1 >= higher2) || (lower2 <= lower1 && higher2 >= higher1))
  }

  def countFullOverlaps(listOfTuplesOfTuples: List[((Int, Int), (Int, Int))]): Int = {
    var count = 0
    listOfTuplesOfTuples.foreach(
      tupleOfTuples =>
        val tuple1 = tupleOfTuples._1
        val tuple2 = tupleOfTuples._2
        if (fullyContains(tuple1, tuple2)) {
          count += 1
        }
    )
    count
  }

  def part1(): Int = {
    val rawInput = readFile()
    val parsedTuples = parseTuples(rawInput)
    countFullOverlaps(parsedTuples)
  }

  println(part1())

  def partiallyContains(tuple1: (Int, Int), tuple2: (Int, Int)): Boolean = {
    val lower1 = tuple1._1
    val higher1 = tuple1._2
    val lower2 = tuple2._1
    val higher2 = tuple2._2

    (lower1 <= lower2 && lower2 <= higher1) ||
      (lower2 <= lower1 && lower1 <= higher2) ||
      (lower1 <= lower2 && higher1 >= higher2) ||
      (lower2 <= lower1 && higher2 >= higher1)
  }

  def countPartialOverlaps(listOfTuplesOfTuples: List[((Int, Int), (Int, Int))]): Int = {
    var count = 0
    listOfTuplesOfTuples.foreach(
      tupleOfTuples =>
        val tuple1 = tupleOfTuples._1
        val tuple2 = tupleOfTuples._2
        if (partiallyContains(tuple1, tuple2)) {
          count += 1
        }
    )
    count
  }

  def part2(): Int = {
    val rawInput = readFile()
    val parsedTuples = parseTuples(rawInput)
    countPartialOverlaps(parsedTuples)
  }

  println(part2())

}
