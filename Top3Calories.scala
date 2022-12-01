package advent2022.day1

import scala.collection.mutable.ListBuffer
import scala.io.Source

object Top3Calories extends App {

  def readFromFile(): List[List[Int]] = {
    val listBuffer = ListBuffer[List[Int]]()
    val bufferedSource = Source.fromFile("src/main/scala/advent2022/day1/input.txt")
    var elfBuffer: ListBuffer[Int] = ListBuffer[Int]()
    for (line <- bufferedSource.getLines) {
      line match
        case "" =>
          listBuffer += elfBuffer.toList
          elfBuffer = ListBuffer[Int]()
        case _ => elfBuffer += line.toInt
    }
    bufferedSource.close()
    val list = listBuffer.toList
    list
  }

  val listOfListOfCalories = readFromFile()
  val sortedListOfCalories = listOfListOfCalories.map(list => list.sum).sorted(Ordering.Int.reverse)
  println(sortedListOfCalories.head)
  println(sortedListOfCalories(0) + sortedListOfCalories(1) +sortedListOfCalories(2))

}
