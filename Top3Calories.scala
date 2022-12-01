package advent2022.day1

import scala.collection.mutable.ListBuffer
import scala.io.Source

object Top3Calories extends App {

  def readFromFile(): List[Int] = {
    val listBuffer = ListBuffer[Int]()
    val bufferedSource = Source.fromFile("src/main/scala/advent2022/day1/input.txt")
    var calorieCount = 0
    for (line <- bufferedSource.getLines) {
      line match
        case "" =>
          listBuffer += calorieCount
          calorieCount = 0
        case str: String => calorieCount += str.toInt
    }
    bufferedSource.close()
    val list = listBuffer.toList
    list
  }

  val listOfListOfCalories = readFromFile()
  val sortedListOfCalories = listOfListOfCalories.sorted(Ordering.Int.reverse)
  println(sortedListOfCalories.head)
  println(sortedListOfCalories(0) + sortedListOfCalories(1) +sortedListOfCalories(2))

}
