import scala.collection.mutable.ListBuffer
import scala.io.Source

object Day3 extends App {
  val priority = List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's',
    't', 'u', 'v', 'w', 'x', 'y', 'z', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O',
    'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z')

  def readFile(): List[String] = {
    val bufferedSource = Source.fromFile("src/main/scala/advent2022/day3/input.txt")
    val listBuffer = ListBuffer[String]()
      for (line <- bufferedSource.getLines) {
        listBuffer += line
      }
    listBuffer.toList
  }

  def splitLines(list: List[String]): List[(String, String)]= {
    list.map(line => line.splitAt(line.length / 2))
  }

  def findIntersection(listOfTuples: List[(String, String)]): List[String] = {
    listOfTuples.map(tuple => tuple._1.filter(char => tuple._2.contains(char)))
  }

  def filterIntersection(listOfIntersections: List[String]): List[Char] = {
    listOfIntersections.map(str => str.head)
  }

  def priorities(list: List[Char]): List[Int] = {
    list.map(str => priority.indexOf(str) + 1)
  }

  def part1(): Int = {
    val inputList = readFile()
    val splitList = splitLines(inputList)
    val intersectionList = findIntersection(splitList)
    val filteredIntersectionList = filterIntersection(intersectionList)
    val listOfPriorities = priorities(filteredIntersectionList)
    listOfPriorities.sum
  }

  println(part1())

  def parseGroups(list: List[String]): List[(String, String, String)] = {
    if (list.size < 3) return Nil
    (list(0), list(1), list(2)) :: parseGroups(list drop 3)
  }

  def findCommonElements(list: List[(String, String, String)]): List[Char] = {
    list.map {
      triple =>
        val intersection1 = triple._1.toSet.intersect(triple._2.toSet)
        intersection1.intersect(triple._3.toSet).head
    }
  }

  def part2(): Int = {
    val inputList = readFile()
    val listOfGroups = parseGroups(inputList)
    val commonElements = findCommonElements(listOfGroups)
    val listOfPriorities = priorities(commonElements)
    listOfPriorities.sum
  }

  println(part2())

}
