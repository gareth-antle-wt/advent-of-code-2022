import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source

class Node(val name: String, var parent: Node, var children: Map[String, Node], var size: Int)

object Day7 extends App {

  def readFile(): List[String] = {
    val bufferedSource = Source.fromFile("src/main/scala/advent2022/day7/input.txt")
    val listBuffer = ListBuffer[String]()
    for (line <- bufferedSource.getLines) {
      listBuffer += line
    }
    listBuffer.toList
  }

  def buildTree(list: List[String]): Node = {
    val root = Node("/", null, Map[String, Node](), 0)
    var currentNode = root

    list.foreach{ str =>
      str match
        case "$ ls" =>
        case "$ cd /" =>
        case s"$$ cd .." => currentNode = currentNode.parent
        case s"dir $name" => currentNode.children = currentNode.children + (name -> Node(name, currentNode, Map[String, Node](), 0))
        case s"$$ cd $name" => currentNode = currentNode.children(name)
        case s"$size $name" => {
          currentNode.children = currentNode.children + (name -> Node(name, currentNode, null, size.toInt))
          updateSize(size.toInt, currentNode)
        }
    }

    root
  }

  def updateSize(size: Int, parent: Node): Unit = {
    parent.size += size
    if (parent.parent != null) {
      updateSize(size, parent.parent)
    }
  }

  def findDirectoriesWithSize(root: Node, size: Int): Int = {
    var rootSize = 0
    if (root.size <= size) {
      rootSize = root.size
    }
    if (root.children != null) {
      val childrenResults = root.children.values.map(child => findDirectoriesWithSize(child, size))
      childrenResults.sum + rootSize
    } else {
      0
    }
  }

  def part1(): Unit = {
    val input = readFile()
    val root = buildTree(input)
    val largeFiles = findDirectoriesWithSize(root, 100000)
    println(largeFiles)
  }

  part1()

  def getSizeOfAllNodes(root: Node): List[Int] = {
    if (root.children == null) {
      List(root.size)
    } else {
      List(root.size) ++ root.children.values.flatMap(node => getSizeOfAllNodes(node))
    }

  }

  def part2(): Unit = {
    val input = readFile()
    val root = buildTree(input)
    val existingSpace = 70000000 - root.size
    val minSpaceNeeded = 30000000 - existingSpace
    val sizeOfAllNodes = getSizeOfAllNodes(root)
    val nodesWithAtLeastMinSpace = sizeOfAllNodes.filter(size => size >= minSpaceNeeded).sorted
    println(nodesWithAtLeastMinSpace.head)
  }

  part2()

}
