import scala.collection.mutable.ListBuffer
import scala.io.Source

object Day8 extends App{

  def readFile(): List[List[(Int, Boolean)]] = {
    val bufferedSource = Source.fromFile("src/main/scala/advent2022/day8/input.txt")
    val outerListBuffer = ListBuffer[List[(Int, Boolean)]]()
    for (line <- bufferedSource.getLines) {
      val innerListBuffer = ListBuffer[(Int, Boolean)]()
      line.foreach{char =>
        val newTuple = (char.toString.toInt, false)
        innerListBuffer += newTuple}
      outerListBuffer += innerListBuffer.toList
    }
    outerListBuffer.toList
  }

  def setOuterTreesAsVisible(matrix: List[List[(Int, Boolean)]]): List[List[(Int, Boolean)]] = {
    var visibilityMatrix = matrix
    val width = matrix.head.length
    val height = matrix.length

    // setup visibility on the outside and go from top down
    for (y <- 0 until height) {
      for (x <- 0 until width) {
        var row = visibilityMatrix(y)
        val currentTuple = row(x)
        if (x == 0 || y == 0 || x == width - 1 || y == height - 1) {
          row = row.updated(x, (currentTuple._1, true))
          visibilityMatrix = visibilityMatrix.updated(y, row)
        } else {
          if (isVisible(visibilityMatrix, x, y)) {
            row = row.updated(x, (currentTuple._1, true))
            visibilityMatrix = visibilityMatrix.updated(y, row)
          }
        }
      }
    }

    visibilityMatrix
  }

  def isVisible(matrix: List[List[(Int, Boolean)]], x: Int, y: Int): Boolean = {

    val visibleNorth = searchNorth(matrix, x, y)
    val visibleSouth = searchSouth(matrix, x, y)
    val visibleEast = searchEast(matrix, x, y)
    val visibleWest = searchWest(matrix, x, y)

    visibleNorth || visibleSouth || visibleEast || visibleWest
  }

  def searchNorth(matrix: List[List[(Int, Boolean)]], startingX: Int, startingY: Int): Boolean = {
    var visible = true
    val thisTree = matrix(startingY)(startingX)
    for (y <- startingY - 1 until -1 by -1) {
      val compTree = matrix(y)(startingX)
      if (compTree._1 >= thisTree._1) {
        visible = false
      }
    }
    visible
  }

  def searchSouth(matrix: List[List[(Int, Boolean)]], startingX: Int, startingY: Int): Boolean = {
    var visible = true
    val thisTree = matrix(startingY)(startingX)
    for (y <- startingY + 1 until matrix.length) {
      val compTree = matrix(y)(startingX)
      if (compTree._1 >= thisTree._1) {
        visible = false
      }
    }
    visible
  }

  def searchEast(matrix: List[List[(Int, Boolean)]], startingX: Int, startingY: Int): Boolean = {
    var visible = true
    val thisTree = matrix(startingY)(startingX)
    for (x <- startingX - 1 until -1 by -1) {
      val compTree = matrix(startingY)(x)
      if (compTree._1 >= thisTree._1) {
        visible = false
      }
    }
    visible
  }

  def searchWest(matrix: List[List[(Int, Boolean)]], startingX: Int, startingY: Int): Boolean = {
    var visible = true
    val thisTree = matrix(startingY)(startingX)
    for (x <- startingX + 1 until matrix.head.length) {
      val compTree = matrix(startingY)(x)
      if (compTree._1 >= thisTree._1) {
        visible = false
      }
    }
    visible
  }

  def calculateVisbility(matrix: List[List[(Int, Boolean)]]): List[List[(Int, Boolean)]] = {
    val matrixWithOuterTreesVisible = setOuterTreesAsVisible(matrix)
    matrixWithOuterTreesVisible
  }

  def countVisible(matrix: List[List[(Int, Boolean)]]): Int = {
    val width = matrix.head.length
    val height = matrix.length
    var count = 0

    for (y <- 0 until height) {
      for (x <- 0 until width) {
        if (matrix(y)(x)._2) {
          count += 1
        }
      }
    }
    count
  }

  def part1(): Unit = {
    val initialMatrix = readFile()
    val visibilityMatrix = calculateVisbility(initialMatrix)
    println(countVisible(visibilityMatrix))
  }

  part1()

  def readFilePart2(): List[List[Int]] = {
    val bufferedSource = Source.fromFile("src/main/scala/advent2022/day8/input.txt")
    val outerListBuffer = ListBuffer[List[Int]]()
    for (line <- bufferedSource.getLines) {
      val innerListBuffer = ListBuffer[Int]()
      line.foreach { char =>
        innerListBuffer += char.toString.toInt
      }
      outerListBuffer += innerListBuffer.toList
    }
    outerListBuffer.toList
  }

  def findNumVisibleTrees(matrix: List[List[Int]], x: Int, y: Int): Int = {
    numVisibleTreesNorth(matrix, x, y) * numVisibleTreesSouth(matrix, x, y) * numVisibleTreesEast(matrix, x, y) * numVisibleTreesWest(matrix, x, y)
  }

  def numVisibleTreesNorth(matrix: List[List[Int]], startingX: Int, startingY: Int): Int = {
    var visible = 0
    val thisTreeHeight = matrix(startingY)(startingX)
    for (y <- startingY - 1 until -1 by -1) {
      visible += 1
      val compTreeHeight = matrix(y)(startingX)
      if (compTreeHeight >= thisTreeHeight) {
        return visible
      }
    }
    visible
  }

  def numVisibleTreesSouth(matrix: List[List[Int]], startingX: Int, startingY: Int): Int = {
    var visible = 0
    val thisTreeHeight = matrix(startingY)(startingX)
    for (y <- startingY + 1 until matrix.length) {
      visible += 1
      val compTreeHeight = matrix(y)(startingX)
      if (compTreeHeight >= thisTreeHeight) {
        return visible
      }
    }
    visible
  }

  def numVisibleTreesEast(matrix: List[List[Int]], startingX: Int, startingY: Int): Int = {
    var visible = 0
    val thisTreeHeight = matrix(startingY)(startingX)
    for (x <- startingX + 1 until matrix.head.length) {
      visible += 1
      val compTreeHeight = matrix(startingY)(x)
      if (compTreeHeight >= thisTreeHeight) {
        return visible
      }
    }
    visible
  }

  def numVisibleTreesWest(matrix: List[List[Int]], startingX: Int, startingY: Int): Int = {
    var visible = 0
    val thisTreeHeight = matrix(startingY)(startingX)
    for (x <- startingX - 1 until -1 by -1) {
      visible += 1
      val compTreeHeight = matrix(startingY)(x)
      if (compTreeHeight >= thisTreeHeight) {
        return visible
      }
    }
    visible
  }

  def findMostTreesVisible(matrix: List[List[Int]]): Int = {
    var mostTreesVisible = 0
    val width = matrix.head.length
    val height = matrix.length

    for (y <- 0 until height) {
      for (x <- 0 until width) {
        val thisTreeVisible = findNumVisibleTrees(matrix, x, y)
        if (thisTreeVisible > mostTreesVisible) {
          mostTreesVisible = thisTreeVisible
        }
      }
    }
    mostTreesVisible
  }

  def part2(): Unit = {
    val initialMatrix = readFilePart2()
    println(findMostTreesVisible(initialMatrix))
  }

  part2()

}
