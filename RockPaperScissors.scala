package advent2022.day2

import scala.collection.mutable.ListBuffer
import scala.io.Source

object RockPaperScissors extends App {

  def part1(): Int = {
    var score = 0
    val bufferedSource = Source.fromFile("src/main/scala/advent2022/day2/input.txt")
    for (line <- bufferedSource.getLines) {
      line match
        case "A X" => score += (1 + 3) // Rock Rock, Tie
        case "B X" => score += (1 + 0) // Paper Rock, Loss
        case "C X" => score += (1 + 6) // Scissor Rock, Win

        case "A Y" => score += (2 + 6) // Rock Paper, Win
        case "B Y" => score += (2 + 3) // Paper Paper, Tie
        case "C Y" => score += (2 + 0) // Scissors Paper, loss

        case "A Z" => score += (3 + 0) // Rock Scissors, Loss
        case "B Z" => score += (3 + 6) // Paper Scissors, Win
        case "C Z" => score += (3 + 3) // Scissors Scissors, Tie
    }
    score
  }

  println(part1())

  def part2(): Int = {
    var score = 0
    val bufferedSource = Source.fromFile("src/main/scala/advent2022/day2/input.txt")
    for (line <- bufferedSource.getLines) {
      line match
        case "A X" => score += (3 + 0) // Rock Loose, play Scissors
        case "B X" => score += (1 + 0) // Paper Loose, play Rock
        case "C X" => score += (2 + 0) // Scissors Loose, play Paper

        case "A Y" => score += (1 + 3) // Rock Tie, play Rock
        case "B Y" => score += (2 + 3) // Paper Tie, play Paper
        case "C Y" => score += (3 + 3) // Scissors Tie, play Scissors

        case "A Z" => score += (2 + 6) // Rock Win, play paper
        case "B Z" => score += (3 + 6) // Paper win, play Scissors
        case "C Z" => score += (1 + 6) // Scissors Win, play rock
    }
    score
  }

  println(part2())

}
