import scala.io.Source

val MaxRed = 12
val MaxGreen = 13
val MaxBlue = 14

case class CubeSet(red: Int, green: Int, blue: Int) {
  def isValid =
    red <= MaxRed &&
    green <= MaxGreen &&
    blue <= MaxBlue
}

object CubeSet {
  def apply(s: String): CubeSet = {
    val colors = s
      .split(",")
      .map(_.trim.split(" ").toArray)
    val red = colors map { 
      case Array(num, "red") => num.toInt
      case _ => 0
    }
    val green = colors map {
      case Array(num, "green") => num.toInt
      case _ => 0
    }
    val blue = colors map {
      case Array(num, "blue") => num.toInt
      case _ => 0
    }
    new CubeSet(red.max, green.max, blue.max)
  }

}

case class Game(id: Int, cubeSets: Array[CubeSet]) {
  def isValid = cubeSets.forall(_.isValid)
  def maxRed = cubeSets.maxBy(_.red).red
  def maxGreen = cubeSets.maxBy(_.green).green
  def maxBlue = cubeSets.maxBy(_.blue).blue
  def power = maxRed * maxGreen * maxBlue
}

object Game {
  def apply(s: String): Game = {
    val contents = s.split(":")
    val id = contents(0)
      .split(" ")(1)
      .toInt
    val cubeSets = contents(1)
      .split(";")
      .map((d) => CubeSet(d))
      .toArray
    new Game(id, cubeSets)
  }
}

object Application {
  def main(args: Array[String]): Unit = {
    val source = Source.fromFile("input.txt")
    val games = source.getLines.map((d) => Game(d)).toArray
    val result1 = games
      .filter(_.isValid)
      .map(_.id.toInt)
      .sum
    val result2 = games
      .map(_.power)
      .sum
    println(s"The sum of valid Game IDs is $result1, the sum of power is $result2")
    source.close()
  }
}

Application.main(Array())
