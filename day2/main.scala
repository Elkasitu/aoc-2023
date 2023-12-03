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
    val result = Source
      .fromFile("input.txt")
      .getLines
      .map((d) => Game(d))
      .filter(_.isValid)
      .map(_.id.toInt)
      .sum
    println(s"The sum of valid Game IDs is $result")
  }
}

Application.main(Array())
