import scala.io.Source


val WordDigits = "one,two,three,four,five,six,seven,eight,nine"
val Digits = "1,2,3,4,5,6,7,8,9"

case class Digit(value: String, index: Int)

def toValue(digit: Digit): String = {
  val i = WordDigits.split(",").indexOf(digit.value)
  if (i == -1)
    digit.value
  else
    Digits.split(",")(i).toString()
}

def getAllIndexes(source: String, term: String): Array[Digit] = {
  term.r.findAllMatchIn(source).map((d) => Digit(term, d.start)).toArray
}

def getDigits(s: String): String = {
  val res: Array[Array[Digit]] = WordDigits
    .split(",")
    .map((d) => getAllIndexes(s, d))
    .filter(_.nonEmpty) ++ Digits
      .split(",")
      .map((d) => getAllIndexes(s, d))
      .filter(_.nonEmpty)
  toValue(res.flatten.minBy(_.index)) + toValue(res.flatten.maxBy(_.index))
}

def getDigitsInt(s: String): Int = {
  getDigits(s).toInt
}

def sumInts(s: String): Int = {
  s.split("\n").map(getDigitsInt(_)).sum
}

object Application {
  def main(args: Array[String]): Unit = {
    val source = Source.fromFile("input.txt")
    val text = try source.mkString finally source.close()
    val sum = sumInts(text)
    println(s"The total sum is $sum")
  }
}

Application.main(Array())
