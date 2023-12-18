package jruumis.adventofcode.year2022

import scala.math.BigDecimal
import scala.util.matching.Regex

object Day21MonkeyMath2 extends App {

    val monkeyMathInputRaw: Vector[String] = scala.io.Source.fromFile("./Sources/Day21MonkeyMath.txt").getLines().toVector

    val numberPattern: Regex = "([a-z][a-z][a-z][a-z]): ([0-9]+)".r
    val aritmOpPattern: Regex = """([a-z][a-z][a-z][a-z]): ([a-z][a-z][a-z][a-z]) ([+\-*/]) ([a-z][a-z][a-z][a-z])""".r

    type MonkeyName = String
    type AritmOp = Char

    case class MonkeyNumber(a: BigDecimal, b: BigDecimal) {
        override def toString: String = s"${a}X + ${b}"

        def +(other: MonkeyNumber): MonkeyNumber = MonkeyNumber(a + other.a, b + other.b)

        def -(other: MonkeyNumber): MonkeyNumber = MonkeyNumber(a - other.a, b - other.b)

        def *(other: MonkeyNumber): MonkeyNumber = (a, other.a) match {
            case (x, y) if x == BigDecimal(0) && y == BigDecimal(0) => MonkeyNumber(0, b * other.b)
            case (x, otherA) if x == BigDecimal(0) => MonkeyNumber(otherA * b, other.b * b)
            case (thisA, x) if x == BigDecimal(0) => MonkeyNumber(thisA * other.b, b * other.b)
            case (x, y) => {
                println("ERROR - multi")
                MonkeyNumber(-1, -1)
            }
        }

        def /(other: MonkeyNumber): MonkeyNumber = (a, other.a) match {
            case (x, y) if x == BigDecimal(0) && y == BigDecimal(0) => MonkeyNumber(0, b / other.b)
            case (x, otherA) if x == BigDecimal(0) => {
                println("ERROR - div1")
                MonkeyNumber(-1, -1)
            }
            case (thisA, x) if x == BigDecimal(0) => MonkeyNumber(thisA / other.b, b / other.b)
            case (x, y) => {
                println("ERROR - div2")
                MonkeyNumber(-1, -1)
            }
        }
    }


    abstract class Monkey {
        val monkey: MonkeyName
    }

    case class MonkeyWithNumber(monkey: MonkeyName, number: MonkeyNumber) extends Monkey

    case class MonkeyWithOp(monkey: MonkeyName, sourceMonkey1: MonkeyName, aritmOp: AritmOp, sourceMonkey2: MonkeyName) extends Monkey

    val monkeyMathParsed: Vector[Monkey] = monkeyMathInputRaw.map(a => a match {
        case numberPattern(m, n) => m match {
            case "humn" => MonkeyWithNumber(m, MonkeyNumber(1, 0))
            case _ => MonkeyWithNumber(m, MonkeyNumber(0, n.toDouble))
        }
        case aritmOpPattern(m, m1, op, m2) => MonkeyWithOp(m, m1, op.head, m2)
    })

    //println(monkeyMathParsed.mkString("\n"))

    val monkeysWithNumbers: Vector[MonkeyWithNumber] = monkeyMathParsed.filter(a => a.isInstanceOf[MonkeyWithNumber]).map(_.asInstanceOf[MonkeyWithNumber])
    val monkeysWithOps: Vector[MonkeyWithOp] = monkeyMathParsed.filter(a => a.isInstanceOf[MonkeyWithOp]).map(_.asInstanceOf[MonkeyWithOp])

    val monkeysWithNumbersMap: Map[MonkeyName, MonkeyWithNumber] = monkeysWithNumbers.map(m => (m.monkey -> m)).toMap

    //println(monkeysWithNumbers)
    //println(monkeysWithOps)


    def getMonkeyNumbers(
                            monkeysWithNumbers: Map[MonkeyName, MonkeyWithNumber],
                            monkeysWithOps: Vector[MonkeyWithOp],
                            pass: Int
                        ): Map[MonkeyName, MonkeyWithNumber] = monkeysWithOps match {
        case Vector() => monkeysWithNumbers
        case (mo@MonkeyWithOp(monkey, sourceMonkey1, aritmOp, sourceMonkey2)) +: rest => {

            val (mwn, mwo) = if (monkeysWithNumbers.get(sourceMonkey1).isDefined && monkeysWithNumbers.get(sourceMonkey2).isDefined) {
                def opResult: MonkeyNumber = aritmOp match {
                    case '+' => monkeysWithNumbers(sourceMonkey1).number + monkeysWithNumbers(sourceMonkey2).number
                    case '-' => monkeysWithNumbers(sourceMonkey1).number - monkeysWithNumbers(sourceMonkey2).number
                    case '*' => monkeysWithNumbers(sourceMonkey1).number * monkeysWithNumbers(sourceMonkey2).number
                    case '/' => monkeysWithNumbers(sourceMonkey1).number / monkeysWithNumbers(sourceMonkey2).number
                }

                if (monkey == "root") {
                    println(s"${monkeysWithNumbers(sourceMonkey1).number} === ${monkeysWithNumbers(sourceMonkey2).number}")

                    val n1 = monkeysWithNumbers(sourceMonkey1).number
                    val n2 = monkeysWithNumbers(sourceMonkey2).number

                    val xxx = (n2.b - n1.b) / n1.a
                    println(xxx)
                } else {}

                val newMonkeysWithNumbers: Map[MonkeyName, MonkeyWithNumber] = monkeysWithNumbers + (monkey -> MonkeyWithNumber(monkey, opResult))
                val newMonkeysWithOps = rest
                (newMonkeysWithNumbers, newMonkeysWithOps)
            } else {
                (monkeysWithNumbers, rest :+ mo)
            }

            //println(s"> ${mwn}")

            getMonkeyNumbers(mwn, mwo, pass + 1)
        }
    }

    val xxx = getMonkeyNumbers(monkeysWithNumbersMap, monkeysWithOps, 0)

    println("=============")
    println(xxx("root"))

}
