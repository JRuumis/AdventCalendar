package jruumis.adventofcode.year2022

import scala.util.matching.Regex

object Day21MonkeyMath extends App {

    val monkeyMathInputRaw: Vector[String] = scala.io.Source.fromFile("./Sources/Day21MonkeyMath.txt").getLines().toVector

    val numberPattern: Regex = "([a-z][a-z][a-z][a-z]): ([0-9]+)".r
    val aritmOpPattern: Regex = """([a-z][a-z][a-z][a-z]): ([a-z][a-z][a-z][a-z]) ([+\-*/]) ([a-z][a-z][a-z][a-z])""".r

    type MonkeyName = String
    type AritmOp = Char

    abstract class Monkey {
        val monkey: MonkeyName
    }

    case class MonkeyWithNumber(monkey: MonkeyName, number: Long) extends Monkey

    case class MonkeyWithOp(monkey: MonkeyName, sourceMonkey1: MonkeyName, aritmOp: AritmOp, sourceMonkey2: MonkeyName) extends Monkey

    val monkeyMathParsed: Vector[Monkey] = monkeyMathInputRaw.map(a => a match {
        case numberPattern(m, n) => MonkeyWithNumber(m, n.toLong)
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
                def opResult: Long = aritmOp match {
                    case '+' => monkeysWithNumbers(sourceMonkey1).number + monkeysWithNumbers(sourceMonkey2).number
                    case '-' => monkeysWithNumbers(sourceMonkey1).number - monkeysWithNumbers(sourceMonkey2).number
                    case '*' => monkeysWithNumbers(sourceMonkey1).number * monkeysWithNumbers(sourceMonkey2).number
                    case '/' => monkeysWithNumbers(sourceMonkey1).number / monkeysWithNumbers(sourceMonkey2).number
                }

                val newMonkeysWithNumbers: Map[MonkeyName, MonkeyWithNumber] = monkeysWithNumbers + (monkey -> MonkeyWithNumber(monkey, opResult))
                val newMonkeysWithOps = rest
                (newMonkeysWithNumbers, newMonkeysWithOps)
            } else {
                (monkeysWithNumbers, rest :+ mo)
            }

            getMonkeyNumbers(mwn, mwo, pass + 1)
        }
    }

    val xxx = getMonkeyNumbers(monkeysWithNumbersMap, monkeysWithOps, 0)

    println(xxx("root"))


}
