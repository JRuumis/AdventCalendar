package jruumis.adventofcode.year2022

import scala.annotation.tailrec

object Day11MonkeyInTheMiddle_2 extends App {
    val monkeysInput = scala.io.Source.fromFile("./Sources/Day11MonkeyInTheMiddle_TEST.txt")
        .getLines().toVector.filter(_ != "").grouped(6).toVector

    //println(monkeysInput.mkString("\n"))

    type MonkeyId = Int
    type Item = BigInt

    //case class Item(worryLevel: Int) // not sure if unique id is required - don't want deduplication for same worryLevel

    case class Monkey(monkeyId: MonkeyId, items: Vector[Item], itemInspections: BigInt, operation: Item => Item, test: Item => MonkeyId) {
        //override def toString: String = s"Monkey(id: ${monkeyId}, items: ${items.mkString(", ")}, inspections: ${itemInspections})"
        override def toString: String = s"Monkey(id: ${monkeyId}, inspections: ${itemInspections})"

        //def operationWithRelief(i: Item): Item = operation(i) / 3
        def operationWithRelief(i: Item): Item = operation(i) // / 3 // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        def addItem(i: Item): Monkey = Monkey(monkeyId, items :+ i, itemInspections, operation, test)

        def getItem: Option[(Monkey, Item)] = items match {
            case Vector() => None
            case i +: rest => Some(Monkey(monkeyId, rest, itemInspections + 1, operation, test), i)
        }

        @tailrec
        final def monkeysAfterExecuteMonkey(monkeys: Vector[Monkey]): Vector[Monkey] = getItem match {
            case None => monkeys
            case Some((currentMonkeyWithItemRemoved, currentItem)) => {

                val itemAfterOperationWithRelief: Item = operationWithRelief(currentItem)

                val targetMonkeyId: MonkeyId = test(itemAfterOperationWithRelief)

                val newTargetMonkey = monkeys(targetMonkeyId).addItem(itemAfterOperationWithRelief)

                val updatedMonkeys = monkeys
                    .updated(targetMonkeyId, newTargetMonkey) // update Target Monkey in monkeys
                    .updated(monkeyId, currentMonkeyWithItemRemoved) // update this Monkey in monkeys

                currentMonkeyWithItemRemoved.monkeysAfterExecuteMonkey(updatedMonkeys)

                // currentMonkey - update vector with current monkey !!!!!
                // myVec.updated(1, "1")
                // update this monkey in the vector as well...
            }
        }
    }

    def getItemOperation(op: String, arg: String): Item => Item = (op, arg) match {
        case ("+", "old") => ((a: Item) => a + a)
        case ("+", b) => ((a: Item) => a + b.toInt)
        case ("*", "old") => ((a: Item) => a * a)
        case ("*", b) => ((a: Item) => a * b.toInt)
    }

    def getItemTest(divisibleBy: BigInt, toMonkeyIfTrue: MonkeyId, toMonkeyIfFalse: MonkeyId)(worryLevel: BigInt): MonkeyId =
        if (worryLevel % divisibleBy == 0) toMonkeyIfTrue else toMonkeyIfFalse

    val monkeyPattern = "Monkey ([0-9]+):".r
    val startingItemsPattern = "  Starting items: ([, 0-9]+)".r
    val operationPattern = "  Operation: new = old ([+*]) (old|[0-9]+)".r
    val testPattern = "  Test: divisible by ([0-9]+)".r
    val ifTruePattern = "    If true: throw to monkey ([0-9]+)".r
    val ifFalsePattern = "    If false: throw to monkey ([0-9]+)".r

    val startMonkeys: Vector[Monkey] = monkeysInput.map(a => a match {
        case (
            monkeyPattern(monkeyId) +:
                startingItemsPattern(startingItems) +:
                operationPattern(itemOperation, arg) +:
                testPattern(itemForTest) +:
                ifTruePattern(monkeyIfTrue) +:
                ifFalsePattern(monkeyIfFalse) +:
                Vector()
            ) => Monkey(
            monkeyId = monkeyId.toInt,
            items = startingItems.split(", ").map(i => BigInt(i.toLong)).toVector,
            itemInspections = 0,
            operation = getItemOperation(itemOperation, arg),
            test = getItemTest(itemForTest.toLong, monkeyIfTrue.toInt, monkeyIfFalse.toInt)
        )
    })

    println("Start monkeys:")
    println(startMonkeys.mkString("\n"))

    case class Round(roundId: Int, monkeys: Vector[Monkey]) {

        @tailrec
        final def executeRound(
                                  monkeyIds: List[MonkeyId] = (0 to monkeys.size - 1).toList,
                                  currentMonkeys: Vector[Monkey] = monkeys
                              ): Round = monkeyIds match {
            case Nil => Round(roundId + 1, currentMonkeys)
            case i :: rest => executeRound(rest, currentMonkeys(i).monkeysAfterExecuteMonkey(currentMonkeys))
        }

        //@tailrec
        final def executeManyRounds(i: Int, accuRounds: Vector[Round] = Vector(this)): Vector[Round] = i match {
            case 0 => accuRounds
            case i if i > 0 => {
                val newRound: Round = this.executeRound()
                newRound.executeManyRounds(i - 1, accuRounds :+ newRound)
            }
        }

        final def executeManyRoundsRememberLast(i: Int, lastRound: Round = this): Round = i match {
            case 0 => lastRound
            case i if i > 0 => {
                val newRound: Round = this.executeRound()

                println(s"\t${newRound}")

                newRound.executeManyRoundsRememberLast(i - 1, newRound)
            }
        }

    }

    /*
    object Round {


        def getMonkeyInspections(monkeyRounds: Vector[Round]) = {



            //val monkeys: Vector[Monkey] = monkeyRounds(0).monkeys

            val xxx = monkeyRounds.dropRight(1).map(x =>
                x.monkeys.map(m => (m.monkeyId, m.items.size))
            )

            val yyy = xxx.transpose.map(mv => ( mv(0)._1, mv.map(_._2).sum ))

            yyy


        }
    }

     */


    val roundZero: Round = Round(0, startMonkeys)


    // Part 1
    //val monkeyVectors20 = roundZero.executeManyRounds(20)

    //println
    //println(monkeyVectors20.mkString("\n"))

    //val ccc = Round.getMonkeyInspections(monkeyVectors20)
    //println(ccc)

    //val lastRound = monkeyVectors20.last
    //val top2ActiveMonkeyInspectionProduct = lastRound.monkeys.map(_.itemInspections).sorted.takeRight(2).reduce(_ * _)

    //println(top2ActiveMonkeyInspectionProduct)


    // Part 2
    val monkeyVectors10000 = roundZero.executeManyRoundsRememberLast(1000)

    val lastRound10000 = monkeyVectors10000 //.last

    println(lastRound10000)

    val top2ActiveMonkeyInspectionProduct10000 = lastRound10000.monkeys.map(_.itemInspections.toLong).sorted.takeRight(2).reduce(_ * _)

    println(top2ActiveMonkeyInspectionProduct10000)


}
