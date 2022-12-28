import scala.annotation.tailrec

object Day11MonkeyInTheMiddle extends App {

    // -= Item =-
    // Item used to be `type Item = Long` for Part 1.
    class Item(val divisions: Map[Int, Int]) {
        override def toString: String = s"Item(${divisions})"

        // Part 2 logic:
        // We just keep (divisor, reminder) pairs.
        // When adding or multiplying, we can add and multiply just the reminders for each of the divisors.
        // Thus, we can go from Monkey to Monkey, who have different divisors each.

        // with Item
        def +(that: Item): Item = {
            val newDivisions = divisions map { case (divisor, reminder) => (divisor, (reminder + that.divisions(divisor)) % divisor) }
            Item(newDivisions)
        }

        def *(that: Item): Item = {
            val newDivisions = divisions map { case (divisor, reminder) => (divisor, (reminder * that.divisions(divisor)) % divisor) }
            Item(newDivisions)
        }

        // with Int
        def +(that: Int): Item = {
            val newDivisions = divisions map { case (divisor, reminder) => (divisor, (reminder + that) % divisor) }
            Item(newDivisions)
        }

        def *(that: Int): Item = {
            val newDivisions = divisions map { case (divisor, reminder) => (divisor, (reminder * that) % divisor) }
            Item(newDivisions)
        }

        def divisibleBy(divisor: Int): Boolean = divisions(divisor) == 0
    }

    object Item {
        // for init only
        def apply(initValue: Integer, divisors: Vector[Int]): Item = {
            val divisions: Map[Int, Int] = divisors.map(divisor =>
                (divisor, initValue % divisor)
            ).toMap

            new Item(divisions)
        }

        def apply(divisions: Map[Int, Int]): Item = {
            new Item(divisions)
        }
    }


    // -= Monkey =-
    type MonkeyId = Int

    case class Monkey(monkeyId: MonkeyId, items: Vector[Item], itemInspections: Long, operation: Item => Item, test: Item => MonkeyId, relief: Item => Item) {

        //override def toString: String = s"Monkey(id: ${monkeyId}, items: ${items.mkString(", ")}, inspections: ${itemInspections})"
        override def toString: String = s"Monkey(id: ${monkeyId}, inspections: ${itemInspections})"

        //def operationWithRelief(i: Item): Item = operation(i) / 3 < --- this we can no longer do
        def operationWithRelief(i: Item): Item = relief(operation(i))

        def addItem(i: Item): Monkey = Monkey(monkeyId, items :+ i, itemInspections, operation, test, relief)

        def getItem: Option[(Monkey, Item)] = items match {
            case Vector() => None
            case i +: rest => Some(Monkey(monkeyId, rest, itemInspections + 1, operation, test, relief), i)
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
            }
        }
    }

    // -= Round =-
    case class Round(roundId: Int, monkeys: Vector[Monkey]) {

        @tailrec
        final def executeRound(
                                  monkeyIds: List[MonkeyId] = (0 to monkeys.size - 1).toList,
                                  currentMonkeys: Vector[Monkey] = monkeys
                              ): Round = monkeyIds match {
            case Nil => Round(roundId + 1, currentMonkeys)
            case i :: rest => executeRound(rest, currentMonkeys(i).monkeysAfterExecuteMonkey(currentMonkeys))
        }

        @tailrec
        final def executeManyRoundsAccumulate(i: Int, accuRounds: Vector[Round] = Vector(this)): Vector[Round] = i match {
            case 0 => accuRounds
            case i if i > 0 => {
                val newRound: Round = this.executeRound()
                newRound.executeManyRoundsAccumulate(i - 1, accuRounds :+ newRound)
            }
        }

        @tailrec
        final def executeManyRoundsRememberLast(i: Int, lastRound: Round = this): Round = i match {
            case 0 => lastRound
            case i if i > 0 => {
                val newRound: Round = this.executeRound()
                newRound.executeManyRoundsRememberLast(i - 1, newRound)
            }
        }
    }

    // -= Parsers =-
    def getItemOperation(op: String, arg: String): Item => Item = (op, arg) match {
        case ("+", "old") => ((a: Item) => a + a)
        case ("+", b) => ((a: Item) => a + b.toInt)
        case ("*", "old") => ((a: Item) => a * a)
        case ("*", b) => ((a: Item) => a * b.toInt)
    }

    def getItemTest(divisor: Int, toMonkeyIfTrue: MonkeyId, toMonkeyIfFalse: MonkeyId)(worryLevel: Item): MonkeyId =
        if (worryLevel divisibleBy divisor) toMonkeyIfTrue else toMonkeyIfFalse

    val monkeyPattern = "Monkey ([0-9]+):".r
    val startingItemsPattern = "  Starting items: ([, 0-9]+)".r
    val operationPattern = "  Operation: new = old ([+*]) (old|[0-9]+)".r
    val testPattern = "  Test: divisible by ([0-9]+)".r
    val ifTruePattern = "    If true: throw to monkey ([0-9]+)".r
    val ifFalsePattern = "    If false: throw to monkey ([0-9]+)".r


    // -= MAIN =-
    val monkeysInput = scala.io.Source.fromFile("./Sources/Day11MonkeyInTheMiddle.txt")
        .getLines().toVector.filter(_ != "").grouped(6).toVector

    val startMonkeysRaw = monkeysInput.map(a => a match {
        case (
            monkeyPattern(monkeyId) +:
                startingItemsPattern(startingItems) +:
                operationPattern(itemOperation, arg) +:
                testPattern(itemForTest) +:
                ifTruePattern(monkeyIfTrue) +:
                ifFalsePattern(monkeyIfFalse) +:
                Vector()
            ) => (
            itemForTest.toInt,
            monkeyId.toInt,
            startingItems.split(", ").map(i => i.toInt).toVector,
            itemOperation,
            arg,
            monkeyIfTrue.toInt,
            monkeyIfFalse.toInt
        )
    })

    val divisors: Vector[Int] = startMonkeysRaw.map { case (d, _, _, _, _, _, _) => d } // extract the test divisors from all Monkeys - to be added to all Items

    // -= Part 1 =-
    // Part 1 can only be executed with an older version of this code :-(


    // -= Part 2 =-
    val part2relief: Item => Item = (i: Item) => i // this is kind of redundant because my Part 2 code does not support a relief function that divides

    val startMonkeys = startMonkeysRaw.map { case (itemForTest, monkeyId, startItems, itemOperation, arg, monkeyIfTrue, monkeyIfFalse) =>
        Monkey(monkeyId, startItems.map(i => Item(i, divisors)), 0, getItemOperation(itemOperation, arg), getItemTest(itemForTest, monkeyIfTrue, monkeyIfFalse), part2relief)
    }

    val roundZero: Round = Round(0, startMonkeys)
    val lastRound10000 = roundZero.executeManyRoundsRememberLast(10000)
    val top2ActiveMonkeyInspectionProduct10000 = lastRound10000.monkeys.map(_.itemInspections.toLong).sorted.takeRight(2).reduce(_ * _)

    println(s"After 10000 Rounds, the product of top 2 Monkey Inspections is ${top2ActiveMonkeyInspectionProduct10000}")
}
