package jruumis.adventofcode.year2022

object Day5SupplyStacks extends App {

    val stacksAndMovesInput: Array[String] = scala.io.Source.fromFile("./Sources/Day5SupplyStacks.txt").getLines().toArray

    val lastStacksLine: Int = 8
    val firstmoveLine: Int = 11
    val numberOfInputStacks: Int = 9
    val inputStackElementPositions = (0 to numberOfInputStacks - 1).map(1 + 4 * _)


    // MOVE
    case class Move(nrOfCrates: Int, fromStack: Int, toStack: Int)

    val moves: List[Move] = stacksAndMovesInput
        .slice(firstmoveLine - 1, stacksAndMovesInput.size)
        .toList
        .map(m => ("""\d+""".r findAllIn m).toList) // parse move strings to extract integers
        .map { case List(crates, from, to) => Move(crates.toInt, from.toInt - 1, to.toInt - 1) }


    // STACK
    case class Stacks(stacks: Array[List[Char]]) {
        def makeMove(
                        move: Move,
                        splitter: (Int, List[Char], List[Char]) => (List[Char], List[Char])
                    ): Stacks = {

            val (cratesToBeMoved, fromStackAfterMove) = splitter(move.nrOfCrates, List(), stacks(move.fromStack))
            val toStackAfterMove = cratesToBeMoved ::: stacks(move.toStack)

            Stacks(
                (0 to stacks.size - 1).map {
                    _ match {
                        case i if i == move.fromStack => fromStackAfterMove
                        case i if i == move.toStack => toStackAfterMove
                        case i => stacks(i)
                    }
                }.toArray
            )
        }

        def topCratesOnStacks(): String = stacks.map(_.head).mkString
    }

    object Stacks {
        // Part One Splitter
        def stackSplitter(splitAt: Int, firstStack: List[Char], secondStack: List[Char]): (List[Char], List[Char]) = (splitAt, secondStack) match {
            case (0, _) => (firstStack, secondStack)
            case (i, s :: rest) if i > 0 => stackSplitter(i - 1, s :: firstStack, rest)
        }

        // Part Two Splitter
        def stackSplitter9001(splitAt: Int, firstStack: List[Char], secondStack: List[Char]): (List[Char], List[Char]) = {
            val (f, s) = stackSplitter(splitAt, firstStack, secondStack)
            (f.reverse, s) // multiple crates moved at once - their order does not change when moved (the original Splitter reverses crates)
        }
    }

    val startStacksInput: Array[List[Char]] = stacksAndMovesInput
        .slice(0, lastStacksLine)
        .map(s => s.padTo(4 * numberOfInputStacks - 1, ' ').toArray) // pad with blanks so that all stack input strings are same length (required for transpose)
        .map(row => inputStackElementPositions.map(a => row(a))) // extract stack elements from stack strings (i.e. remove "[", "]" and extra blanks)
        .map(_.toArray)
        .transpose
        .map(s => s.filter(_ != ' ').toList) // remove blanks so that stacks don't have empty elements

    val startStack = Stacks(startStacksInput)

    // Part One moves
    val endStack = moves.foldLeft(startStack)((stack, move) => stack.makeMove(move, Stacks.stackSplitter))
    val topCratesOnEndStack = endStack.topCratesOnStacks()

    println(s"Top crates on stacks after moves: ${topCratesOnEndStack}")

    // Part Two moves
    val endStack9001 = moves.foldLeft(startStack)((stack, move) => stack.makeMove(move, Stacks.stackSplitter9001))
    val topCratesOnEndStack9001 = endStack9001.topCratesOnStacks()

    println(s"Top crates on stacks after moves (moving with CrateMover 9001): ${topCratesOnEndStack9001}")
}
