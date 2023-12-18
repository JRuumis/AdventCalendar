package jruumis.adventofcode.year2022

import scala.annotation.tailrec

object Day10CRT extends App {

    val commandsInput = scala.io.Source.fromFile("./Sources/Day10CRT.txt").getLines().toVector

    abstract class Command {
        val totalCommandExecutionCycles: Int
        val increaseRegistryBy: Int

        def isAtLastCommandCycleWith(commandCycle: Int): Boolean = commandCycle == totalCommandExecutionCycles
    }

    case class Noop() extends Command {
        override val totalCommandExecutionCycles: Int = 1
        override val increaseRegistryBy: Int = 0
    }

    case class Addx(delta: Int) extends Command {
        override val totalCommandExecutionCycles: Int = 2
        override val increaseRegistryBy: Int = delta
    }

    val addxPattern = "addx (-?[0-9]+)".r
    val parsedCommands: Vector[Command] = commandsInput.map {
        _ match {
            case "noop" => Noop()
            case addxPattern(d) => Addx(d.toInt)
        }
    }

    case class Cycle(cycle: Int, cycleWithinCommand: Int, command: Command, registryValueDuringCycle: Int, registryValueAfterCycle: Int) {
        val signalStrengthDuringCycle: Int = cycle * registryValueDuringCycle
    }

    val zeroCycle: Cycle = Cycle(0, 0, Noop(), 1, 1)

    @tailrec
    def machine(commands: Vector[Command], previousCycle: Cycle = zeroCycle, accuCycles: Vector[Cycle] = Vector()): Vector[Cycle] = commands match {
        case Vector() => accuCycles
        case currentCommand +: rest => {
            val newCycles: Vector[Cycle] = (1 to currentCommand.totalCommandExecutionCycles).toVector.map(commandCycle =>
                Cycle(
                    cycle = previousCycle.cycle + commandCycle,
                    cycleWithinCommand = commandCycle,
                    command = currentCommand,
                    registryValueDuringCycle = previousCycle.registryValueAfterCycle,
                    registryValueAfterCycle =
                        previousCycle.registryValueAfterCycle +
                            (if (currentCommand isAtLastCommandCycleWith commandCycle) currentCommand.increaseRegistryBy else 0)
                ))

            machine(rest, newCycles.last, accuCycles ++ newCycles)
        }
    }

    val cycles: Vector[Cycle] = machine(parsedCommands)
    val selectedCycleIndexes: Vector[Int] = Vector(20, 60, 100, 140, 180, 220)
    val selectedCycles: Vector[Cycle] = selectedCycleIndexes.map(i => cycles(i - 1))
    val selectedCycleSummarySignalStrengthDuring = selectedCycles.map(_.signalStrengthDuringCycle).sum

    println(s"For the cycles ${selectedCycleIndexes.mkString(",")} the summary signal strength during cycle is ${selectedCycleSummarySignalStrengthDuring}")

    val nrOfLines = 6
    val nrOfPixels = 40

    val crtScreenContent =
        (0 to nrOfLines - 1).map { y =>
            (0 to nrOfPixels - 1).map { x =>
                if (
                    x == cycles(x + nrOfPixels * y).registryValueDuringCycle ||
                        x + 1 == cycles(x + nrOfPixels * y).registryValueDuringCycle ||
                        x - 1 == cycles(x + nrOfPixels * y).registryValueDuringCycle
                ) '#' else ' '
            }.mkString
        }

    println(s"\nCRT Screen output:\n${crtScreenContent.mkString("\n")}")
}
