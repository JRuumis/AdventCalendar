import scala.util.matching.Regex

object Day2CubeConundrum extends App {
    val cubeGamesRaw: Vector[String] = scala.io.Source.fromFile("./Sources/2023/Day2CubeConundrum.txt").getLines().toVector

    case class RGB(red: Int, green: Int, blue: Int) {
        val redMax: Int = 12
        val greenMax: Int = 13
        val blueMax: Int = 14

        val isValid: Boolean = red <= redMax && green <= greenMax && blue <= blueMax
        override def toString: String = s"<red: $red, green: $green, blue:$blue>"

        lazy val power: Int = red * green * blue
        def max (other: RGB): RGB = RGB(this.red max other.red, this.green max other.green, this.blue max other.blue)
    }
    case class Game(gameId: Int, reveals: Vector[RGB]) {
        val validGame: Boolean = reveals.foldLeft(true)((a, b) => a && b.isValid)
        val gameIdIfValid: Int = if(validGame) gameId else 0

        override def toString: String = s"Game $gameId, RGB: $reveals, valid: $validGame, gameIdIfValid: $gameIdIfValid"
    }

    val GamePattern: Regex = """Game ([0-9]+):(.+)""".r
    val RevealPattern: Regex = """([0-9, a-z]+)""".r
    val ColourPattern: Regex = """([0-9]+) ([a-z]+)""".r


    val games: Vector[Game] = cubeGamesRaw.map{ g => GamePattern.findFirstMatchIn(g) match {
        case None => None
        case Some(GamePattern(gameIdString,rest)) => {

            val gameId: Int = Integer.parseInt(gameIdString)

            val revealStrings = RevealPattern.findAllMatchIn(rest).map { revealString =>
                revealString match {
                    case RevealPattern(rev) => Some(rev.trim)
                    case _ => None
                }
            }.filter(_.isDefined).map(_.get).toVector

            val reveals = revealStrings.map(revealString => {
                ColourPattern.findAllMatchIn(revealString).map {
                    _ match {
                        case ColourPattern(count, colour) => Some((Integer.parseInt(count), colour))
                        case _ => None
                    }
                }.filter(_.isDefined).map(_.get).toVector
            })

            val rgbs: Vector[RGB] = reveals.map(reveal => {
                val redCount = reveal.find { case (_, c) => c == "red" } match {
                    case Some((i, _)) => i
                    case _ => 0
                }

                val greenCount = reveal.find { case (_, c) => c == "green" } match {
                    case Some((i, _)) => i
                    case _ => 0
                }

                val blueCount = reveal.find { case (_, c) => c == "blue" } match {
                    case Some((i, _)) => i
                    case _ => 0
                }

                RGB(redCount, greenCount, blueCount)
            })

            Some(Game(gameId, rgbs))
        }
    }}.filter(_.isDefined).map(_.get)

    println(games.mkString("\n"))

    // Part One
    val validSum: Int = games.foldLeft(0)( (a, g) => a + g.gameIdIfValid )
    println(s"valid sum: $validSum")

    // Part Two
    val cubePowers: Int = games.map(g => g.reveals.reduce(_ max _).power).sum
    print(s"Cube powers sum: $cubePowers")

}
