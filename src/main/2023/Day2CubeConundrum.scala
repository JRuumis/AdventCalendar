import scala.util.matching.Regex

object Day2CubeConundrum extends App {
    // Cube RGB
    case class CubeReveal(red: Int, green: Int, blue: Int) {
        lazy val isValid: Boolean = red <= CubeReveal.redMax && green <= CubeReveal.greenMax && blue <= CubeReveal.blueMax
        lazy val cubePower: Int = red * green * blue

        def minRequiredCubes(other: CubeReveal): CubeReveal = CubeReveal(this.red max other.red, this.green max other.green, this.blue max other.blue)

        override def toString: String = s"<red: $red, green: $green, blue:$blue>"
    }
    object CubeReveal {
        val redMax: Int = 12
        val greenMax: Int = 13
        val blueMax: Int = 14
    }

    // Game with ID and multiple Cube Reveals (RGBs)
    case class Game(gameId: Int, cubeReveals: Vector[CubeReveal]) {
        lazy val validGame: Boolean = cubeReveals.foldLeft(true)((a, b) => a && b.isValid)
        lazy val gameIdIfValid: Int = if(validGame) gameId else 0

        override def toString: String = s"Game $gameId, RGB: $cubeReveals, valid: $validGame, gameIdIfValid: $gameIdIfValid"
    }

    // Input parsing
    val cubeGamesRaw: Vector[String] = scala.io.Source.fromFile("./Sources/2023/Day2CubeConundrum.txt").getLines().toVector

    val GamePattern: Regex = """Game ([0-9]+):(.+)""".r
    val RevealPattern: Regex = """([0-9, a-z]+)""".r
    val SingleColourPattern: Regex = """([0-9]+) ([a-z]+)""".r

    val games: Vector[Game] = cubeGamesRaw.map{GamePattern.findFirstMatchIn(_) match {
        case None => None
        case Some(GamePattern(gameIdString,revealsRaw)) => {
            val gameId: Int = Integer.parseInt(gameIdString)

            val revealStrings: Vector[String] = RevealPattern.findAllMatchIn(revealsRaw).map {_ match {
                case RevealPattern(revealString) => Some(revealString.trim)
                case _ => None
            }}.filter(_.isDefined).map(_.get).toVector

            val reveals: Vector[Vector[(Int, String)]] = revealStrings.map(revealString =>
                SingleColourPattern.findAllMatchIn(revealString).map {_ match {
                    case SingleColourPattern(count, colour) => Some((Integer.parseInt(count), colour))
                    case _ => None
                }}.filter(_.isDefined).map(_.get).toVector
            )

            def colourCount(reveal: Vector[(Int,String)], colour: String): Int = reveal.find { case (_, c) => c == colour} match {
                case Some((i, _)) => i
                case _ => 0
            }

            val revealRgbs: Vector[CubeReveal] = reveals.map(reveal => {
                val redCount = colourCount(reveal, "red")
                val greenCount = colourCount(reveal, "green")
                val blueCount = colourCount(reveal, "blue")

                CubeReveal(redCount, greenCount, blueCount)
            })

            Some(Game(gameId, revealRgbs))
        }
    }}.filter(_.isDefined).map(_.get)

    //println(games.mkString("\n"))

    // Part One
    val validSum: Int = games.foldLeft(0)( (a, g) => a + g.gameIdIfValid )
    println(s"Valid Game IDs sum: $validSum")

    // Part Two
    val cubePowers: Int = games.map(g => g.cubeReveals.reduce(_ minRequiredCubes _).cubePower).sum
    print(s"Cube powers sum: $cubePowers")
}