import scala.math.pow

object Day25HotAir extends App {

    val snafuRaw: Vector[Vector[Char]] = scala.io.Source.fromFile("./Sources/2022/Day25HotAir.txt").getLines().map(_.toVector).toVector


    def snafuToDecimal(snafu: Vector[Char], currentSnafuFivePower: Long = 0, accuDecimal: Long = 0): Long = snafu match {
        case Vector() => accuDecimal
        case rest :+ '2' => snafuToDecimal(rest, currentSnafuFivePower + 1, accuDecimal + pow(5, currentSnafuFivePower).toLong * 2.toLong)
        case rest :+ '1' => snafuToDecimal(rest, currentSnafuFivePower + 1, accuDecimal + pow(5, currentSnafuFivePower).toLong * 1.toLong)
        case rest :+ '0' => snafuToDecimal(rest, currentSnafuFivePower + 1, accuDecimal + pow(5, currentSnafuFivePower).toLong * 0.toLong)
        case rest :+ '-' => snafuToDecimal(rest, currentSnafuFivePower + 1, accuDecimal + pow(5, currentSnafuFivePower).toLong * -1.toLong)
        case rest :+ '=' => snafuToDecimal(rest, currentSnafuFivePower + 1, accuDecimal + pow(5, currentSnafuFivePower).toLong * -2.toLong)
    }

    println(snafuRaw.map(_.mkString).mkString("\n"))

    val decimals = snafuRaw.map(r => snafuToDecimal(r))

    println
    println(decimals.mkString("\n"))

    val decimalsSum: Long = decimals.sum
    println
    println(decimalsSum)

    // 20 - max power of 5
    val maxPowerFive = 20

    //println( (0 to (maxPowerFive-1)).toVector )

    //val toPow = (0 to (maxPowerFive-1)).toVector.map(p => (p.toLong -> pow(5,p).toLong)).toMap
    //val fromPow = (0 to (maxPowerFive-1)).toVector.map(p => (pow(5,p).toLong -> p.toLong)).toMap

    //println(toPow)
    //println(fromPow)

    val maxLenRaw = snafuRaw.map(r => r.length).max
    println(maxLenRaw)


    //val digits: Vector[Char] = Vector('=','-','0','1','2')

    def translate(i: Long): Char = i match {
        case -2 => '='
        case -1 => '-'
        case 0 => '0'
        case 1 => '1'
        case 2 => '2'
    }

    def decToSnafu(i: Long, accu: Vector[Char] = Vector()): Vector[Char] = {
        val xxx: Long = (i + 2) % 5
        val yyy: Long = (i + 2) / 5

        if (yyy == 0) translate(xxx - 2) +: accu else decToSnafu(yyy, translate(xxx - 2) +: accu)
    }

    println(decToSnafu(decimalsSum).mkString)

    //val seq = (1 to 50).toVector

    //val xxx = seq.map(i => s"${i} ---> ${lame(i)}")
    //println( xxx.mkString("\n") )


    /*
    def brute(currentDecimal: Long, accu: Vector[Vector[Char]]): Vector[Vector[Char]] = {
    }
     */


}
