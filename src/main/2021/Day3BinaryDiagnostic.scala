import scala.annotation.tailrec

object Day3BinaryDiagnostic extends App {
    val binariesRaw = scala.io.Source.fromFile("./Sources/2021/Day3BinaryDiagnostic_TEST1.txt").getLines().toVector

    def binaryCounter(binaries: Vector[String], accu: Vector[Int] ): Vector[Int] = binaries match {
        case Vector() => accu
        case b +: rest => {
            val zzz = (b.toVector.map(_.asDigit) zip accu)
            val newAccu = zzz.map{case(a,b) => a+b}
            binaryCounter(rest, newAccu)
        }
    }

    val binaryCounts = binaryCounter(binariesRaw, "0".repeat(binariesRaw.head.size).toVector.map(_.asDigit))
    val inputSize: Float = binariesRaw.size

    val gammaRate: Vector[Int] = binaryCounts.map(b => if(b < inputSize/2) 0 else 1)
    val epsilonRate: Vector[Int] = gammaRate.map(b => if(b==1) 0 else 1)


    @tailrec
    def binToDec(bin: Vector[Int], multi: Int = 1, decAccu: Int = 0): Int = bin match {
        case Vector() => decAccu
        case rest :+ b => binToDec(rest, multi*2, decAccu + b*multi)
    }

    val decGammaRate: Int = binToDec(gammaRate)
    val decEpsilonRate: Int = binToDec(epsilonRate)

    println(s"Gamma rate: $decGammaRate, Epsilon rate: $decEpsilonRate, Product: ${decGammaRate * decEpsilonRate}")


    // Part Two
    val oxygenRating: Vector[Int] = binaryCounts.map(b => if(b >= inputSize/2) 1 else 0)
    val co2Rating: Vector[Int] = binaryCounts.map(b => if(b <= inputSize/2) 1 else 0)

    println(oxygenRating)
    println(co2Rating)



}
