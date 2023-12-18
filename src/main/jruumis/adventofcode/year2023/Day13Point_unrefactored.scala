package jruumis.adventofcode.year2023

object Day13Point_unrefactored extends App {

    //val inputRaw = scala.io.Source.fromFile("./Sources/2023/Day13Point.txt").getLines().toVector.mkString("\n")
    val inputRaw = scala.io.Source.fromFile("./Sources/2023/Day13Point_TEST2.txt").getLines().toVector.mkString("\n")

    val matrices = inputRaw.split("\n\n").toVector.map(_.split("\n").map(_.toVector).toVector)
    val matricesTransposed = matrices.map(m => m.transpose)

    val matrices1 = matrices.map(m => m.map(a => a.mkString))
    val matricesTrasnposed1 = matricesTransposed.map(m => m.map(a => a.mkString))

    //println(matrices(1).mkString("\n"))
    //println

    //println(matricesTransposed(1).mkString("\n"))


    def oneDiff(a: Vector[String], b: Vector[String]): Option[(Int, Int)] = {

        val smudges = (0 until a.size).toVector.flatMap(y => (0 until a.head.size).toVector.map(x => {
            if( a(y)(x) != b(y)(x) ) Some((y,x)) else None
        })).filter(_.isDefined)

        if (smudges.size == 1) smudges.head else None
    }

    //val aaa = oneDiff(Vector("janis"), Vector("jaNis"))
    //println(aaa)



    def findSymmetry1(in: Vector[String], line: Int = 0, maxSoFar: Int = 0, lineWithMaxSoFar: Int = 0, accu: Vector[String] = Vector()): Int = in match {
        case Vector() => lineWithMaxSoFar
        case i +: rest => {

            val newAccu = i +: accu

            val restSize = rest.size

            val newAccuSize = newAccu.size

            val minSize = math.min(restSize, newAccuSize)

            if(minSize > 0 && minSize > maxSoFar && newAccu.take(minSize) == rest.take(minSize) )
                findSymmetry1(rest, line+1, minSize, line+1, newAccu)
            else {
                findSymmetry1(rest, line+1, maxSoFar, lineWithMaxSoFar, newAccu)
            }
        }
    }


    def findSymmetry2(
                         in: Vector[String],
                         line: Int = 0,
                         maxSoFar: Int = 0,
                         lineWithMaxSoFar: Int = 0,
                         smudge: Option[(Int,Int)] = None,
                         accu: Vector[String] = Vector()
                     ): (Int, Option[(Int,Int)]) = in match {
        case Vector() => (lineWithMaxSoFar, smudge)
        case i +: rest => {

            val newAccu = i +: accu
            val restSize = rest.size
            val newAccuSize = newAccu.size
            val minSize = math.min(restSize, newAccuSize)

            val newSmudge: Option[(Int, Int)] = oneDiff(newAccu.take(minSize), rest.take(minSize))

            if (minSize > 0 && minSize > maxSoFar && newSmudge.isDefined)
                findSymmetry2(rest, line + 1, minSize, line + 1, newSmudge, newAccu)
            else {
                findSymmetry2(rest, line + 1, maxSoFar, lineWithMaxSoFar, smudge, newAccu)
            }
        }
    }


    val m = matrices1.map(findSymmetry1(_))
    val mt = matricesTrasnposed1.map(findSymmetry1(_))
    val xxx = (m zip mt).map{case(r,c) => 100*r + c}.sum

    println(m)
    println(mt)
    println(xxx)


    println

    val m2 = matrices1.map(findSymmetry2(_))
    val mt2 = matricesTrasnposed1.map(findSymmetry2(_))
    val xxx2 = (m2 zip mt2).map { case ((r, smuR), (c,smuC)) => 100 * r + c }.sum

    println(m2)
    println(mt2)

    println(xxx2)


}
