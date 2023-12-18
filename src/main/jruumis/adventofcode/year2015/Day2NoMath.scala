package jruumis.adventofcode.year2015

object Day2NoMath extends App {

    val dimsRaw: Vector[String] = scala.io.Source.fromFile("./Sources/2015/Day2NoMath.txt").getLines().toVector

    case class Dims(l: Int, w: Int, h: Int) {
        val sides: Vector[Int] = Vector(l*w, w*h, h*l)
        val smallestSide = sides.min
        val wrapSurface = 2*l*w + 2*w*h + 2*h*l + smallestSide

        val facePerimeters = Vector(2*(l+w), 2*(w+h), 2*(h+l))
        val smallestPerimeter: Int = facePerimeters.min
        val volume: Int = l*w*h
        val ribbonLength: Int = volume + smallestPerimeter
    }

    val dims: Vector[Dims] = dimsRaw.map(r => r.split("x")).map(a => Dims(a(0).toInt, a(1).toInt, a(2).toInt))
    val dimSufraces: Int = dims.map(_.wrapSurface).sum

    println(s"Total wrap needed: ${dimSufraces}")

    val dimRibbons: Int = dims.map(_.ribbonLength).sum

    println(s"Total ribbon needed: ${dimRibbons}")

}
