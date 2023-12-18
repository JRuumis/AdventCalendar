package jruumis.adventofcode.year2015

import scala.util.matching.Regex

object Day15Hungry extends App {

    val ingridientsRaw: Vector[String] = scala.io.Source.fromFile("./Sources/2015/Day15Hungry.txt").getLines().toVector

    case class Properties(cap: Int, dur: Int, fla: Int, tex: Int, cal: Int) {
        def *(i: Int): Properties = Properties(cap*i, dur*i, fla*i, tex*i, cal*i)
        def +(other: Properties): Properties = Properties(cap + other.cap, dur + other.dur, fla + other.fla, tex + other.tex, cal + other.cal)

        val score: Int = {
            (if(cap < 0) 0 else cap) * (if(dur < 0) 0 else dur) * (if(fla < 0) 0 else fla) * (if(tex < 0) 0 else tex)
        }
    }
    case class Ingridient(name: String, properties: Properties)

    val ingridientPattern: Regex = """([A-Za-z]+): capacity (-?[0-9]+), durability (-?[0-9]+), flavor (-?[0-9]+), texture (-?[0-9]+), calories (-?[0-9]+)""".r
    val ingridients: Vector[Ingridient] = ingridientsRaw.map(_ match {
        case ingridientPattern(a,b,c,d,e,f) => Ingridient(a, Properties(b.toInt,c.toInt,d.toInt,e.toInt,f.toInt) )
    })

    val properties: Vector[Properties] = ingridients.map(_.properties)

    val maxSetSide: Int = 100
    val numIngridients: Int = ingridients.length

    def permutate(left: Int, currentRoot: Vector[Int] = Vector()): Set[Vector[Int]] = left match {
        case 0 => Set(currentRoot)
        case 1 => Set(currentRoot :+ (maxSetSide - currentRoot.sum))
        case _ => {
            val sum = currentRoot.sum
            (0 to (maxSetSide - sum)).map(i => permutate(left-1, currentRoot :+ i)).reduce(_ ++ _)
        }
    }

    val permutations: Set[Vector[Int]] =  permutate(numIngridients) // 176,851 for PROD data set
    val permutatedIngridients = permutations.toVector
        .map(p => (0 to p.size-1).map(i => properties(i) * p(i)))
        .map(a => a.reduce(_ + _))

    val ingridientMaxScore: Int =        permutatedIngridients.map(_.score).max
    val ingridient500CalMaxScore: Int =  permutatedIngridients.filter(a => a.cal == 500).map(_.score).max

    println(s"Maximum possible cookie score: ${ingridientMaxScore}")
    println(s"Maximum possible cookie score with 500 calories: ${ingridient500CalMaxScore}")
}