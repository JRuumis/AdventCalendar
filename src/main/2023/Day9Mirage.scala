import scala.annotation.tailrec

object Day9Mirage extends App {
    val environmentSequences: Vector[Vector[Long]] = scala.io.Source.fromFile("./Sources/2023/Day9Mirage.txt")
        .getLines().map(_.split(" ").map(_.toLong).toVector).toVector

    @tailrec
    def diffsFromSequence(sequence: Vector[Long], accu: Vector[Long] = Vector()): Vector[Long] = sequence match {
        case Vector() => accu
        case _ +: Vector() => accu
        case a +: b +: rest => diffsFromSequence(b +: rest, accu :+ (b-a))
    }

    @tailrec
    def diver(curSequence: Vector[Long], accuDiffs: Vector[Vector[Long]], depth: Int = 0): Vector[Vector[Long]] = {
        if(curSequence.forall(_ == 0L))
            accuDiffs
        else {
            val newDepth: Vector[Long] = diffsFromSequence(curSequence)
            diver(newDepth, accuDiffs :+ newDepth, depth+1)
        }
    }

    def sequenceDepths(sequence: Vector[Long]): Vector[Vector[Long]] = diver(sequence, Vector(sequence))

    @tailrec
    def getNextDepths(curDepths: Vector[Vector[Long]], prevDepth: Vector[Long] = Vector(), accu: Vector[Vector[Long]] = Vector()): Vector[Vector[Long]] = curDepths match {
        case Vector() => accu

        case rest :+ bottom if prevDepth == Vector() => {
            val bottomDepth: Vector[Long] = bottom :+ 0L
            getNextDepths(rest, bottomDepth, bottomDepth +: accu)
        }

        case rest :+ current => {
            val curDepth: Vector[Long] = current :+ (prevDepth.last + current.last)
            getNextDepths(rest, curDepth, curDepth +: accu)
        }
    }

    @tailrec
    def getPrevDepths(curDepths: Vector[Vector[Long]], prevDepth: Vector[Long] = Vector(), accu: Vector[Vector[Long]] = Vector()): Vector[Vector[Long]] = curDepths match {
        case Vector() => accu

        case rest :+ current if prevDepth == Vector() => {
            val curDepth: Vector[Long] = 0L +: current
            getPrevDepths(rest, curDepth, curDepth +: accu)
        }

        case rest :+ current => {
            val curDepth: Vector[Long] = (current.head - prevDepth.head) +: current
            getPrevDepths(rest, curDepth, curDepth +: accu)
        }
    }

    val sequenceDepths: Vector[Vector[Vector[Long]]] = environmentSequences.map(sequenceDepths(_))

    // Part One
    val sequenceDepthsNext: Vector[Vector[Vector[Long]]] = sequenceDepths.map(getNextDepths(_))
    val nextsOnTop: Vector[Long] = sequenceDepthsNext.map(_.head.last)

    println(s"The sum of next values in histories is ${nextsOnTop.sum}")

    // Part Two
    val sequenceDepthsPrev: Vector[Vector[Vector[Long]]] = sequenceDepths.map(getPrevDepths(_))
    val prevsOnTop: Vector[Long] = sequenceDepthsPrev.map(_.head.head)

    println(s"The sum of previous values in histories is ${prevsOnTop.sum}")
}