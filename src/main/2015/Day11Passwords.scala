import scala.annotation.tailrec
import scala.math.pow

object Day11Passwords extends App {

    //val previousPassword = "abcdefgh".toVector // TEST1 - abcdffaa
    //val previousPassword = "ghijklmn".toVector // TEST2 - ghjaabcc
    //val previousPassword = "hepxcrrq".toVector // PROD Part 1 - hepxxyzz
    val previousPassword = "hepxxyzz".toVector // PROD Part 2 - heqaabcc


    val valA: Long = 'a'.toLong
    val valZ: Long = 'z'.toLong
    val base = valZ - valA + 1
    val adj: Long = valA

    val forbiddenChars: Set[Long] = Set('i', 'o', 'l').map(c => c.toLong - adj)

    def toBaseInt(v: Vector[Char]): Long = {
        v.map(_.toLong - adj).reverse.zipWithIndex
            .map{case(c,b) => c * pow(base,b).toLong}
            .sum
    }

    @tailrec
    def toIntVector(i: Long, accu: Vector[Long] = Vector()): Vector[Long] = {
        if(i / base == 0)   (i % base) +: accu
        else                toIntVector(i / base, (i % base) +: accu)
    }

    def toCharVector(i: Long): Vector[Char] = {
        val cv: Vector[Char] = toIntVector(i).map(c => (c + adj).toChar)
        if(cv.length == 7) 'a' +: cv else cv
    }


    @tailrec
    def req1IncreasingStraight(v: Vector[Long]): Boolean = v match {
        case a +: b +: c +: _ if b == a+1 && c == b+1 => true
        case _ +: b +: c +: rest => req1IncreasingStraight(b +: c +: rest)
        case _ => false
    }

    @tailrec
    def req2ForbiddenChars(v: Vector[Long]): Boolean = v match {
        case a +: _ if forbiddenChars contains a => false
        case _ +: rest => req2ForbiddenChars(rest)
        case Vector() => true
    }

    @tailrec
    def req3TwoPairs(v: Vector[Long], foundPairChar: Option[Long] = None): Boolean = v match {
        case a +: b +: rest if a == b && !foundPairChar.isDefined => req3TwoPairs(rest, Some(a)) // first pair found
        case a +: b +: rest if a == b && foundPairChar.isDefined && foundPairChar.get == a => req3TwoPairs(rest, foundPairChar) // second pair found, char same
        case a +: b +: _ if a == b && foundPairChar.isDefined /* && foundPairChar.get != a */ => true // second pair found, char different, not sure if the non-match condition is required...
        case _ +: b +: rest => req3TwoPairs(b +: rest, foundPairChar) // no match
        case _ => false // requirement unfulfilled
    }

    @tailrec
    def iterate(currPasswordLong: Long): Long = {
        val curVec: Vector[Long] = toIntVector(currPasswordLong)

        if(req1IncreasingStraight(curVec) && req2ForbiddenChars(curVec) && req3TwoPairs(curVec)) currPasswordLong
        else iterate(currPasswordLong+1)
    }

    val previousPasswordLong: Long = toBaseInt(previousPassword)
    val newPasswordLong: Long = iterate(previousPasswordLong+1)
    val newPasswordVect = toCharVector(newPasswordLong)

    println(s"Previous password:\t${previousPassword.mkString}")
    println(s"New password:\t\t${newPasswordVect.mkString}")
}