package jruumis.adventofcode.year2015

import java.security.MessageDigest

object Day5md5 extends App {

    //val keyString: String = "abcdef"
    val keyString: String = "ckczppom"

    def md5(s: String): String = {
        MessageDigest.getInstance("MD5").digest(s.getBytes).map(0xFF & _).map { "%02x".format(_) }.foldLeft("") {_ + _}
    }

    def searchNumber(currentNumber: Int): Int = {
        val key: String = keyString + currentNumber.toString
        val enc: String = md5(key)

        if(currentNumber % 250000 == 0) {
            println(s"${key} --> ${enc}")
        } else {}

        if (md5(key).take(6) == "000000") {
            println(s"${key} --> ${enc}")
            currentNumber
        }else
            searchNumber(currentNumber+1)
    }

    val position = searchNumber(0)
    println(position)
}
