object Day20Grove2 extends App {

    val numbersInput: Vector[Long] = scala.io.Source.fromFile("./Sources/Day20Grove.txt").getLines().map(_.toLong).toVector
    println(numbersInput)
    //println(numbersInput.size == numbersInput.distinct.size)

    val uniqueNumbers: Vector[(Long, Long)] = numbersInput.zipWithIndex.map { case (a, b) => (a, b.toLong) }
    val seqLength: Int = uniqueNumbers.length

    def newPosition(currentPosition: Long, moveBy: Long): Int = { // positions are zero-based: 0 .. seqLength-1

        val newPos: Long = (currentPosition + moveBy) % (seqLength - 1)

        val direction: Char = if (moveBy > 0) 'R' else if (moveBy < 0) 'L' else if (moveBy == 0) 'S' else 'X'

        val newPosAdj = if (newPos < 0) (seqLength - 1) + newPos else newPos

        val xxx = direction match {
            case 'L' if newPosAdj == 0 => seqLength - 1
            case 'R' if newPosAdj == seqLength - 1 => 0
            case 'S' => newPosAdj
            case _ => newPosAdj
        }

        xxx.toInt
        //newPosAdj
        //if(newPosAdj == 0) seqLength-2 else if(newPosAdj == seqLength-2) 0 else newPosAdj
    }

    def dumbSearch(searchValue: (Long, Long), vec: Vector[(Long, Long)], index: Int = 0): Int = vec match {
        case c +: rest => if (c == searchValue) index else dumbSearch(searchValue, rest, index + 1)
        case Vector() => {
            println(f"ERROR! Value ${searchValue} not found in ${vec}")
            0
        }
    }

    def moveFromTo(vec: Vector[(Long, Long)], indexFrom: Int = 0, indexTo: Int): Vector[(Long, Long)] = {

        val firstPart: Vector[(Long, Long)] = if (indexFrom > 0) vec.slice(0, indexFrom) else Vector()
        val secondPart: Vector[(Long, Long)] = if (indexFrom + 1 < seqLength) vec.slice(indexFrom + 1, seqLength) else Vector()
        val valueToMove: (Long, Long) = vec(indexFrom)

        val vectorWithValueRemoved: Vector[(Long, Long)] = firstPart ++ secondPart

        val adjustedIndexTo: Int = indexTo //if(indexTo > indexFrom) indexTo-1 else indexTo

        //val xxx = vectorWithValueRemoved.slice(0, adjustedIndexTo+1)

        //val firstPart2: Vector[(Int, Int)] = if(indexTo == seqLength) vectorWithValueRemoved else if(adjustedIndexTo+1 > 0) vectorWithValueRemoved.slice(0, adjustedIndexTo+1) else Vector()
        val firstPart2: Vector[(Long, Long)] = if (adjustedIndexTo > 0) vectorWithValueRemoved.slice(0, adjustedIndexTo) else Vector()
        val secondPart2: Vector[(Long, Long)] = if (adjustedIndexTo < seqLength - 1) vectorWithValueRemoved.slice(adjustedIndexTo, seqLength - 1) else Vector()

        (firstPart2 :+ valueToMove) ++ secondPart2
    }


    val rightAnswer =
        Vector(
            Vector(1, 2, -3, 3, -2, 0, 4),
            Vector(2, 1, -3, 3, -2, 0, 4),
            Vector(1, -3, 2, 3, -2, 0, 4),
            Vector(1, 2, 3, -2, -3, 0, 4),
            Vector(1, 2, -2, -3, 0, 3, 4),
            Vector(1, 2, -3, 0, 3, 4, -2),
            Vector(1, 2, -3, 0, 3, 4, -2),
            Vector(1, 2, -3, 4, 0, 3, -2)
        )


    def move(numbersToMove: Vector[(Long, Long)], currentNumbers: Vector[(Long, Long)], step: Long): Vector[(Long, Long)] = numbersToMove match {
        case Vector() => currentNumbers
        case (pair@(n, i)) +: rest => {

            val moverCurrentlyAtPosition: Int = dumbSearch(pair, currentNumbers)
            val moverNewPosition: Int = newPosition(moverCurrentlyAtPosition.toLong, n)

            val newCurrentNumbers = moveFromTo(currentNumbers, moverCurrentlyAtPosition, moverNewPosition)

            //print(newCurrentNumbers.map(_._1) + s" - step ${step}")
            //println(if(rightAnswer(step) == newCurrentNumbers.map(_._1)) " -> OK" else " -> ERROR")


            move(rest, newCurrentNumbers, step + 1)
        }
    }

    /*
    val movedNumbers = move(uniqueNumbers, uniqueNumbers, 1)
    println(movedNumbers)


    val zero: (Long, Long) = movedNumbers.find(_._1 == 0).get
    println(zero)

    //val zero = (0,5) // test
    //val zero = (0,4273) // prod

    val zeroPosition = dumbSearch(zero, movedNumbers)
    println(zeroPosition)

    val pos1: Int = (zeroPosition + 1000) % seqLength
    val pos2: Int = (zeroPosition + 2000) % seqLength
    val pos3: Int = (zeroPosition + 3000) % seqLength

    val n1 = movedNumbers(pos1)
    val n2 = movedNumbers(pos2)
    val n3 = movedNumbers(pos3)

    println(s"${n1} ${n2} ${n3}")

    println(n1._1 + n2._1 + n3._1)


     */

    // Part 2
    val decryptionKey: Long = 811589153

    val secondNumbers = uniqueNumbers.map { case (a, b) => (a * decryptionKey, b) }
    println(s"decryptedNumbers: ${secondNumbers.map(_._1)}")

    val moved1 = move(secondNumbers, secondNumbers, 1)
    println(s"after round1: ${moved1.map(_._1)}")

    val moved2 = move(secondNumbers, moved1, 1)
    println(s"after round2: ${moved2.map(_._1)}")


    val moved3 = move(secondNumbers, moved2, 1)
    val moved4 = move(secondNumbers, moved3, 1)
    val moved5 = move(secondNumbers, moved4, 1)
    val moved6 = move(secondNumbers, moved5, 1)
    val moved7 = move(secondNumbers, moved6, 1)
    val moved8 = move(secondNumbers, moved7, 1)
    val moved9 = move(secondNumbers, moved8, 1)
    val moved10 = move(secondNumbers, moved9, 1)


    println(moved10)

    val zero2: (Long, Long) = moved10.find(_._1 == 0).get
    println(zero2)

    val zeroPosition2 = dumbSearch(zero2, moved10)
    println(zeroPosition2)


    val _pos1: Int = (zeroPosition2 + 1000) % seqLength
    val _pos2: Int = (zeroPosition2 + 2000) % seqLength
    val _pos3: Int = (zeroPosition2 + 3000) % seqLength

    val _n1 = moved10(_pos1)
    val _n2 = moved10(_pos2)
    val _n3 = moved10(_pos3)

    println(s"${_n1} ${_n2} ${_n3}")

    println(_n1._1 + _n2._1 + _n3._1)


}
