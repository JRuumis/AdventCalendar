object Day20Grove extends App {

    val numbersInput: Vector[Int] = scala.io.Source.fromFile("./Sources/Day20Grove_TEST.txt").getLines().map(_.toInt).toVector
    println(numbersInput)
    //println(numbersInput.size == numbersInput.distinct.size)

    val uniqueNumbers: Vector[(Int, Int)] = numbersInput.zipWithIndex
    val seqLength: Int = uniqueNumbers.length

    def newPosition(currentPosition: Int, moveBy: Int): Int = { // positions are zero-based: 0 .. seqLength-1
        val newPos: Int = (currentPosition + moveBy) % seqLength
        val newPosAdj = if (newPos < 0) newPos + seqLength else newPos

        if (newPosAdj == 0) seqLength - 1 else if (newPosAdj == seqLength - 1) 0 else newPosAdj
    }

    def dumbSearch(searchValue: (Int, Int), vec: Vector[(Int, Int)], index: Int = 0): Int = vec match {
        case c +: rest => if (c == searchValue) index else dumbSearch(searchValue, rest, index + 1)
        case Vector() => {
            println(f"ERROR! Value ${searchValue} not found in ${vec}")
            0
        }
    }

    def moveFromTo(vec: Vector[(Int, Int)], indexFrom: Int = 0, indexTo: Int): Vector[(Int, Int)] = {

        val firstPart: Vector[(Int, Int)] = if (indexFrom > 0) vec.slice(0, indexFrom) else Vector()
        val secondPart: Vector[(Int, Int)] = if (indexFrom + 1 < seqLength) vec.slice(indexFrom + 1, seqLength) else Vector()
        val valueToMove: (Int, Int) = vec(indexFrom)

        val vectorWithValueRemoved: Vector[(Int, Int)] = firstPart ++ secondPart

        val adjustedIndexTo: Int = if (indexTo > indexFrom) indexTo - 1 else indexTo

        val xxx = vectorWithValueRemoved.slice(0, adjustedIndexTo + 1)

        //val firstPart2: Vector[(Int, Int)] = if(indexTo == seqLength) vectorWithValueRemoved else if(adjustedIndexTo+1 > 0) vectorWithValueRemoved.slice(0, adjustedIndexTo+1) else Vector()
        val firstPart2: Vector[(Int, Int)] = if (adjustedIndexTo + 1 > 0) vectorWithValueRemoved.slice(0, adjustedIndexTo + 1) else Vector()
        val secondPart2: Vector[(Int, Int)] = if (adjustedIndexTo + 1 < seqLength - 1) vectorWithValueRemoved.slice(adjustedIndexTo + 1, seqLength - 1) else Vector()

        (firstPart2 :+ valueToMove) ++ secondPart2
    }

    def move(numbersToMove: Vector[(Int, Int)], currentNumbers: Vector[(Int, Int)]): Vector[(Int, Int)] = numbersToMove match {
        case Vector() => currentNumbers
        case (pair@(n, i)) +: rest => {

            val moverCurrentlyAtPosition: Int = dumbSearch(pair, currentNumbers)
            val moverNewPosition: Int = newPosition(moverCurrentlyAtPosition, n)

            val newCurrentNumbers = moveFromTo(currentNumbers, moverCurrentlyAtPosition, moverNewPosition)

            //println(newCurrentNumbers)
            move(rest, newCurrentNumbers)
        }
    }

    val xxx = move(uniqueNumbers, uniqueNumbers)
    println(xxx)


    val aaa = xxx.find(_._1 == 0)
    println(aaa)

    val zero = (0, 5) // test
    //val zero = (0,4273) // prod

    val zeroPosition = dumbSearch(zero, xxx)

    println(zeroPosition)

    val pos1: Int = (zeroPosition + 1000) % seqLength
    val pos2: Int = (zeroPosition + 2000) % seqLength
    val pos3: Int = (zeroPosition + 3000) % seqLength

    val n1 = xxx(pos1)
    val n2 = xxx(pos2)
    val n3 = xxx(pos3)

    println(s"${n1} ${n2} ${n3}")

    println(n1._1 + n2._1 + n3._1)


}
