package jruumis.adventofcode.year2015

object Day17Containers extends App {

    val containersRaw: Vector[Int] = scala.io.Source.fromFile("./Sources/2015/Day17Containers.txt").getLines().toVector.map(_.toInt)
    //val totalStorage: Int = 25
    val totalStorage: Int = 150

    val uniqueContainers: Set[(Int, Int)] = containersRaw.zipWithIndex.toSet

    val usedRoots: scala.collection.mutable.Set[Set[(Int,Int)]] = scala.collection.mutable.Set()

    def permutate(leftContainers: Set[(Int,Int)], currentRoot: Set[(Int, Int)] = Set()): Set[Set[(Int,Int)]] = {
        val currentSum: Int = currentRoot.toVector.map{case(a,_)=>a}.sum // toVector required to prevent deduplication of equal volume containers

        if(usedRoots contains currentRoot)
            Set() // prune
        else {
            usedRoots += currentRoot

            if (currentSum == totalStorage) {
                Set(currentRoot)
            } else {
                val candidateContainers = leftContainers.filter{case(i,_) => i <= totalStorage - currentSum}
                val permutations = candidateContainers.map(c => permutate(candidateContainers - c, currentRoot + c))

                if(permutations.isEmpty) Set() else permutations.reduce(_ ++ _)
            }
        }
    }

    val validCombinationsOfContainers: Set[Set[(Int, Int)]] = permutate(uniqueContainers)

    println(s"Total number of container combinations to fill ${totalStorage} is ${validCombinationsOfContainers.size}")

    val minNumberOfContainersToFill: Int = validCombinationsOfContainers.map(_.size).min
    val validCombinationsOfContainersMinOnly = validCombinationsOfContainers.filter(_.size == minNumberOfContainersToFill)

    println(s"Total number of container combinations with the minimum container number of ${minNumberOfContainersToFill} to fill ${totalStorage} is ${validCombinationsOfContainersMinOnly.size}")
}