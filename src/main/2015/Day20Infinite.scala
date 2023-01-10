object Day20Infinite extends App {

    val atLeastAsManyPresentsAs: Long = 33100000

    def divisors(n: Long): Set[Long] = {
        (1l to (scala.math.sqrt(n)+1).toLong).map(i => n % i match {
            case 0 if n/i == i => Set(i)
            case 0 => Set(i, n/i)
            case _ => Set().asInstanceOf[Set[Long]]
        }).reduce(_ ++ _)
    }

    def divisorsWithLimit(limit: Long)(n: Long): Set[Long] = divisors(n).filter(i => n <= limit * i)

    def searchHouse(currentHouseNumber: Long, presents: Int, div: Long => Set[Long]): Long = {
        if(div(currentHouseNumber).map(_ * presents).sum < atLeastAsManyPresentsAs) searchHouse(currentHouseNumber + 1, presents, div)
        else currentHouseNumber
    }

    val houseNumberWithAtLeastAsManyPresents: Long = searchHouse(1, 10, divisors)
    println(s"House number with at least ${atLeastAsManyPresentsAs} presents is ${houseNumberWithAtLeastAsManyPresents}")

    val houseNumberWithAtLeastAsManyPresentsPartTwo: Long = searchHouse(1, 11, divisorsWithLimit(50))
    println(s"House number with at least ${atLeastAsManyPresentsAs} presents but with 50 houses per elf limit is ${houseNumberWithAtLeastAsManyPresentsPartTwo}")
}