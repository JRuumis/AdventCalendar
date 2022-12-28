object Day1CalorieCounting extends App {

    val sourceString: String = scala.io.Source.fromFile("./Sources/Day1CalorieCounter.txt").mkString
    val calorySplit = sourceString.split("\r\n\r\n").map((a: String) => a.split("\r\n").map(_.toInt))
    val calorySums = calorySplit.map(_.sum).toList
    val maxCalories = calorySums.max

    print(s"The top Elf is carrying ${maxCalories} calories.\n")

    val top3Calories = calorySums.sortWith((a, b) => a > b).take(3)
    val top3CaloriesSum = top3Calories.sum

    print(s"The top 3 elves carry ${top3CaloriesSum} calories overall.\n")
}
