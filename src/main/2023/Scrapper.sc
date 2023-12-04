

// Creating an immutable Map
val originalMap = Map("one" -> 1, "two" -> 2, "three" -> 3)

// Updating the Map with a new key-value pair
val updatedMap1 = originalMap + ("four" -> 4)

// Updating the Map with a modification to an existing key-value pair
val updatedMap2 = originalMap + ("two" -> 22)

// Displaying the original and updated Maps
println("Original Map: " + originalMap)
println("Updated Map 1: " + updatedMap1)
println("Updated Map 2: " + updatedMap2)