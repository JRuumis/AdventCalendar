package jruumis.adventofcode.coordinates

sealed trait Direction {
    val coord: Coord
    val opposite: Direction
}

case object Up extends Direction {
    val coord = Coord(0, -1)
    val opposite = Down
}

case object Down extends Direction {
    val coord = Coord(0, 1)
    val opposite = Up
}

case object Left extends Direction {
    val coord = Coord(-1, 0)
    val opposite = Right
}

case object Right extends Direction {
    val coord = Coord(1, 0)
    val opposite = Left
}
