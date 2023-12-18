package jruumis.adventofcode.coordinates

sealed trait Direction { val coord: Coord }
case object Up extends Direction { override val coord = Coord(0, -1) }
case object Down extends Direction { override val coord = Coord(0, 1) }
case object Left extends Direction { override val coord = Coord(-1, 0) }
case object Right extends Direction { override val coord = Coord(1, 0) }
