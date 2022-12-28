import scala.math.abs

object Day18Lava extends App {


    val cubeInputRaw = scala.io.Source.fromFile("./Sources/Day18Lava.txt").getLines().toVector

    abstract class Coord3D {
        val x: Int
        val y: Int
        val z: Int
    }

    case class Surface(cubeX: Int, cubeY: Int, cubeZ: Int, x: Int, y: Int, z: Int) {

        def neighbourSurfaces(existingCubes: Set[Cube]): Vector[Surface] = (x, y, z) match {
            // X
            case (i, 0, 0) if i == 1 || i == -1 => Vector(

                (existingCubes contains Cube(cubeX + i, cubeY - 1, cubeZ), existingCubes contains Cube(cubeX, cubeY - 1, cubeZ)) match {
                    case (true, _) => Surface(cubeX + i, cubeY - 1, cubeZ, 0, 1, 0) // L
                    case (false, true) => Surface(cubeX, cubeY - 1, cubeZ, i, 0, 0) // same pane
                    case (false, false) => Surface(cubeX, cubeY, cubeZ, 0, -1, 0) // same cube
                },

                (existingCubes contains Cube(cubeX + i, cubeY + 1, cubeZ), existingCubes contains Cube(cubeX, cubeY + 1, cubeZ)) match {
                    case (true, _) => Surface(cubeX + i, cubeY + 1, cubeZ, 0, -1, 0) // L
                    case (false, true) => Surface(cubeX, cubeY + 1, cubeZ, i, 0, 0) // same pane
                    case (false, false) => Surface(cubeX, cubeY, cubeZ, 0, 1, 0) // same cube
                },

                (existingCubes contains Cube(cubeX + i, cubeY, cubeZ - 1), existingCubes contains Cube(cubeX, cubeY, cubeZ - 1)) match {
                    case (true, _) => Surface(cubeX + i, cubeY, cubeZ - 1, 0, 0, 1) // L
                    case (false, true) => Surface(cubeX, cubeY, cubeZ - 1, i, 0, 0) // same pane
                    case (false, false) => Surface(cubeX, cubeY, cubeZ, 0, 0, -1) // same cube
                },

                (existingCubes contains Cube(cubeX + i, cubeY, cubeZ + 1), existingCubes contains Cube(cubeX, cubeY, cubeZ + 1)) match {
                    case (true, _) => Surface(cubeX + i, cubeY, cubeZ + 1, 0, 0, -1) // L
                    case (false, true) => Surface(cubeX, cubeY, cubeZ + 1, i, 0, 0) // same pane
                    case (false, false) => Surface(cubeX, cubeY, cubeZ, 0, 0, 1) // same cube
                }
            )

            // Y
            case (0, i, 0) if i == 1 || i == -1 => Vector(

                (existingCubes contains Cube(cubeX - 1, cubeY + i, cubeZ), existingCubes contains Cube(cubeX - 1, cubeY, cubeZ)) match {
                    case (true, _) => Surface(cubeX - 1, cubeY + i, cubeZ, 1, 0, 0) // L
                    case (false, true) => Surface(cubeX - 1, cubeY, cubeZ, 0, i, 0) // same pane
                    case (false, false) => Surface(cubeX, cubeY, cubeZ, -1, 0, 0) // same cube
                },

                (existingCubes contains Cube(cubeX + 1, cubeY + i, cubeZ), existingCubes contains Cube(cubeX + 1, cubeY, cubeZ)) match {
                    case (true, _) => Surface(cubeX + 1, cubeY + i, cubeZ, -1, 0, 0) // L
                    case (false, true) => Surface(cubeX + 1, cubeY, cubeZ, 0, i, 0) // same pane
                    case (false, false) => Surface(cubeX, cubeY, cubeZ, 1, 0, 0) // same cube
                },

                (existingCubes contains Cube(cubeX, cubeY + i, cubeZ - 1), existingCubes contains Cube(cubeX, cubeY, cubeZ - 1)) match {
                    case (true, _) => Surface(cubeX, cubeY + i, cubeZ - 1, 0, 0, 1) // L
                    case (false, true) => Surface(cubeX, cubeY, cubeZ - 1, 0, i, 0) // same pane
                    case (false, false) => Surface(cubeX, cubeY, cubeZ, 0, 0, -1) // same cube
                },

                (existingCubes contains Cube(cubeX, cubeY + i, cubeZ + 1), existingCubes contains Cube(cubeX, cubeY, cubeZ + 1)) match {
                    case (true, _) => Surface(cubeX, cubeY + i, cubeZ + 1, 0, 0, -1) // L
                    case (false, true) => Surface(cubeX, cubeY, cubeZ + 1, 0, i, 0) // same pane
                    case (false, false) => Surface(cubeX, cubeY, cubeZ, 0, 0, 1) // same cube
                }

            )

            // Z
            case (0, 0, i) if i == 1 || i == -1 => Vector(

                (existingCubes contains Cube(cubeX, cubeY - 1, cubeZ + i), existingCubes contains Cube(cubeX, cubeY - 1, cubeZ)) match {
                    case (true, _) => Surface(cubeX, cubeY - 1, cubeZ + i, 0, 1, 0) // L
                    case (false, true) => Surface(cubeX, cubeY - 1, cubeZ, 0, 0, i) // same pane
                    case (false, false) => Surface(cubeX, cubeY, cubeZ, 0, -1, 0) // same cube
                },

                (existingCubes contains Cube(cubeX, cubeY + 1, cubeZ + i), existingCubes contains Cube(cubeX, cubeY + 1, cubeZ)) match {
                    case (true, _) => Surface(cubeX, cubeY + 1, cubeZ + i, 0, -1, 0) // L
                    case (false, true) => Surface(cubeX, cubeY + 1, cubeZ, 0, 0, i) // same pane
                    case (false, false) => Surface(cubeX, cubeY, cubeZ, 0, 1, 0) // same cube
                },

                (existingCubes contains Cube(cubeX - 1, cubeY, cubeZ + i), existingCubes contains Cube(cubeX - 1, cubeY, cubeZ)) match {
                    case (true, _) => Surface(cubeX - 1, cubeY, cubeZ + i, 1, 0, 0) // L
                    case (false, true) => Surface(cubeX - 1, cubeY, cubeZ, 0, 0, i) // same pane
                    case (false, false) => Surface(cubeX, cubeY, cubeZ, -1, 0, 0) // same cube
                },

                (existingCubes contains Cube(cubeX + 1, cubeY, cubeZ + i), existingCubes contains Cube(cubeX + 1, cubeY, cubeZ)) match {
                    case (true, _) => Surface(cubeX + 1, cubeY, cubeZ + i, -1, 0, 0) // L
                    case (false, true) => Surface(cubeX + 1, cubeY, cubeZ, 0, 0, i) // same pane
                    case (false, false) => Surface(cubeX, cubeY, cubeZ, 1, 0, 0) // same cube
                }

            )
        }

        /*
        def neighbourSurfacesIncorrect: Vector[Surface] = (x,y,z) match {
            // X
            case (i,_,_) if i == 1 || i == -1 => Vector(
                Surface(cubeX, cubeY, cubeZ, 0,1,0), Surface(cubeX, cubeY, cubeZ, 0,-1,0), Surface(cubeX, cubeY, cubeZ, 0,0,1), Surface(cubeX, cubeY, cubeZ, 0,0,-1), // same cube
                Surface(cubeX, cubeY+1, cubeZ, i,0,0), Surface(cubeX, cubeY-1, cubeZ, i,0,0), Surface(cubeX, cubeY, cubeZ+1, i,0,0), Surface(cubeX, cubeY, cubeZ-1, i,0,0), // same plain, other cubes
                Surface(cubeX+i, cubeY-1, cubeZ, 0,1,0), Surface(cubeX+i, cubeY+1, cubeZ, 0,-1,0), Surface(cubeX+i, cubeY, cubeZ-1, 0,0,1), Surface(cubeX+i, cubeY, cubeZ+1, 0,0,-1) // L, other cubes
            )

            // Y
            case (_,i,_) if i == 1 || i == -1 => Vector(
                Surface(cubeX, cubeY, cubeZ, 1,0,0), Surface(cubeX, cubeY, cubeZ, -1,0,0), Surface(cubeX, cubeY, cubeZ, 0,0,1), Surface(cubeX, cubeY, cubeZ, 0,0,-1), // same cube
                Surface(cubeX+1, cubeY, cubeZ, 0,i,0), Surface(cubeX-1, cubeY, cubeZ, 0,i,0), Surface(cubeX, cubeY, cubeZ+1, 0,i,0), Surface(cubeX, cubeY, cubeZ-1, 0,i,0), // same plain, other cubes
                Surface(cubeX-1, cubeY+i, cubeZ, 1,0,0), Surface(cubeX+1, cubeY+i, cubeZ, -1,0,0), Surface(cubeX, cubeY+i, cubeZ-1, 0,0,1), Surface(cubeX, cubeY+i, cubeZ+1, 0,0,-1) // L, other cubes
            )

            // Z
            case (_,_,i) if i == 1 || i == -1 => Vector(
                Surface(cubeX, cubeY, cubeZ, 1,0,0), Surface(cubeX, cubeY, cubeZ, -1,0,0), Surface(cubeX, cubeY, cubeZ, 0,1,0), Surface(cubeX, cubeY, cubeZ, 0,-1,0), // same cube
                Surface(cubeX+1, cubeY, cubeZ, 0,0,i), Surface(cubeX-1, cubeY, cubeZ, 0,0,i), Surface(cubeX, cubeY+1, cubeZ, 0,0,i), Surface(cubeX, cubeY-1, cubeZ, 0,0,i), // same plain, other cubes
                Surface(cubeX-1, cubeY, cubeZ+i, 1,0,0), Surface(cubeX+1, cubeY, cubeZ+i, -1,0,0), Surface(cubeX, cubeY-1, cubeZ+i, 0,1,0), Surface(cubeX, cubeY+1, cubeZ+i, 0,-1,0) // L, other cubes
            )
        }

         */
    }


    case class Cube(x: Int, y: Int, z: Int) extends Coord3D {

        def isNeigbour(other: Cube): Boolean = (
            (abs(x - other.x) == 1 && abs(y - other.y) == 0 && abs(z - other.z) == 0) ||
                (abs(x - other.x) == 0 && abs(y - other.y) == 1 && abs(z - other.z) == 0) ||
                (abs(x - other.x) == 0 && abs(y - other.y) == 0 && abs(z - other.z) == 1)
            )

        def getNeighbours: Vector[Cube] = Vector(
            Cube(x + 1, y, z), Cube(x - 1, y, z),
            Cube(x, y + 1, z), Cube(x, y - 1, z),
            Cube(x, y, z + 1), Cube(x, y, z - 1),
        )

        val surfaces: Vector[Surface] = Vector(
            Surface(x, y, z, 1, 0, 0),
            Surface(x, y, z, -1, 0, 0),
            Surface(x, y, z, 0, 1, 0),
            Surface(x, y, z, 0, -1, 0),
            Surface(x, y, z, 0, 0, 1),
            Surface(x, y, z, 0, 0, -1)
        )

    }


    val cubes: Vector[Cube] = cubeInputRaw.map(s => s.split(",").toVector).map { case (Vector(a, b, c)) => Cube(a.toInt, b.toInt, c.toInt) }
    val cubesSet: Set[Cube] = cubes.toSet

    //println(cubeCoords.mkString("\n"))

    val totalNumberOfSurfaces = cubes.size * 6


    val vectorNeigbours: Vector[Boolean] = (0 to cubes.size - 2).flatMap(c1 => (c1 + 1 to cubes.size - 1).map(c2 => cubes(c1) isNeigbour cubes(c2))).toVector
    val nrOfNeigbours: Int = vectorNeigbours.filter(a => a).size
    val surfacesLost: Int = nrOfNeigbours * 2

    val exposedSurfaces = totalNumberOfSurfaces - surfacesLost

    println(exposedSurfaces)


    val topZCoord: Int = cubes.map(_.z).max
    val topZCubes = cubes.filter(_.z == topZCoord)
    val frontierTopZSurfaces: Set[Surface] = topZCubes.map(c => c.surfaces.find(s => s.z == 1).get).toSet

    def isSurfaceExposed(s: Surface): Boolean = !(cubesSet contains Cube(s.cubeX + s.x, s.cubeY + s.y, s.cubeZ + s.z))


    def surfaceSurfer(frontierSurfaces: Set[Surface], seenSurfaces: Set[Surface]): Int = {

        if (frontierSurfaces.size == 0) seenSurfaces.size
        else {

            val frontierNeighbourSurfaces: Set[Surface] = frontierSurfaces.flatMap(s =>
                s.neighbourSurfaces(cubesSet) //.filter(ns =>
                //    cubesSet contains Cube(ns.cubeX, ns.cubeY, ns.cubeZ)
                //)
            )

            val frontierNeighbourSurfacesExposed: Set[Surface] = frontierNeighbourSurfaces.filter(s => isSurfaceExposed(s))
            val frontierNeighbourSurfacesExposedNotSeenBefore: Set[Surface] = frontierNeighbourSurfacesExposed -- seenSurfaces

            //val xxx = 1

            surfaceSurfer(frontierNeighbourSurfacesExposedNotSeenBefore, seenSurfaces ++ frontierSurfaces)
        }
    }

    val xxx: Int = surfaceSurfer(frontierTopZSurfaces, Set())

    println(xxx)


    /*
    val allCubeNeighbours: Set[Cube] = cubes.flatMap(c => c.getNeighbours).toSet

    val entrapped1 = allCubeNeighbours
        .map(n => (n, (n.getNeighbours.map(nn => cubesSet contains nn)) :+ !(cubesSet contains n) )   )
        .map{case(c,a) => (c,a.reduce(_ && _))}

    val entrappedCubes: Set[Cube] = entrapped1.filter{ case(_,e) => e }.map{case(c,_) => c}


    val entrappedCubesVector = entrappedCubes.toVector

    val vectorNeigboursXXX: Vector[Boolean] = (0 to entrappedCubesVector.size-2).flatMap(c1 => (c1+1 to entrappedCubesVector.size-1).map (c2 => entrappedCubesVector(c1) isNeigbour entrappedCubesVector(c2) )).toVector
    val nrOfNeigboursXXX: Int = vectorNeigboursXXX.filter(a => a).size
    val surfacesLostXXX: Int = nrOfNeigboursXXX * 2




    val entrappedSurfaces = entrappedCubes.size * 6 - surfacesLostXXX

    val xxx = exposedSurfaces - entrappedSurfaces

    println(xxx)


     */


}
