package jruumis.adventofcode.year2022

import scala.annotation.tailrec
import scala.math.abs

object Day9RopeBridge extends App {

    val ropeStepsInput = scala.io.Source.fromFile("./Sources/2022/Day9RopeBridge.txt").getLines()

    case class Move(deltaX: Int, deltaY: Int)

    val moves: List[Move] = ropeStepsInput.map(_.split(" ").toList).map {
        _ match {
            case "U" :: len :: Nil => Move(0, -1 * len.toInt)
            case "D" :: len :: Nil => Move(0, len.toInt)
            case "L" :: len :: Nil => Move(-1 * len.toInt, 0)
            case "R" :: len :: Nil => Move(len.toInt, 0)
        }
    }.toList

    val stepMoves: List[Move] = moves.flatMap { m =>
        (1 to abs(m.deltaX + m.deltaY)).map(_ => Move(m.deltaX / abs(m.deltaX + m.deltaY), m.deltaY / abs(m.deltaX + m.deltaY)))
    } // split steps larger than 1 to steps of length 1

    case class Coordinates(x: Int, y: Int) {
        def moveBy(m: Move): Coordinates = Coordinates(x + m.deltaX, y + m.deltaY)
    }

    abstract sealed class RopeLink {
        val epoch: Int
        val currentCoord: Coordinates
    }

    case class Head(epoch: Int, currentCoord: Coordinates) extends RopeLink {
        def leadWith(move: Move): Head = Head(epoch + 1, currentCoord moveBy move)
    }

    case class Tail(epoch: Int, currentCoord: Coordinates) extends RopeLink {
        def follow(ropeLinkToFollow: RopeLink): Tail = (ropeLinkToFollow.currentCoord, currentCoord) match {
            case (leader, follower) if (leader == follower) => Tail(epoch + 1, currentCoord) // Head on Tail, don't move
            case (leader, follower) if abs(leader.x - follower.x) <= 1 && abs(leader.y - follower.y) <= 1 => Tail(epoch + 1, currentCoord) // Next to each other, don't move
            case (leader, follower) if abs(leader.y - follower.y) > 1 && abs(leader.x - follower.x) > 1 => { // !!!!!!!!! missed this initially
                val move: Move = Move(
                    leader.x - follower.x + (if (leader.x - follower.x >= 0) -1 else 1),
                    leader.y - follower.y + (if (leader.y - follower.y >= 0) -1 else 1)
                )
                Tail(epoch + 1, currentCoord moveBy move)
            }
            case (leader, follower) if abs(leader.x - follower.x) > 1 => {
                val move: Move = Move(
                    leader.x - follower.x + (if (leader.x - follower.x >= 0) -1 else 1),
                    leader.y - follower.y // if x is big step, y is +/-1 and should align with head
                )
                Tail(epoch + 1, currentCoord moveBy move)
            } // move along X axis
            case (leader, follower) if abs(leader.y - follower.y) > 1 => {
                val move: Move = Move(
                    leader.x - follower.x, // if y is big step, x is +/-1 and should align with head
                    leader.y - follower.y + (if (leader.y - follower.y >= 0) -1 else 1)
                )
                Tail(epoch + 1, currentCoord moveBy move)
            }
        }
    }

    @tailrec
    def pullTail(leader: RopeLink, followers: List[Tail], accuNewFollowers: List[Tail] = List()): List[Tail] = followers match {
        case Nil => accuNewFollowers.reverse
        case follower :: rest => {
            val newFollower: Tail = follower follow leader
            pullTail(newFollower, rest, newFollower :: accuNewFollowers)
        }
    }

    @tailrec
    def ropeMover(moves: List[Move], currentHead: Head, currentTailLinks: List[Tail], accuHeads: List[Head], accuTails: List[List[Tail]]): (List[Head], List[List[Tail]]) = moves match {
        case Nil => (accuHeads, accuTails)
        case currentMove :: rest => {
            val newHead: Head = currentHead leadWith currentMove
            val newTailLinks: List[Tail] = pullTail(newHead, currentTailLinks)

            ropeMover(rest, newHead, newTailLinks, newHead :: accuHeads, newTailLinks :: accuTails)
        }
    }

    val startHead = Head(epoch = 1, Coordinates(0, 0))

    def lastTailLinkCoordSize(tails: List[List[Tail]]): Int = tails.map(_.reverse.head.currentCoord).distinct.size

    // Part 1
    val startTail = List(Tail(epoch = 1, Coordinates(0, 0)))
    val (heads, tails) = ropeMover(stepMoves, startHead, startTail, List(startHead), List(startTail))
    val tailCoordinateDistinctCount = lastTailLinkCoordSize(tails)
    println(s"Tail has visited ${tailCoordinateDistinctCount} coordinates at least once.")

    // Part 2
    val startTail2 = (1 to 9).toList.map(_ => Tail(epoch = 1, Coordinates(0, 0)))
    val (heads2, tails2) = ropeMover(stepMoves, startHead, startTail2, List(startHead), List(startTail2))
    val tailCoordinateDistinctCount2 = lastTailLinkCoordSize(tails2)
    println(s"Last link of the long tail has visited ${tailCoordinateDistinctCount2} coordinates at least once.")
}
