package jruumis.adventofcode.year2023

import scala.annotation.tailrec

object Day05Seed extends App {

    val sourceLinesRaw: Vector[String] = scala.io.Source.fromFile("./Sources/2023/Day5Seed.txt").getLines().toVector.filter(_ != "")

    val seeds: Vector[Long] = sourceLinesRaw.head match {
        case s"seeds: $seedList" => seedList.split(" ").toVector.map(_.toLong)
        case x => {
            println(s"Error when extracting seeds. Line: $x")
            Vector()
        }
    }

    case class Interval(targetId: Long, sourceId: Long, length: Long) {
        val sourceStartId: Long = sourceId
        val sourceFinishId: Long = sourceId + length - 1
        val targetStartId: Long = targetId
        val targetFinishId: Long = targetId + length - 1
        val delta: Long = targetId - sourceId

        override def toString: String = s"[$sourceStartId..$sourceFinishId -> $targetStartId..$targetFinishId (length: $length, delta: $delta)]"
    }
    object Interval {
        def createFromIntersection(intersectionStartId: Long, intersectionFinishId: Long, sourceInterval: Interval, targetInterval: Interval): Interval = {
            val newSourceId: Long = intersectionStartId - sourceInterval.delta
            val newTargetId: Long = intersectionStartId + targetInterval.delta
            val newLength: Long = intersectionFinishId - intersectionStartId + 1

            Interval(newTargetId, newSourceId, newLength)
        }

        def createFromSourceOnly(intersectionStartId: Long, intersectionFinishId: Long, sourceInterval: Interval): Interval = {
            val newSourceId: Long = intersectionStartId - sourceInterval.delta
            val newTargetId: Long = intersectionStartId
            val newLength: Long = intersectionFinishId - intersectionStartId + 1

            Interval(newTargetId, newSourceId, newLength)
        }

        def createFromTargetOnly(intersectionStartId: Long, intersectionFinishId: Long, targetInterval: Interval): Interval = {
            val newSourceId: Long = intersectionStartId
            val newTargetId: Long = intersectionStartId + targetInterval.delta
            val newLength: Long = intersectionFinishId - intersectionStartId + 1

            Interval(newTargetId, newSourceId, newLength)
        }
    }

    case class IntervalGroup(intervals: Vector[Interval], sourceName: Option[String] = None, targetName: Option[String] = None) {

        private def merger(sourceIntervals: Vector[Interval], targetIntervals: Vector[Interval]): Vector[Interval] =  {

            val intervalTouchPoints: Vector[(Long,Long)] =  (
                    sourceIntervals.map(si => (si.targetStartId,1L)) ++
                    sourceIntervals.map(si => (si.targetFinishId,2L)) ++
                    targetIntervals.map(si => (si.sourceStartId,1L)) ++
                    targetIntervals.map(si => (si.sourceFinishId,2L))
                ).sortBy{case(a,b) => 10*a+b}.distinct

            val touchIntervals: Vector[(Long, Long)] = (intervalTouchPoints zip intervalTouchPoints.tail).map{_ match {
                case ((a,1L),(b,2L)) => (a,b)
                case ((a,1L),(b,1L)) => (a,b-1)
                case ((a,2L),(b,2L)) => (a+1,b)
                case ((a,2L),(b,1L)) => (a+1,b-1)
            }}.filter{case(x,y) => x <= y}

            val newIntervals: Vector[Interval] = touchIntervals.map{ case(touchStart,touchFinish) => {

                val newSourceInterval: Option[Interval] = sourceIntervals.find(si => si.targetStartId <= touchStart && si.targetFinishId >= touchFinish)
                val newTargetInterval: Option[Interval] = targetIntervals.find(ti => ti.sourceStartId <= touchStart && ti.sourceFinishId >= touchFinish)

                val newInterval: Option[Interval] = (newSourceInterval, newTargetInterval) match {
                    case (Some(sourceInterval), Some(targetInterval)) =>
                        Some(Interval.createFromIntersection(touchStart, touchFinish, sourceInterval, targetInterval))

                    case (None, Some(targetInterval)) =>
                        Some(Interval.createFromTargetOnly(touchStart, touchFinish, targetInterval))

                    case (Some(sourceInterval), None) =>
                        Some(Interval.createFromSourceOnly(touchStart, touchFinish, sourceInterval))

                    case (None, None) => {
                        println("Error - match not found")
                        None
                    }
                }

                newInterval
            }}.collect {case(Some(interval)) => interval}

            newIntervals
        }

        def +(next: IntervalGroup): IntervalGroup = {
            IntervalGroup(merger(this.intervals, next.intervals))
        }

        def transform(input: Long): Long = {
            val interval = this.intervals.find(i => i.sourceStartId <= input && i.sourceFinishId >= input)
            if(interval.isDefined) input + interval.get.delta else input
        }

        def getMinInRange(rangeStartId: Long, rangeFinishId: Long): Long = {

            val fullyIncludedIntervalMins: Vector[Long] =
                intervals.filter(i => rangeStartId <= i.sourceStartId && rangeFinishId >= i.sourceStartId).map(_.targetStartId)

            val partiallyIncludedIntervalMin: Vector[Long] = {
                val partialInterval: Option[Interval] = intervals.find(i => rangeStartId > i.sourceStartId && rangeStartId <= i.sourceFinishId) // range can start inside one interval only
                if(partialInterval.isDefined) Vector(partialInterval.get.delta + rangeStartId) else Vector()
            }

            val firstInRangeNotInIntervalCandidates: Vector[Long] = Vector(rangeStartId) ++ intervals.flatMap(i => Vector(i.sourceStartId - 1, i.sourceFinishId + 1))
            val firstInRangeNotInInterval: Vector[Long] = firstInRangeNotInIntervalCandidates
                .filter(c => c >= rangeStartId && c <= rangeFinishId)
                .filter(c => this.intervals.find(i => i.sourceStartId <= c && i.sourceFinishId >= c).isEmpty)

            val allMinCandidates: Vector[Long] = fullyIncludedIntervalMins ++ partiallyIncludedIntervalMin ++ firstInRangeNotInInterval

            allMinCandidates.min
        }
    }

    @tailrec
    def inputExtractor(
                     sourceLinesRaw: Vector[String],
                     currentGroupIntervals: Vector[Interval] = Vector(),
                     currentGroupSourceName: String = "",
                     currentGroupTargetName: String = "",
                     accuIntervalGroups: Vector[IntervalGroup] = Vector()): Vector[IntervalGroup] = sourceLinesRaw match {

        case Vector() if !currentGroupIntervals.isEmpty => accuIntervalGroups :+ IntervalGroup(currentGroupIntervals, Some(currentGroupSourceName), Some(currentGroupTargetName))
        case Vector() => accuIntervalGroups

        case s"$sourceName-to-$targetName map:" +: rest if !currentGroupIntervals.isEmpty =>
            inputExtractor(rest, Vector(), sourceName, targetName, accuIntervalGroups :+ IntervalGroup(currentGroupIntervals, Some(currentGroupSourceName), Some(currentGroupTargetName)))

        case s"$sourceName-to-$targetName map:" +: rest =>
            inputExtractor(rest, Vector(), sourceName, targetName, accuIntervalGroups)

        case s"$targetId $sourceId $steps" +: rest if targetId.forall(_.isDigit) && sourceId.forall(_.isDigit) && steps.forall((_.isDigit)) =>
            inputExtractor(rest, currentGroupIntervals :+ Interval(targetId.toLong, sourceId.toLong, steps.toLong), currentGroupSourceName, currentGroupTargetName, accuIntervalGroups)

        case x => {
            println(s"Error: unrecognised input row format: $x")
            Vector()
        }
    }

    val transformations: Vector[IntervalGroup] = inputExtractor(sourceLinesRaw.tail)
    val singleTransformation: IntervalGroup = transformations.reduce(_ + _)


    // Part One
    val lowestLocationNumber: Long = seeds.map(singleTransformation.transform(_)).min
    println(s"Lowest location number is $lowestLocationNumber")

    // Part Two
    @tailrec
    def getMinFromIntervals(seeds: Vector[Long], smallest: Option[Long] = None): Option[Long] = seeds match {
        case Vector() => smallest
        case s1 +: s2 +: rest => {

            val localMin: Long = singleTransformation.getMinInRange(s1, s1+s2)
            //println(s"\tLocal minimum for $s1 and $s2: $localMin")

            getMinFromIntervals(rest, if (!smallest.isDefined || (smallest.isDefined && smallest.get >= localMin)) Some(localMin) else smallest)
        }
    }

    val lowestLocationFromRange: Long = getMinFromIntervals(seeds).getOrElse(-1L)
    println(s"Lowest location number from range is $lowestLocationFromRange")
}