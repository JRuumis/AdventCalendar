import scala.annotation.tailrec

object Day5Seed2 extends App {

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

    //case class IntervalBoundaryPoint(id: Long, isStart: Boolean)

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

            //println(s"Interval touch points: $intervalTouchPoints")
            //println(s"Touch intervals: $touchIntervals")

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

            //println(s"fullyIncludedIntervalMins: $fullyIncludedIntervalMins")

            val partiallyIncludedIntervalMin: Vector[Long] = {
                val partialInterval: Option[Interval] = intervals.find(i => rangeStartId > i.sourceStartId && rangeStartId <= i.sourceFinishId) // range can start inside one interval only
                if(partialInterval.isDefined) Vector(partialInterval.get.delta + rangeStartId) else Vector()
            }

            //println(s"partiallyIncludedIntervalMin: $partiallyIncludedIntervalMin")


            val firstInRangeNotInIntervalCandidates: Vector[Long] = Vector(rangeStartId) ++ intervals.flatMap(i => Vector(i.sourceStartId - 1, i.sourceFinishId + 1))
            val firstInRangeNotInInterval: Vector[Long] = firstInRangeNotInIntervalCandidates
                .filter(c => c >= rangeStartId && c <= rangeFinishId)
                .filter(c => this.intervals.find(i => i.sourceStartId <= c && i.sourceFinishId >= c).isEmpty)

            //println(s"firstInRangeNotInIntervalCandidates: $firstInRangeNotInIntervalCandidates")
            //println(s"firstInRangeNotInInterval: $firstInRangeNotInInterval")


            val allMinCandidates: Vector[Long] = fullyIncludedIntervalMins ++ partiallyIncludedIntervalMin ++ firstInRangeNotInInterval

            allMinCandidates.min
        }
    }

    @tailrec
    def extractor(
                     sourceLinesRaw: Vector[String],
                     currentGroupIntervals: Vector[Interval] = Vector(),
                     currentGroupSourceName: String = "",
                     currentGroupTargetName: String = "",
                     accuIntervalGroups: Vector[IntervalGroup] = Vector()): Vector[IntervalGroup] = sourceLinesRaw match {

        case Vector() if !currentGroupIntervals.isEmpty => accuIntervalGroups :+ IntervalGroup(currentGroupIntervals, Some(currentGroupSourceName), Some(currentGroupTargetName))
        case Vector() => accuIntervalGroups

        case s"$sourceName-to-$targetName map:" +: rest if !currentGroupIntervals.isEmpty =>
            extractor(rest, Vector(), sourceName, targetName, accuIntervalGroups :+ IntervalGroup(currentGroupIntervals, Some(currentGroupSourceName), Some(currentGroupTargetName)))

        case s"$sourceName-to-$targetName map:" +: rest =>
            extractor(rest, Vector(), sourceName, targetName, accuIntervalGroups)

        case s"$targetId $sourceId $steps" +: rest if targetId.forall(_.isDigit) && sourceId.forall(_.isDigit) && steps.forall((_.isDigit)) =>
            extractor(rest, currentGroupIntervals :+ Interval(targetId.toLong, sourceId.toLong, steps.toLong), currentGroupSourceName, currentGroupTargetName, accuIntervalGroups)

        case x => {
            println(s"Error: unrecognised input row format: $x")
            Vector()
        }
    }

    val transformations: Vector[IntervalGroup] = extractor(sourceLinesRaw.tail)
    val singleTransformation: IntervalGroup = transformations.reduce(_ + _)

    // Part One
    val lowestLocationNumber: Long = seeds.map(singleTransformation.transform(_)).min
    println(s"Lowest location number is $lowestLocationNumber")


    // Part Two
    println

    @tailrec
    def buu2(seeds: Vector[Long], smallest: Option[Long] = None): Option[Long] = seeds match {
        case Vector() => smallest
        case s1 +: s2 +: rest => {

            val xxx: Long = singleTransformation.getMinInRange(s1, s1+s2)
            println(s"Local minimum for $s1 and $s2: $xxx")

            buu2(rest, if (!smallest.isDefined || (smallest.isDefined && smallest.get >= xxx)) Some(xxx) else smallest)

            /*
            //println("---")
            val xxx = (0L to s2 - 1L).map(_ + s1)
            //println("----")

            //val yyy = xxx.map(x => longSearcher(x))
            val yyy = xxx.foldLeft(-1L)((a, b) => {

                val zzz = longSearcher(b)
                if (a == -1 || a > zzz) zzz else a
            })
            println(s"-----$yyy")

            buu2(rest, if (!smallest.isDefined || (smallest.isDefined && smallest.get >= yyy)) Some(yyy) else smallest)

             */
        }
    }

    val lowestLocationFromRange: Long = buu2(seeds).getOrElse(-1L)
    println(s"Lowest location number from range is $$lowestLocationFromRange")




    /*

    def searcher(dict: Vector[Interval], inputSourceId: Long): Long = {
        val foundInterval: Option[Interval] = dict.find(i => inputSourceId >= i.sourceId && inputSourceId < i.sourceId + i.steps)

        foundInterval match {
            case None => inputSourceId
            case Some(interval) => interval.targetId + (inputSourceId - interval.sourceId)
        }
    }

    val sameCache: scala.collection.mutable.Set[Long] = scala.collection.mutable.Set()
    var cache: scala.collection.mutable.Map[Long,Long] = scala.collection.mutable.Map[Long,Long]()

    def longSearcher(seed: Long): Long = {
        if(cache.isDefinedAt(seed)) cache(seed) else
        if(sameCache contains seed) seed else {
            val a1 = searcher(transformations(Connection("seed", "soil")), seed)
            val a2 = searcher(transformations(Connection("soil", "fertilizer")), a1)
            val a3 = searcher(transformations(Connection("fertilizer", "water")), a2)
            val a4 = searcher(transformations(Connection("water", "light")), a3)
            val a5 = searcher(transformations(Connection("light", "temperature")), a4)
            val a6 = searcher(transformations(Connection("temperature", "humidity")), a5)
            val a7 = searcher(transformations(Connection("humidity", "location")), a6)

            if(cache == a7)
                sameCache.add(seed)
            else
                cache(seed) = a7

            a7
        }
    }

    // Part One
    val a1 = seeds.map(s => searcher(transformations(Connection("seed","soil")), s))
    val a2 = a1.map(s => searcher(transformations(Connection("soil","fertilizer")), s))
    val a3 = a2.map(s => searcher(transformations(Connection("fertilizer","water")), s))
    val a4 = a3.map(s => searcher(transformations(Connection("water","light")), s))
    val a5 = a4.map(s => searcher(transformations(Connection("light","temperature")), s))
    val a6 = a5.map(s => searcher(transformations(Connection("temperature","humidity")), s))
    val a7 = a6.map(s => searcher(transformations(Connection("humidity","location")), s))

    println(a7.min)

    // part Two




    @tailrec
    def buu2(seeds: Vector[Long], smallest: Option[Long] = None): Option[Long] = seeds match {
        case Vector() => smallest
        case s1 +: s2 +: rest => {

            println("---")
            val xxx = (0L to s2 - 1L).map(_ + s1)
            println("----")

            //val yyy = xxx.map(x => longSearcher(x))
            val yyy = xxx.foldLeft(-1L)((a,b)=> {

                val zzz = longSearcher(b)
                if(a == -1 || a > zzz) zzz else a
            })
            println(s"-----$yyy")

            buu2(rest, if (!smallest.isDefined || (smallest.isDefined && smallest.get >= yyy)) Some(yyy) else smallest)
        }
    }

    val seeds2 = buu2(seeds)
    println(seeds2)



     */

    //350004378
    //440300553
    //201731623
    //503320162
    //47238414
    //195554894
    //24261545
    //333834851
    //2942405136
    //

}
