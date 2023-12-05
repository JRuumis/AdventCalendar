import scala.annotation.tailrec

object Day5Seed3 extends App {

    case class Interval(targetId: Long, sourceId: Long, steps: Long)

    case class Connection(sourceName: String, targetName: String)

    val sourceLinesRaw: Vector[String] = scala.io.Source.fromFile("./Sources/2023/Day5Seed.txt").getLines().toVector.filter(_ != "")

    println(sourceLinesRaw)

    val seeds: Vector[Long] = sourceLinesRaw.head match {
        case s"seeds: $seedList" => seedList.split(" ").toVector.map(_.toLong)
        case _ => Vector()
    }


    def extractor(vect: Vector[String], currentConnection: Option[Connection] = None, accu: Map[Connection,Vector[Interval]] = Map()):Map[Connection,Vector[Interval]] = vect match {
        case Vector() => accu
        case "\n" +: rest => extractor(rest, currentConnection, accu)
        case s"$sourceName-to-$targetName map:" +: rest => extractor(rest, Some(Connection(sourceName, targetName)), accu)
        case s"$targetId $sourceId $steps" +: rest
            if currentConnection.isDefined  =>
            extractor(rest, currentConnection, accu + (currentConnection.get -> (accu.getOrElse(currentConnection.get, Vector()) :+ Interval(targetId.toLong, sourceId.toLong, steps.toLong)) ))
        case a => {
            println(s"Error: $a")
            accu
        }
    }

    val transformations: Map[Connection, Vector[Interval]] = extractor(sourceLinesRaw.tail)

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
