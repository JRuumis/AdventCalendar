package jruumis.adventofcode.year2023

import scala.annotation.tailrec

object Day19Aplenty extends App {

    val linesRaw = scala.io.Source.fromFile("./Sources/2023/Day19Aplenty.txt").getLines.toVector
    //val linesRaw = scala.io.Source.fromFile("./Sources/2023/Day19Aplenty_TEST1.txt").getLines.toVector

    // --------------------------
    //  Parsing
    // --------------------------
    sealed trait PartAction
    case object Accept extends PartAction
    case object Reject extends PartAction
    case class MoveToWorkflow(workflowId: String) extends PartAction

    sealed trait PartProperty
    case object X extends PartProperty
    case object M extends PartProperty
    case object A extends PartProperty
    case object S extends PartProperty

    def propertyParser(propertyString: String): PartProperty = propertyString match {
        case "x" => X
        case "m" => M
        case "a" => A
        case "s" => S
    }

    def partActionParser(partActionString: String): PartAction = partActionString match {
        case "A" => Accept
        case "R" => Reject
        case s"$workflowId" => MoveToWorkflow(workflowId)
    }

    case class Condition(partProperty: PartProperty, propertyChecker: Int => Boolean, splitInt: Option[Int], splitChar: Option[Char], moveTo: PartAction)

    @tailrec
    def conditionParser(conditions: Vector[String], accuConditions: Vector[Condition] = Vector()): Vector[Condition] = conditions match {
        case Vector() => accuConditions

        case s"$partProperty>$propertyValue:$moveTo" +: rest => {
            val newCondition = Condition(propertyParser(partProperty), (a: Int) => a > propertyValue.toInt, Some(propertyValue.toInt), Some('>'), partActionParser(moveTo))
            conditionParser(rest, accuConditions :+ newCondition)
        }

        case s"$partProperty<$propertyValue:$moveTo" +: rest => {
            val newCondition = Condition(propertyParser(partProperty), (a: Int) => a < propertyValue.toInt, Some(propertyValue.toInt), Some('<'), partActionParser(moveTo))
            conditionParser(rest, accuConditions :+ newCondition)
        }

        case s"$moveTo" +: rest => {
            val newCondition = Condition(X, (a: Int) => true, None, None, partActionParser(moveTo))
            conditionParser(rest, accuConditions :+ newCondition)
        }
    }

    case class Part(x: Int, m: Int, a: Int, s: Int) {
        val propertyValues: Map[PartProperty, Int] = Map(
            X -> x,
            M -> m,
            A -> a,
            S -> s
        )

        lazy val propertySum: Int = x + m + a + s
        override def toString: String = s"[x=$x,m=$m,a=$a,s=$s]"
    }


    // --------------------------
    //  Workflow
    // --------------------------
    case class Workflow(name: String, conditions: Vector[Condition]) {
        def checkPart(p: Part): Option[PartAction] = Workflow.partChecker(p, conditions)
    }
    object Workflow {

        @tailrec
        private def partChecker(part: Part, conditions: Vector[Condition]): Option[PartAction] = conditions match {
            case Vector() => None
            case (cond:Condition) +: rest => {

                val propertyValue: Int = part.propertyValues(cond.partProperty)
                val checkOutcome: Boolean = cond.propertyChecker(propertyValue)

                if (checkOutcome) {
                    Some(cond.moveTo)
                } else {
                    partChecker(part, rest)
                }
            }
        }
    }

    case class Workflows(flows: Vector[Workflow]) {
        val workflowIdFromName: Map[String, Int] = flows.zipWithIndex.map{case(a,i) => a.name -> i}.toMap
        val inWorkflow = flows(workflowIdFromName("in"))

        @tailrec
        final def checkPart(p: Part, workflowId: Int = workflowIdFromName("in")): Option[PartAction] = {
           val workflow: Workflow = flows(workflowId)

            workflow.checkPart(p) match {
               case Some(Accept) => Some(Accept)
               case Some(Reject) => Some(Reject)
               case Some(MoveToWorkflow(workflowName)) => checkPart(p, workflowIdFromName(workflowName))
               case None => checkPart(p, workflowId + 1)
           }
        }

        def getConditionsFromWorkflowName(workflowName: String): Vector[Condition] = flows(workflowIdFromName(workflowName)).conditions
        def splitRanges(ranges: Ranges, conditions: Vector[Condition]): Vector[Ranges] = Ranges.split(ranges, conditions, getConditionsFromWorkflowName)
    }


    // --------------------------
    // Ranges
    //  --------------------------
    case class RangePair(forTrue: Option[Range], forFalse: Option[Range])

    case class Range(from: Int, to: Int) {
        def splitWith(splitOp: Char, splitInt: Int): RangePair = splitOp match {
            case '>' if from > splitInt => RangePair(Some(this), None)
            case '>' if to <= splitInt => RangePair(None, Some(this))
            case '>' => RangePair( Some(Range(splitInt+1,to)), Some(Range(from,splitInt)) )

            case '<' if to < splitInt => RangePair(Some(this), None)
            case '<' if from >= splitInt => RangePair(None, Some(this))
            case '<' => RangePair( Some(Range(from,splitInt-1)), Some(Range(splitInt,to)) )
        }
    }

    case class RangesPair(forTrue: Option[Ranges], forFalse: Option[Ranges])

    case class Ranges(ranges: Map[PartProperty, Range]) {

        private def splitRangeWithCondition(condition: Condition): RangesPair = {
            (condition.splitInt, condition.splitChar) match {
                case (None, None) => RangesPair(Some(this), None) // true/false
                case (Some(splitInt), Some(splitChar)) => splitWith(condition.partProperty, splitChar, splitInt)
            }
        }

        private def splitWith(property: PartProperty, splitOp: Char, splitInt: Int): RangesPair = {

            val splitPair: RangePair = ranges(property).splitWith(splitOp, splitInt)

            val rangesForTrue: Option[Ranges]  = if (splitPair.forTrue.isDefined) Some(Ranges(ranges.updated(property, splitPair.forTrue.get))) else None
            val rangesForFalse: Option[Ranges] = if (splitPair.forFalse.isDefined) Some(Ranges(ranges.updated(property, splitPair.forFalse.get))) else None

            RangesPair(rangesForTrue, rangesForFalse)
        }

        lazy val combos: Long = ranges.values.map(v => (v.to - v.from + 1).toLong).product
    }
    object Ranges {

        def split(
                           curRanges: Ranges,
                           curConditions: Vector[Condition],
                           getConditions: (String) => Vector[Condition]
                       ): Vector[Ranges] = curConditions match {

            case Vector() => Vector()
            case (cond: Condition) +: rest => {
                val splitRangesPair: RangesPair = curRanges.splitRangeWithCondition(cond)

                    ((cond.moveTo, splitRangesPair.forTrue) match {
                        case (_, None) => Vector()
                        case (Reject, _) => Vector()
                        case (Accept, Some(rangeTrue)) => Vector(rangeTrue)
                        case (MoveToWorkflow(nextWorkflowName), Some(rangeTrue)) => split(rangeTrue, getConditions(nextWorkflowName), getConditions)
                    })
                    ++
                    (splitRangesPair.forFalse match {
                        case None => Vector()
                        case Some(rangeFalse) => split(rangeFalse, rest, getConditions)
                    })
            }
        }

    }


    // MAIN
    val workflows: Workflows = {
        val flows: Vector[Workflow] = linesRaw.collect{
            case(s"$workfowName{$conditions}") => Workflow(workfowName, conditionParser(conditions.split(",").toVector) )
        }

        Workflows(flows)
    }

    // Part One
    val parts: Vector[Part] = linesRaw.collect {
        case (s"{x=$partX,m=$partM,a=$partA,s=$partS}") => Part(partX.toInt, partM.toInt, partA.toInt, partS.toInt)
    }

    val partsAcceptedRejected: Vector[(Part, Option[PartAction])] = parts.map(p => (p, workflows.checkPart(p)))
    val acceptedPartsPropertySum: Int = partsAcceptedRejected.collect{ case(part, Some(Accept)) => part.propertySum }.sum
    println(s"Property Sums of all Accepted Parts is ${acceptedPartsPropertySum}")


    // Part Two
    val startRanges = Ranges(Map(
        X -> Range(1,4000),
        M -> Range(1,4000),
        A -> Range(1,4000),
        S -> Range(1,4000)
    ))

    val startConditions: Vector[Condition] = workflows.inWorkflow.conditions
    val acceptedRanges: Vector[Ranges] = workflows.splitRanges(startRanges, startConditions)
    val sumOfCombos: Long = acceptedRanges.map(_.combos).sum
    println(s"All possible Accepted combinations is is ${sumOfCombos}")
}