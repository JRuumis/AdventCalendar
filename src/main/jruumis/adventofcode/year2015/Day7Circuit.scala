package jruumis.adventofcode.year2015

import sun.rmi.transport.Target

import scala.util.matching.Regex

object Day7Circuit extends App {

    val circuitRaw: Vector[String] = scala.io.Source.fromFile("./Sources/2015/Day7Circuit.txt").getLines().toVector

    //println(circuitRaw)

    val assignPattern: Regex = """([a-z]+|[0-9]+) -> ([a-z]+)""".r
    val andPattern: Regex = """([a-z]+|[0-9]+) AND ([a-z]+|[0-9]+) -> ([a-z]+)""".r
    val orPattern: Regex = """([a-z]+|[0-9]+) OR ([a-z]+|[0-9]+) -> ([a-z]+)""".r
    val lShiftPattern: Regex = """([a-z]+|[0-9]+) LSHIFT ([a-z]+|[0-9]+) -> ([a-z]+)""".r
    val rShiftPattern: Regex = """([a-z]+|[0-9]+) RSHIFT ([a-z]+|[0-9]+) -> ([a-z]+)""".r
    val notPattern: Regex = """NOT ([a-z]+|[0-9]+) -> ([a-z]+)""".r

    abstract sealed class Value
    case class Variable(name: String) extends Value
    case class Integer(i: Short) extends Value

    abstract sealed class Operation {
        val t: Variable
        def getTarget(targets: Map[String,Integer]): Option[Integer]
    }

    case class Assign(a: Value, t: Variable) extends Operation {
        override def getTarget(targets: Map[String,Integer]): Option[Integer] = a match {
            case Variable(name) => targets.get(name)
            case int @ Integer(_) => Some(int)
        }
    }

    //val a: Integer = Integer(123)
    //val b: Integer = Integer(125)
    //val c: Short = (a.i & b.i).toShort



    case class And(a: Value, b: Value, t: Variable) extends Operation {
        override def getTarget(targets: Map[String, Integer]): Option[Integer] = (a,b) match {
            case (Variable(name1), Variable(name2)) => if(targets.get(name1).isDefined && targets.get(name2).isDefined) Some(Integer((targets(name1).i & targets(name2).i).toShort)) else None
            case (Variable(name1), Integer(int2)) => if(targets.get(name1).isDefined) Some(Integer((targets(name1).i & int2).toShort)) else None
            case (Integer(int1), Variable(name2)) => if(targets.get(name2).isDefined) Some(Integer((targets(name2).i & int1).toShort)) else None
            case (Integer(int1), Integer(int2)) => Some(Integer((int1 & int2).toShort))
        }
    }

    case class Or(a: Value, b: Value, t: Variable) extends Operation {
        override def getTarget(targets: Map[String, Integer]): Option[Integer] = (a,b) match {
            case (Variable(name1), Variable(name2)) => if(targets.get(name1).isDefined && targets.get(name2).isDefined) Some(Integer((targets(name1).i | targets(name2).i).toShort)) else None
            case (Variable(name1), Integer(int2)) => if(targets.get(name1).isDefined) Some(Integer((targets(name1).i | int2).toShort)) else None
            case (Integer(int1), Variable(name2)) => if(targets.get(name2).isDefined) Some(Integer((targets(name2).i | int1).toShort)) else None
            case (Integer(int1), Integer(int2)) => Some(Integer((int1 | int2).toShort))
        }
    }

    case class LShift(a: Value, b: Value, t: Variable) extends Operation {
        override def getTarget(targets: Map[String, Integer]): Option[Integer] = (a,b) match {
            case (Variable(name1), Variable(name2)) => if(targets.get(name1).isDefined && targets.get(name2).isDefined) Some(Integer((targets(name1).i << targets(name2).i).toShort)) else None
            case (Variable(name1), Integer(int2)) => if(targets.get(name1).isDefined) Some(Integer((targets(name1).i << int2).toShort)) else None
            case (Integer(int1), Variable(name2)) => if(targets.get(name2).isDefined) Some(Integer((targets(name2).i << int1).toShort)) else None
            case (Integer(int1), Integer(int2)) => Some(Integer((int1 << int2).toShort))
        }
    }

    case class RShift(a: Value, b: Value, t: Variable) extends Operation {
        override def getTarget(targets: Map[String, Integer]): Option[Integer] = (a,b) match {
            case (Variable(name1), Variable(name2)) => if(targets.get(name1).isDefined && targets.get(name2).isDefined) Some(Integer((targets(name1).i >> targets(name2).i).toShort)) else None
            case (Variable(name1), Integer(int2)) => if(targets.get(name1).isDefined) Some(Integer((targets(name1).i >> int2).toShort)) else None
            case (Integer(int1), Variable(name2)) => if(targets.get(name2).isDefined) Some(Integer((targets(name2).i >> int1).toShort)) else None
            case (Integer(int1), Integer(int2)) => Some(Integer((int1 >> int2).toShort))
        }
    }

    case class Not(a: Value, t: Variable) extends Operation {
        override def getTarget(targets: Map[String,Integer]): Option[Integer] = a match {
            case Variable(name) => if(targets.get(name).isDefined) Some(Integer((~(targets(name).i)).toShort)) else None
            case Integer(int) => Some(Integer((~int).toShort))
        }
    }


    def getValue(s: String): Value = {
        if(s.forall(_.isDigit)) Integer(s.toShort)
        else Variable(s)
    }

    val startCircuit: Vector[Operation] = circuitRaw.map(c => c match {
        case assignPattern(a,b) => Assign(getValue(a), Variable(b))
        case andPattern(a,b,c) => And(getValue(a), getValue(b), Variable(c))
        case orPattern(a,b,c) => Or(getValue(a), getValue(b), Variable(c))
        case lShiftPattern(a,b,c) => LShift(getValue(a), getValue(b), Variable(c))
        case rShiftPattern(a,b,c) => RShift(getValue(a), getValue(b), Variable(c))
        case notPattern(a,b) => Not(getValue(a), Variable(b))
    })

    println(startCircuit.mkString("\n"))



    def iterate(currentCircuits: Vector[Operation], targets: Map[String,Integer] = Map()): Map[String,Integer] = currentCircuits match {
        case Vector() => targets
        case circuit +: rest => {
            val circuitVal: Option[Integer] = circuit.getTarget(targets)

            if(circuitVal.isDefined) iterate(rest, targets + (circuit.t.name -> circuitVal.get))
            else iterate(rest :+ circuit, targets)
        }
    }

    val iteratedCircuit: Map[String, Integer] = iterate(startCircuit)
    val iteratedCircuitOutput: Vector[String] = iteratedCircuit.map{case(a,b) => s"${a}: ${if(b.i < 0) 65536+b.i else b.i }" }.toVector.sorted

    //println(iteratedCircuitOutput.mkString("\n"))
    println(iteratedCircuitOutput.head)

    val signalA: Short = 16076

    val startCircuit2: Vector[Operation] = (startCircuit.toSet - Assign(Integer(19138),Variable("b")) + Assign(Integer(16076),Variable("b"))).toVector

    val iteratedCircuit2: Map[String, Integer] = iterate(startCircuit2)
    val iteratedCircuitOutput2: Vector[String] = iteratedCircuit2.map{case(a,b) => s"${a}: ${if(b.i < 0) 65536+b.i else b.i }" }.toVector.sorted

    println(iteratedCircuitOutput2.head)

}
