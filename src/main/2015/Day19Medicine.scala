import scala.util.matching.Regex

object Day19Medicine extends App {

    type Molecule = String

    val medicineRaw: Vector[String] = scala.io.Source.fromFile("./Sources/2015/Day19Medicine.txt").getLines.toVector
    val transformationsRaw: Vector[String] = medicineRaw.slice(0, medicineRaw.length-2)
    val molecule:Molecule = medicineRaw.last

    val transformationPattern: Regex = """([A-Za-z]+) => ([A-Za-z]+)""".r
    val transformations: Vector[(Molecule, Molecule)] = transformationsRaw.map(_ match {
        case transformationPattern(a,b) => (a,b)
    })

    def replacer(source: Molecule, searchFor: Molecule, replaceWith: Molecule, visitedMolecules: Set[Molecule] = Set()): Set[Molecule] = {
        val searchForLength = searchFor.length

        (0 to (source.length - searchForLength)).map(i => {
            if(source.slice(i, i+searchForLength) == searchFor) {
                val before: Molecule = if(i == 0) "" else source.slice(0,i)
                val after: Molecule = if(i == (source.length - searchForLength)) "" else source.slice(i+searchForLength,source.length)
                val newMolecule: Molecule = before + replaceWith + after

                if(visitedMolecules contains newMolecule) None
                else Some(before + replaceWith + after)
            } else None
        }).filter(_.isDefined).map(_.get).toSet
    }

    def replace(source: Molecule, backwards: Boolean = false, visitedMolecules: Set[Molecule] = Set()): Set[Molecule] = {
        transformations.map{_ match {
            case(from, to) if !backwards => replacer(source, from, to, visitedMolecules)
            case(to, from) if backwards => replacer(source, from, to, visitedMolecules)
        }}.reduce(_ ++ _)
    }

    val calibration: Set[Molecule] = replace(molecule)
    println(s"Calibration transformations: ${calibration.size}")

    val partTwoStartMolecule: Molecule = "e"

    def search(currentMolecules: Vector[Molecule], visitedMolecules: Set[Molecule] = Set(), epoch: Int = 0): Option[Int] = currentMolecules match {
        case Vector() => None
        case currentMolecule +: rest => {
            if(currentMolecule == partTwoStartMolecule) Some(epoch)
            else {
                val nextMolecules: Vector[Molecule] = replace(currentMolecule, true, visitedMolecules).toVector.sortBy(m => m.size)
                val nextFound: Option[Int] = search(nextMolecules, visitedMolecules ++ currentMolecules, epoch+1)

                if(nextFound.isDefined) nextFound
                else search(rest, visitedMolecules + currentMolecule, epoch)
            }
        }
    }

    val generationsToFind: Int = search(Vector(molecule)).get
    println(s"Molecule found after ${generationsToFind} generations.")
}