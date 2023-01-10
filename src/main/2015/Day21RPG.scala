import scala.annotation.tailrec
import scala.util.matching.Regex

object Day21RPG extends App {

    val weaponsRaw: Vector[String] = "Dagger        8     4       0\nShortsword   10     5       0\nWarhammer    25     6       0\nLongsword    40     7       0\nGreataxe     74     8       0".split("\n").toVector
    val armorRaw: Vector[String] = "Leather      13     0       1\nChainmail    31     0       2\nSplintmail   53     0       3\nBandedmail   75     0       4\nPlatemail   102     0       5".split("\n").toVector
    val ringsRaw: Vector[String] = "Damage+1    25     1       0\nDamage+2    50     2       0\nDamage+3   100     3       0\nDefense+1   20     0       1\nDefense+2   40     0       2\nDefense+3   80     0       3".split("\n").toVector

    val itemPattern: Regex = """([A-Za-z0-9\+]+)[ ]+([0-9]+)[ ]+([0-9]+)[ ]+([0-9]+)""".r

    abstract sealed class Item {
        val name: String
        val cost: Int
        val damage: Int
        val armor: Int
    }
    case class Weapon(name: String, cost: Int, damage: Int, armor: Int) extends Item
    case class Armor(name: String, cost: Int, damage: Int, armor: Int) extends Item
    case class Ring(name: String, cost: Int, damage: Int, armor: Int) extends Item

    val weapons: Vector[Weapon] = weaponsRaw.map(_ match {
        case itemPattern(a,b,c,d) => Weapon(a,b.toInt,c.toInt,d.toInt)
    }) //:+ Weapon("None",0,0,0) // weapon is mandatory!

    val armor: Vector[Armor] = armorRaw.map(_ match {
        case itemPattern(a,b,c,d) => Armor(a,b.toInt,c.toInt,d.toInt)
    }) :+ Armor("None",0,0,0)

    val rings: Vector[Ring] = ringsRaw.map(_ match {
        case itemPattern(a,b,c,d) => Ring(a,b.toInt,c.toInt,d.toInt)
    }) :+ Ring("None",0,0,0)

    val bossHitPoints: Int = 104
    val bossDamage: Int = 8
    val bossArmor: Int = 1
    val playerHitpoints: Int = 100

    val equipmentCombos: Vector[(Weapon, Armor, Ring, Ring, Int, Int, Int)] = weapons.flatMap(weapon => armor.flatMap(armor => rings.flatMap(ring1 => rings.map(ring2 => {
        if(ring1 != ring2 || ring1 == Ring("None",0,0,0)) // can have 0 to 2 different rings
            Some((
                    weapon,armor,ring1,ring2,
                    weapon.cost + armor.cost + ring1.cost + ring2.cost,
                    weapon.damage + armor.damage + ring1.damage + ring2.damage,
                    weapon.armor + armor.armor + ring1.armor + ring2.armor
                ))
        else None
    })))).filter(_.isDefined).map(_.get).sortBy{case(_,_,_,_,cost,_,_) => cost}

    @tailrec
    def iterateFight(
                        currentPlayerHitpoints: Int,
                        playerDamage: Int,
                        playerArmor: Int,
                        currentBossHitpoints: Int,
                        currentMovePlayer: Boolean = true
                    ): Boolean = (currentPlayerHitpoints, currentBossHitpoints) match {
        case (php,_) if php <= 0 => false
        case (_,bhp) if bhp <= 0 => true
        case (_,_) if currentMovePlayer => {
            val damage: Int = if(playerDamage - bossArmor < 1) 1 else playerDamage - bossArmor
            iterateFight(currentPlayerHitpoints, playerDamage, playerArmor, currentBossHitpoints - damage, !currentMovePlayer)
        }
        case (_,_) if !currentMovePlayer => {
            val damage: Int = if(bossDamage - playerArmor < 1) 1 else bossDamage - playerArmor
            iterateFight(currentPlayerHitpoints - damage, playerDamage, playerArmor, currentBossHitpoints, !currentMovePlayer)
        }
    }

    @tailrec
    def checkEquipmentCombo(
                               equipCombos: Vector[(Weapon, Armor, Ring, Ring, Int, Int, Int)],
                               requireWin: Boolean = true
                           ): Int = equipCombos match {
        case Vector() => -1 // should not happen
        case (_,_,_,_,cost,playerDamage,playerArmor) +: rest => {
            if(iterateFight(playerHitpoints, playerDamage, playerArmor, bossHitPoints) == requireWin) cost
            else checkEquipmentCombo(rest, requireWin)
        }
    }

    val minCostToWin: Int = checkEquipmentCombo(equipmentCombos)
    println(s"Minimum cost to win: ${minCostToWin}")

    // Part 2
    val equipCombosRev = equipmentCombos.reverse
    val maxCostToLose: Int = checkEquipmentCombo(equipCombosRev, requireWin = false)
    println(s"Maximum cost to lose: ${maxCostToLose}")
}