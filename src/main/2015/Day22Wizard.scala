object Day22Wizard extends App {

    abstract sealed class Spell {
        val manaCost: Int
        val damage: Int
        val armor: Int
        val heal: Int
        val manaRecharge: Int
    }
    abstract sealed class InstantSpell extends Spell
    abstract sealed class TurnSpell extends Spell {
        val turns: Option[Int]
    }

    case class MagicMissile(manaCost: Int = 53, damage: Int = 4, armor: Int = 0, heal: Int = 0, manaRecharge: Int = 0) extends InstantSpell
    case class Drain(manaCost: Int = 73, damage: Int = 2, armor: Int = 0, heal: Int = 2, manaRecharge: Int = 0) extends InstantSpell

    case class Shield(manaCost: Int = 113, damage: Int = 0, armor: Int = 7, heal: Int = 0, manaRecharge: Int = 0, turns: Option[Int] = Some(6)) extends TurnSpell {
        def turn: Shield = Shield(manaCost, damage, armor, heal, manaRecharge, if(turns.isDefined && turns.get > 1) Some(turns.get-1) else None)
    }
    case class Poison(manaCost: Int = 173, damage: Int = 3, armor: Int = 0, heal: Int = 0, manaRecharge: Int = 0, turns: Option[Int] = Some(6)) extends TurnSpell {
        def turn: Poison = Poison(manaCost, damage, armor, heal, manaRecharge, if(turns.isDefined && turns.get > 1) Some(turns.get-1) else None)
    }
    case class Recharge(manaCost: Int = 229, damage: Int = 0, armor: Int = 0, heal: Int = 0, manaRecharge: Int = 101, turns: Option[Int] = Some(5)) extends TurnSpell {
        def turn: Recharge = Recharge(manaCost, damage, armor, heal, manaRecharge, if(turns.isDefined && turns.get > 1) Some(turns.get-1) else None)
    }

    val allSpells: Vector[Spell] = Vector(MagicMissile(), Drain(), Shield(), Poison(), Recharge())

    val playerHitPoints: Int = 50
    val playerMana: Int = 500

    val bossHitPoints: Int = 104
    val bossDamage: Int = 8
    val bossArmor: Int = 1

    def gameTree(
                    currentPlayerHitPoints: Int,
                    currentPlayerMana: Int,
                    currentBossHitPoints: Int,
                    manaSpent: Int,
                    activeTurnSpells: Vector[Spell],
                    playerTurn: Boolean // !!!!!!!!!!
                ): (Boolean, Int) = (currentPlayerHitPoints, currentBossHitPoints) match {
        case (php,_) if php <= 0 => (false, manaSpent)
        case (_,bhp) if bhp <= 0 => (true, manaSpent)
        //case (_,_) if currentPlayerMana == 0 => (false, manaSpent)

        /*
        case (_,_) => {

            val (turnDamage: Int, turnArmor: Int, turnMana: Int) = activeTurnSpells.map(s => (s.damage, s.armor, s.manaRecharge)).reduce{case((d1,a1,m1),(d2,a2,m2)) => (d1+d2, a1+a2, m1+m2)}


            val afterTurnBossHitPoints = if(turnDamage > 0 && turnDamage - bossArmor) < 1) 1 else turnDamage - bossArmor
            val afterTurnArmor = turnArmor
            val afterTurnMana = currentPlayerMana + turnMana

            if(afterTurnMana <= 0) (false, manaSpent)
            else {
                val availableSpells: Vector[Spell] = allSpells.filter(s => s.manaCost <= playerMana)

                if(availableSpells == Vector()) (false, manaSpent)
                else {
                    availableSpells.map(s => s match {
                        case i if i.isInstanceOf[InstantSpell] => {

                        }


                        gameTree()
                    })
                }

            }


        }

         */
    }



}
