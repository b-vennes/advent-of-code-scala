package aoc.events.y2023.d2

import aoc.*
import aoc.tools.*

case class Round(blue: Long, red: Long, green: Long):
    def isPossible(blueDice: Long, redDice: Long, greenDice: Long): Boolean =
        blue <= blueDice && red <= redDice && green <= greenDice

object Round:
    enum Color:
        case Blue
        case Green
        case Red

    case class Entry(color: Color, number: Long)

    val parseColor: Parse[Color] =
        Warp.calculate {
            case value if value.startsWith("blue") =>
                Warp.toLocation(Color.Blue -> value.drop("blue".length))
            case value if value.startsWith("red") =>
                Warp.toLocation(Color.Red -> value.drop("red".length))
            case value if value.startsWith("green") =>
                Warp.toLocation(Color.Green -> value.drop("green".length))
            case other =>
                Warp.doomed(RuntimeException(s"Color '$other' does not start with a color."))
        }

    val parseEntry: Parse[Entry] =
        Parse.unsignedNum
            .followedBy(Parse.word(" "))
            .followedBy(parseColor)
            .withParsed {
                case ((number, _), color) => Entry(color, number)
            }

    val parse: Parse[Round] =
        Parse.split(parseEntry, ", ")
            .withParsed(_.foldLeft(Round(0, 0, 0)) {
                case (round, Entry(Color.Blue, amount))  => round.copy(blue = amount)
                case (round, Entry(Color.Red, amount))   => round.copy(red = amount)
                case (round, Entry(Color.Green, amount)) => round.copy(green = amount)
            })

case class Power(maxBlue: Long, maxRed: Long, maxGreen: Long):

    private def max(left: Long, right: Long): Long =
        if right > left then right else left

    def updateBlue(blue: Long) = copy(maxBlue = max(blue, maxBlue))

    def updateRed(red: Long) = copy(maxRed = max(red, maxRed))

    def updateGreen(green: Long) = copy(maxGreen = max(green, maxGreen))

    val get: Long = maxBlue * maxRed * maxGreen

object Power:
    val starting: Power = Power(0, 0, 0)

case class Game(number: Long, rounds: List[Round]):
    def isPossible(blueDice: Long, redDice: Long, greenDice: Long): Boolean =
        rounds.forall(_.isPossible(blueDice, redDice, greenDice))

    def power: Long =
        rounds
            .foldLeft(Power.starting) {
                case (power, Round(blue, red, green)) =>
                    power.updateBlue(blue).updateRed(red).updateGreen(green)
            }
            .get

object Game:
    val parseGameTag: Parse[Long] =
        Parse.word("Game ")
            .followedBy(Parse.unsignedNum)
            .followedBy(Parse.word(": "))
            .withParsed {
                case ((_, num), _) => num
            }

    val parse: Parse[Game] =
        parseGameTag
            .followedBy(Parse.split(Round.parse, "; "))
            .withParsed {
                case (tag, rounds) => Game(tag, rounds)
            }
