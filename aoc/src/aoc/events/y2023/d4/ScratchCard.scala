package aoc.events.y2023.d4

import aoc.*

case class ScratchCard(index: Long, winners: List[Long], numbers: List[Long]):
    val winningNumbers: Long = numbers.count(winners.contains)

    val points: Long =
        winningNumbers match
        case 0 => 0
        case 1 => 1
        case x => scala.math.pow(2, x - 1).toLong

    def winsCards(context: List[ScratchCard]): Long =
        1 + context
            .filter: other =>
                other.index > index && other.index <= index + winningNumbers
            .map(_.winsCards(context))
            .sum

object ScratchCard:

    def debugParse[A]: Warp[A, A] =
        Warp.debug(a => s"Parsed: $a")

    val parseNumbersSeparatedBySpace: Parse[List[Long]] =
        Parse.splitRepeated(Parse.unsignedNum, ' ')

    val parse: Parse[ScratchCard] =
        Parse.word("Card")
            .ignoring(Parse.whitespace)
            .followedBy(Parse.unsignedNum)
            .withParsed(_._2)
            .ignoring(Parse.word(":"))
            .ignoring(Parse.whitespace)
            .followedBy(parseNumbersSeparatedBySpace)
            .ignoring(Parse.whitespace)
            .ignoring(Parse.word("|"))
            .ignoring(Parse.whitespace)
            .followedBy(parseNumbersSeparatedBySpace)
            .ignoring(Parse.whitespace)
            .withParsed:
                case ((index, winners), numbers) =>
                    ScratchCard(index, winners, numbers)
