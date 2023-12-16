package aoc.events.y2018.d1

import aoc.*

object Day1:
    def change(sign: Parse.Symbols.Sign, num: Long): Long =
        sign match
        case Parse.Symbols.Sign.Plus  => num
        case Parse.Symbols.Sign.Minus => -1 * num

    def parseNumberWithSign(sign: Parse.Symbols.Sign): Warp[String, Long] =
        Parse.unsignedNum
            .move((num, _) => change(sign, num))

    val parseChange: Warp[String, Long] =
        Parse
            .sign
            .calculate: (sign, rem) =>
                Warp.toLocation(rem)
                    .warp(parseNumberWithSign(sign))
