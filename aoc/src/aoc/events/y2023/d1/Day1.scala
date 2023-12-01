package aoc.events.y2023.d1

import aoc.tools.*

object Day1:

    case class CalibrationValue(first: Int, second: Int):
        def number = s"${first}${second}".toInt

    object CalibrationValue:
        val parse: Warp[String, CalibrationValue] =
            Parse.repeat(Parse.digitOrLetter)
                .move: (value, _) =>
                    value.collect:
                        case i: Int => i
                .calculate:
                    case (head :: tail) =>
                        Warp.toLocation(CalibrationValue(head, tail.lastOption.getOrElse(head)))
                    case values =>
                        Warp.doomed(
                            RuntimeException(
                                "Not enough numbers to make calibration values in ${values}"
                            )
                        )
