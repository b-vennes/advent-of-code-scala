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
                                s"Not enough numbers to make calibration values in ${values}"
                            )
                        )

        val parseWithNumberWords: Warp[String, CalibrationValue] =
            Warp.startAt[String]
                .calculate: text =>
                    Warp.toLocation:
                      text.replaceAll("one", "1")
                        .replaceAll("two", "2")
                        .replaceAll("three", "3")
                        .replaceAll("four", "4")
                        .replaceAll("five", "5")
                        .replaceAll("six", "6")
                        .replaceAll("seven", "7")
                        .replaceAll("eight", "8")
                        .replaceAll("nine", "9")
                    .warp(parse)
                    .move: result =>
                        println(s"$text parsed to $result")
                        result
