package aoc.events.y2023.d1

import aoc.{Parse, Warp}

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
                        Warp.toLocation(CalibrationValue(
                            head,
                            tail.lastOption.getOrElse(head)))
                    case values =>
                        Warp.doomed(
                            RuntimeException(
                                s"Not enough numbers to make calibration values in ${values}"
                            ))

        val parseWithNumberWords: Warp[String, CalibrationValue] =
            Warp.startAt[String]
                .move: text =>
                    text.replaceAll("one", "o1e")
                        .replaceAll("two", "t2o")
                        .replaceAll("three", "t3e")
                        .replaceAll("four", "f4r")
                        .replaceAll("five", "f5e")
                        .replaceAll("six", "s6x")
                        .replaceAll("seven", "s7n")
                        .replaceAll("eight", "e8t")
                        .replaceAll("nine", "n9e")
                .warp(parse)
