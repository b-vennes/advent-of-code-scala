package aoc.events.y2023.d1

import aoc.Input
import aoc.tools.*

object A:
    val solve: Warp[Input, String] =
        Warp.startAt[Input]
            .calculate(_.readLines)
            .multiverseWarp(Day1.CalibrationValue.parse)
            .move(_.map(_.number).sum.toString)
