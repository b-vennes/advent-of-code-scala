package aoc.events.y2023.d1

import aoc.*
import aoc.tools.*

object B:
    val solve: Warp[Input, String] =
        Warp.startAt[Input]
            .calculate(_.readLines)
            .multiverseWarp(Day1.CalibrationValue.parseWithNumberWords)
            .move(_.map(_.number).sum.toString)
