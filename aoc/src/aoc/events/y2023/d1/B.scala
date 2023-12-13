package aoc.events.y2023.d1

import aoc.*

object B:
    val solve: Solution =
        Warp.startAt[Input]
            .calculate(_.readLines)
            .multiWarp(Day1.CalibrationValue.parseWithNumberWords)
            .move(_.map(_.number).sum.toString)
