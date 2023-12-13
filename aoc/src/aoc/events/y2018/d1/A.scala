package aoc.events.y2018.d1

import scala.util.Try

import aoc.*

object A:
    val solve: Solution =
        Warp.startAt[Input]
            .calculate(_.readLines)
            .multiWarp(Day1.parseChange)
            .move(_.sum)
            .move(_.toString)
