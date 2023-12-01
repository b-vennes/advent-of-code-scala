package aoc.events.y2018.d1

import scala.util.Try

import aoc.*
import aoc.tools.*

object A:
    val solve: Warp[Input, String] =
        Warp.startAt[Input]
            .calculate(_.readLines)
            .multiverseWarp(Day1.parseChange)
            .move(_.sum)
            .move(_.toString)
