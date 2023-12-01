package aoc.events.y2019.d1

import scala.annotation.tailrec
import aoc.tools.*
import aoc.*

object B:
    def fuel(module: Long): Long =
        val x = Math.floorDiv(module, 3L) - 2L
        if x <= 0 then 0
        else x + fuel(x)

    val solve: Warp[Input, String] =
        Warp.startAt[Input]
            .calculate(_.readLines)
            .multiverseWarp(Parse.unsignedNum)
            .move(_.map(_._1).map(fuel).sum)
            .move(_.toString)
