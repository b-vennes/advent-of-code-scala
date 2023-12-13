package aoc.events.y2019.d1

import aoc.*

object A:
    def fuel(mass: Long): Long =
        Math.floorDiv(mass, 3) - 2

    val solve: Solution =
        Warp.startAt[Input]
            .calculate(_.readLines)
            .multiWarp(Parse.unsignedNum)
            .move(_.map(_._1).map(fuel).sum)
            .move(_.toString)
