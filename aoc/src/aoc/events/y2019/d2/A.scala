package aoc.events.y2019.d2

import aoc.tools.*
import aoc.*

object A:
    val solve: Warp[Input, String] =
        Day2.parseProgram
            .move(program =>
                0 -> program
                    .updated(1, 12)
                    .updated(2, 2)
            )
            .warp(Day2.runUntilHalt)
            .move(result => result(0))
            .move(_.toString)
