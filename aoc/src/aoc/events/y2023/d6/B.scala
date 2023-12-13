package aoc.events.y2023.d6

import aoc.*

object B:
    val solve: Warp[Input, String] =
        Day6Input.superRace
            .move(_.waysToBeat)
            .move(_.toString)
