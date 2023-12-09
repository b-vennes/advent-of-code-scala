package aoc.events.y2023.d6

import aoc.*
import aoc.tools.*

object A:

    val solve: Warp[Input, String] =
        Day6Input.asMultipleRaces
            .multiverseWarp(
                Warp.startAt[Race]
                    .move(_.waysToBeat)
            )
            .move(_.product)
            .move(_.toString)
