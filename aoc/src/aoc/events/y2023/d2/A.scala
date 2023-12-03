package aoc.events.y2023.d2

import aoc.*
import aoc.tools.*

object A:
    val solve: Warp[Input, String] =
        Input.readLines
            .multiverseWarp(Game.parse)
            .move(games =>
                games
                    .map(_._1)
                    .filter(_.isPossible(14, 12, 13))
                    .map(_.number)
                    .sum
                    .toString
            )
