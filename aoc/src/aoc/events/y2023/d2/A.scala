package aoc.events.y2023.d2

import aoc.*

object A:
    val solve: Solution =
        Input.readLines
            .multiWarp(Game.parse)
            .move(games =>
                games
                    .map(_._1)
                    .filter(_.isPossible(14, 12, 13))
                    .map(_.number)
                    .sum
                    .toString
            )
