package aoc.events.y2023.d2

import aoc.*
import aoc.tools.*

object B:
    val solve: Warp[Input, String] =
        Input.readLines
            .multiverseWarp(Game.parse)
            .move(games => games.map(_._1))
            .multiverseWarp(Warp.startAt[Game].move(_.power))
            .move(_.sum.toString)
