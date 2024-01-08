package aoc.events.y2023.d2

import aoc.*

object B:
    val solve: Warp[Input, String] =
        Input.readLines
            .multiWarp(Game.parse)
            .move(games => games.map(_._1))
            .multiWarp(Warp.startAt[Game].move(_.power))
            .move(_.sum.toString)
