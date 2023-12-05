package aoc.events.y2023.d4

import aoc.*
import aoc.tools.*

object A:

    val calculatePoints: Warp[List[ScratchCard], Long] =
        Warp.startAt[List[ScratchCard]]
            .multiverseWarp(Warp.startAt[ScratchCard].move(_.points))
            .move(_.sum)

    val solve: Warp[Input, String] =
        Input.readLines
            .multiverseWarp(ScratchCard.parse)
            .move(_.map(_._1))
            .warp(calculatePoints)
            .move(_.toString)
