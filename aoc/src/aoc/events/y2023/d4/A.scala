package aoc.events.y2023.d4

import aoc.*

object A:

    val calculatePoints: Warp[List[ScratchCard], Long] =
        Warp.startAt[List[ScratchCard]]
            .multiWarp(Warp.startAt[ScratchCard].move(_.points))
            .move(_.sum)

    val solve: Warp[Input, String] =
        Input.readLines
            .multiWarp(ScratchCard.parse)
            .move(_.map(_._1))
            .warp(calculatePoints)
            .move(_.toString)
