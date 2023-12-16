package aoc.events.y2023.d4

import aoc.*

object B:

    val calculateCards: Warp[List[ScratchCard], Long] =
        Warp.startAt[List[ScratchCard]]
            .move: cards =>
                cards
                    .map(_.winsCards(cards))
                    .sum

    val parseCards: Warp[List[String], List[ScratchCard]] =
        Warp.startAt[List[String]]
            .multiWarp(ScratchCard.parse)
            .move(_.map(_._1))

    val solve: Warp[Input, String] =
        Input.readLines
            .warp(parseCards)
            .warp(calculateCards)
            .move(_.toString)
