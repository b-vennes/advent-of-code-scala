package aoc.events.y2023.d7

import aoc.*

val solvePartA: Solution =
    Input.readLines
        .multiWarp(
            HandAndBid.parse
        )
        .multiWarp(Warp.startAt[Parse.Success[HandAndBid]].move(_._1))
        .move(_.sortWith((first, second) => first.hand.beats(second.hand)))
        .move(_.reverse.zipWithIndex.map((h, i) => h -> (i + 1)))
        .move(_.map((h, i) => h.bid * i).sum)
        .move(_.toString())
