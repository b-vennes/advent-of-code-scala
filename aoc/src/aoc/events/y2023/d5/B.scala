package aoc.events.y2023.d5

import aoc.*
import aoc.tools.*

object B:
    val solve: Warp[Input, String] =
        Input.readAll
            .move(_.replaceAll(s"${System.lineSeparator()}${System.lineSeparator()}", "##"))
            .move(_.replaceAll(s"${System.lineSeparator()}", "&&"))
            .warp(parseInputRanges)
            .move(_._1)
            .move(_.toString())
