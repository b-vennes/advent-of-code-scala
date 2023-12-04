package aoc.events.y2023.d3

import aoc.events.y2023.d3.*

import scala.concurrent.ExecutionContext

class BTests extends munit.FunSuite {

    given ExecutionContext = ExecutionContext.global

    test("gear ratio 1") {
        val expected = "1012"

        val input = List(
            "....44...",
            "......*..",
            "..5.23...",
            "...*7....",
        )

        B.parseEngineBlock.warp(B.findGearRatioSum).jump(input)
            .map(result => assertEquals(result, expected))
    }
}
