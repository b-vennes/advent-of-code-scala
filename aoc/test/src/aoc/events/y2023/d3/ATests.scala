package aoc.events.y2023.d3

import aoc.events.y2023.d3.*

import scala.concurrent.ExecutionContext

class ATests extends munit.FunSuite {

    given ExecutionContext = ExecutionContext.global

    test("parse engine block 1") {
        val expected = EngineBlock(
            List(Tag(44, 4, 0), Tag(222, 1, 2)),
            List(Part('*', 6, 1))
        )

        val input = List(
            "....44...",
            "......*..",
            ".222.....",
            ".........",
        )

        A.parseEngineBlock.jump(input)
            .map(result => assertEquals(result, expected))
    }

    test("find sum of unused tags 1") {
        val expected = "44"

        val input = EngineBlock(
            List(Tag(44, 4, 0), Tag(222, 1, 2)),
            List(Part('*', 6, 1))
        )

        A.findSumOfUnusedTags.jump(input)
            .map(result => assertEquals(result, expected))
    }
}
