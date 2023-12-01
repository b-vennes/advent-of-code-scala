package aoc.tools

import scala.concurrent.{Future, ExecutionContext}

class WarpTests extends munit.FunSuite:
    given ExecutionContext = scala.concurrent.ExecutionContext.global

    test("Warp to location yields location"):
        val expected = "test"

        val result: Future[String] = Warp.toLocation("test").go

        result.map(result => assertEquals(result, expected))

    test("Warp to location chained with move"):
        val expected = "second location"

        val result: Future[String] = Warp
            .toLocation("first location")
            .move(_ => "second location")
            .go

        result.map(result => assertEquals(result, expected))

    test("Warp as attempt"):
        val expected = 1110

        val result = Warp.toLocation(100)
            .move(_ + 10)
            .calculate(x => Warp.toLocation(x + 1000))
            .go

        result.map(result => assertEquals(result, expected))
