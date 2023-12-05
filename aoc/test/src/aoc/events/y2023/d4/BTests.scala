package aoc.events.y2023.d4

import scala.concurrent.ExecutionContext

class BTests extends munit.FunSuite:

    given ExecutionContext = ExecutionContext.global

    test("calculate cards 1") {
        val input = List(
            ScratchCard(1, List(1, 2), List(1, 2)),
            ScratchCard(2, List(), List()),
            ScratchCard(3, List(), List())
        )

        val expected = 5L

        B.calculateCards.jump(input)
            .map(result => assertEquals(result, expected))
    }

    test ("calculate cards 2") {
        val input = List(
            "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53",
            "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19",
            "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1",
            "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83",
            "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36",
            "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"
        )

        val expected = 30L

        B.parseCards.warp(B.calculateCards)
            .jump(input)
            .map(result => assertEquals(result, expected))
    }

    test("calculate cards 3") {
        val input = List(
            ScratchCard(1, List(1), List(2)),
            ScratchCard(2, List(1), List(1)),
            ScratchCard(3, List(), List()),
        )

        val expected = 4L

        B.calculateCards.jump(input)
            .map(result => assertEquals(result, expected))
    }

    test("calculate cards 4") {
        val input = List(
            ScratchCard(1, List(1, 2), List(1, 2)),
            ScratchCard(2, List(1), List(1)),
            ScratchCard(3, List(), List()),
        )

        // 1 -> (2, 3), 2 -> 3
        // 1 -> 2 -> 3
        //   -> 3
        // 2 -> 3
        // 3

        val expected = 7L

        B.calculateCards.jump(input)
            .map(result => assertEquals(result, expected))
    }