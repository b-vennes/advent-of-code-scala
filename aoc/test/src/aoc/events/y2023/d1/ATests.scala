package aoc.events.y2023.d1

import aoc.tools.*
import Day1.CalibrationValue
import scala.concurrent.ExecutionContext

class ATests extends munit.FunSuite:

  given ExecutionContext = scala.concurrent.ExecutionContext.global

  test("parsing example 1") {
    val input = "a1bbbc1d"
    val expected = CalibrationValue(1, 1)
    val expectedRemaining = ""
    CalibrationValue.parse
        .jump(input)
        .map { result =>
          assertEquals(result, expected)
        }
  }
