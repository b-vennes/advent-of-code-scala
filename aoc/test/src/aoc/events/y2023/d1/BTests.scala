package aoc.events.y2023.d1

import aoc.tools.*
import Day1.CalibrationValue
import scala.concurrent.ExecutionContext

class BTests extends munit.FunSuite:

  given ExecutionContext = scala.concurrent.ExecutionContext.global

  test("parsing example 1") {
    val input = "helloonetwofour"
    val expected = CalibrationValue(1, 4)
    val expectedRemaining = ""
    CalibrationValue.parseWithNumberWords
        .jump(input)
        .map { result =>
          assertEquals(result, expected)
        }
  }
