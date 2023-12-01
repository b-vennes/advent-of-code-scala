package aoc.events.y2019.d2

import aoc.tools.{*, given}
import munit.FunSuite
import scala.concurrent.ExecutionContext

class ATests extends FunSuite {
  given ExecutionContext = scala.concurrent.ExecutionContext.global

  test("1,0,0,0,99") {
    val program = Array(1, 0, 0, 0, 99)
    val expected = Array(2, 0, 0, 0, 99)
    
    Day2.runUntilHalt.jump((0, program))
      .map(result => assertEquals(result.toSeq, expected.toSeq))
  }

  test("2,3,0,3,99") {
    val program = Array(2, 3, 0, 3, 99)
    val expected = Array(2, 3, 0, 6, 99)
    Day2.runUntilHalt.jump((0, program))
      .map(result => assertEquals(result.toSeq, expected.toSeq))
  }
}
