package aoc.events.y2019.d1

import munit.FunSuite

class BTests extends FunSuite {
  test("14") {
    val input = 14
    val expected = 2L
    val result = B.fuel(input)
    assertEquals(result, expected)
  }
}
