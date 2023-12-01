package aoc.events.y2018.d1

class BTests extends munit.FunSuite {
  test("+1, -1") {
    val frequencies = List(1L, -1L)
    val result = B.findRepeatedFrequency(List.empty, 0L, frequencies)
    assertEquals(result, 0L)
  }

  test("+3, +3, +4, -2, -4") {
    val input: List[Long] = List(3, 3, 4, -2, -4)
    val result = B.findRepeatedFrequency(List.empty, 0L, input)
    assertEquals(result, 10L)
  }

  test("-6, +3, +8, +5, -6") {
    val input: List[Long] = List(-6, 3, 8, 5, -6)
    val result = B.findRepeatedFrequency(List.empty, 0L, input)
    assertEquals(result, 5L)
  }
}
