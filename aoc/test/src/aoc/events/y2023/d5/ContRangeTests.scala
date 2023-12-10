package aoc.events.y2023.d5

class ContRangeTests extends munit.FunSuite:
    test("left dangling overlap") {
        val range1 = ContRange(1, 100)
        val range2 = ContRange(50, 110)

        val expected = Some(ContRange(50, 51)) -> List(ContRange(1, 49))

        assertEquals(
            range1.splice(range2),
            expected
        )
    }

    test("right dangling overlap") {
        val range1 = ContRange.from(50).to(110)
        val range2 = ContRange.from(1).to(100)

        val expected = Some(ContRange.from(50).to(100)) -> List(ContRange.from(101).to(110))

        assertEquals(
            range1.splice(range2),
            expected
        )
    }

    test("upTo") {
        val starting = 1L
        val upTo = 5L
        val expected = ContRange(1L, 4L)
        assertEquals(ContRange.from(starting).upToExclusive(upTo), expected)
    }

    test("to same place") {
        assertEquals(
            ContRange.from(1L).to(1L),
            ContRange(1, 1)
        )
    }
