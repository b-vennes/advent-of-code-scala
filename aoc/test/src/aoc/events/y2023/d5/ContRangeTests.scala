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
        val range1 = ContRange(50, 110)
        val range2 = ContRange(1, 100)

        val expected = Some(ContRange(50, 51)) -> List(ContRange(100, 10))

        assertEquals(
            range1.splice(range2),
            expected
        )
    }
