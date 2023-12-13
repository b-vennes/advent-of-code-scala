package aoc.events.y2023.d12

class DamagedRecordTests extends munit.FunSuite:
    test("options") {
        val input = DamagedRecord(
            List(
                DamagedGroup(
                    Status.Known,
                    Status.Unknown,
                    Status.Known
                ),
                DamagedGroup(
                    Status.Unknown,
                    Status.Known
                )
            ),
            List(1, 2)
        )

        val expected: List[Record] =
            List(
                Record(
                    List(
                        KnownGroup.withSize(3),
                        KnownGroup.withSize(2)
                    ),
                    List(1, 2)
                ),
            )

        assertEquals(input.options(), expected)
    }
