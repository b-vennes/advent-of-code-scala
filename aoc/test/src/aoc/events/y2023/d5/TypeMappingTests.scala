package aoc.events.y2023.d5

class TypeMappingTests extends munit.FunSuite:
    test("Mapping#mapRange") {
        val input = ContRange.from(1L).to(10L)
        val mapping = TypeMap.Mapping(5L, 2L, 3L)
        val expected = Some(ContRange.from(5L).to(7L)) -> List(ContRange.from(1L).to(1L), ContRange.from(5L).to(10L))
        assertEquals(
            mapping.mapRange(input),
            expected
        )
    }

    test("mapRange") {
        val input = ContRange.from(79).to(92)
        val mapping = TypeMap(
            "seed",
            "soil",
            List(
                TypeMap.Mapping(50, 98, 2),
                TypeMap.Mapping(52, 50, 48),
            )
        )
        val expected = List(ContRange.from(81).to(94))

        assertEquals(
            mapping.mapRange(input),
            expected
        )
    }
