package aoc

import events.*

type Solution = Warp[Input, String]

object Solution:
    val get: Map[Problem, Solution] = Map(
        Problem("2018", 1, "a") -> y2018.d1.A.solve,
        Problem("2018", 1, "b") -> y2018.d1.B.solve,
        Problem("2019", 1, "a") -> y2019.d1.A.solve,
        Problem("2019", 1, "b") -> y2019.d1.B.solve,
        Problem("2019", 2, "a") -> y2019.d2.A.solve,
        Problem("2019", 2, "b") -> y2019.d2.B.solve,
        Problem("2023", 1, "a") -> y2023.d1.A.solve,
        Problem("2023", 1, "b") -> y2023.d1.B.solve,
        Problem("2023", 2, "a") -> y2023.d2.A.solve,
        Problem("2023", 2, "b") -> y2023.d2.B.solve,
        Problem("2023", 3, "a") -> y2023.d3.A.solve,
        Problem("2023", 3, "b") -> y2023.d3.B.solve,
        Problem("2023", 4, "a") -> y2023.d4.A.solve,
        Problem("2023", 4, "b") -> y2023.d4.B.solve,
        Problem("2023", 5, "a") -> y2023.d5.A.solve,
        Problem("2023", 5, "b") -> y2023.d5.B.solve,
        Problem("2023", 6, "a") -> y2023.d6.A.solve,
        Problem("2023", 6, "b") -> y2023.d6.B.solve,
        Problem("2023", 11, "a") -> y2023.d11.solvePartA,
        Problem("2023", 11, "b") -> y2023.d11.solvePartB
    )

    val solve: Warp[Problem, String] =
        Warp.calculate { problem =>
            Warp.toLocation(Input.forProblem(problem))
                .warp(get(problem))
        }
