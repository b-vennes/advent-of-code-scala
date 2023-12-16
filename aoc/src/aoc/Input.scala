package aoc

import scala.util.Try

case class Input(event: String, day: Int):
    val inputPath: os.Path = os.pwd / "input" / event / s"day$day"

    def readAll: Warp.FromAnywhere[String] =
        Warp.toLocation(os.read(inputPath))

    def readLines: Warp.FromAnywhere[List[String]] =
        readAll
            .move(_.split(System.lineSeparator()))
            .move(_.toList)

object Input:
    def forProblem(problem: Problem): Input =
        Input(problem.event, problem.day)

    def readAll: Warp[Input, String] = Warp.startAt[Input].calculate(_.readAll)

    def readLines: Warp[Input, List[String]] =
        Warp.startAt[Input].calculate(_.readLines)
