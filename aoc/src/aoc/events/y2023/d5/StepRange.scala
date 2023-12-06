package aoc.events.y2023.d5

import aoc.tools.*

case class StepRange(step: String, range: ContRange)

object StepRange:
    def parse(step: String): Parse[StepRange] =
        Parse
            .unsignedNum
            .ignoring(Parse.word(" "))
            .followedBy(Parse.unsignedNum)
            .withParsed {
                case (start, range) => StepRange(step, ContRange(start, range))
            }
