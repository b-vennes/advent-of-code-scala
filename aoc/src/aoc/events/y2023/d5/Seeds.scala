package aoc.events.y2023.d5

import aoc.*

opaque type Seeds = List[Long]

extension (seeds: Seeds)
    def toList: List[Long] = seeds

object Seeds:
    val parse: Parse[Seeds] =
        Parse.word("seeds: ")
            .followedBy(Parse.split(Parse.unsignedNum, " "))
            .withParsed {
                case (_, seeds) => seeds
            }
