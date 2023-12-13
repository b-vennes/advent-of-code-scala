package aoc.events.y2023.d5

import aoc.*

val parseInputSeeds: Parse[(Seeds, List[TypeMap])] =
    Seeds.parse
        .ignoring(Parse.word("##"))
        .followedBy(Parse.splitRepeated(
            TypeMap.parse,
            s"##"
        ))

val parseInputRanges: Parse[(List[StepRange], List[TypeMap])] =
    Parse.word("seeds: ")
        .followedBy(Parse.splitRepeated(StepRange.parse("seed"), " "))
        .ignoring(Parse.word("##"))
        .followedBy(Parse.splitRepeated(
            TypeMap.parse,
            s"##"
        ))
        .withParsed {
            case ((_, seeds), maps) => seeds -> maps
        }
