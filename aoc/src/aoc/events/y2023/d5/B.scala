package aoc.events.y2023.d5

import aoc.*
import aoc.tools.*

object B:

    def toLocation(maps: List[TypeMap]): Warp[StepRange, List[StepRange]] =
        Warp.startAt[StepRange]
            .calculate {
                case StepRange("location", range) =>
                    Warp.toLocation(List(StepRange("location", range)))
                case StepRange(current, range) => maps.find(_.in == current)
                        .fold(throw RuntimeException(
                            s"Could not find matching map for type '$current' at value $range"
                        ))(m =>
                            Warp.toLocation(m.mapRange(range))
                                .calculate(ranges =>
                                    Warp.toLocation(ranges)
                                        .move(_.map(range => StepRange(m.out, range)))
                                        .multiverseWarp(toLocation(maps))
                                        .move(_.flatten)
                                )
                        )
            }

    val solve: Warp[Input, String] =
        Input.readAll
            .move(_.replaceAll(s"${System.lineSeparator()}${System.lineSeparator()}", "##"))
            .move(_.replaceAll(s"${System.lineSeparator()}", "&&"))
            .warp(parseInputRanges)
            .move(_._1)
            .calculate {
                case (ranges, mappings) =>
                    Warp.toLocation(ranges)
                        .multiverseWarp(toLocation(mappings))
                        .move(_.flatten)
            }
            .move(_.minBy(_.range.start).range.start)
            .move(_.toString())
