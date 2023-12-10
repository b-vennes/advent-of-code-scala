package aoc.events.y2023.d5

import aoc.*
import aoc.tools.*

object A:

    def toLocation(maps: List[TypeMap]): Warp[(String, Long), (String, Long)] =
        Warp.startAt[(String, Long)]
            .calculate {
                case ("location", value) => Warp.toLocation("location" -> value)
                case (current, value) => maps.find(_.in == current)
                        .fold(throw RuntimeException(
                            s"Could not find matching map for type '$current' at value $value"
                        ))(m =>
                            Warp.toLocation(m.out -> m.map(value))
                                .warp(toLocation(maps))
                        )
            }

    def seedLocation(maps: List[TypeMap]): Warp[Long, Long] =
        Warp.startAt[Long]
            .move("seed" -> _)
            .warp(toLocation(maps))
            .move(_._2)

    val solve: Warp[Input, String] =
        Input.readAll
            .move(_.replaceAll(s"${System.lineSeparator()}${System.lineSeparator()}", "##"))
            .move(_.replaceAll(s"${System.lineSeparator()}", "&&"))
            .warp(parseInputSeeds)
            .move(_._1)
            .calculate {
                case (seeds, maps) =>
                    Warp.toLocation(seeds.toList)
                        .multiverseWarp(seedLocation(maps))
            }
            .move(_.min)
            .move(_.toString)
