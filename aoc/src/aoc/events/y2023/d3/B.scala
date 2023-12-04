package aoc.events.y2023.d3

import aoc.*
import aoc.tools.*

object B:

    val parseEngineBlock =
        Warp.startAt[List[String]].move(_.zipWithIndex)
            .multiverseWarp(
                Warp.startAt[(String, Int)]
                    .follow {
                        case (line, index) =>
                            EngineBlock.parseLine(index)
                                .jump(line)
                    }
            )
            .move(_.map(_._1))
            .move(
                _.foldLeft(EngineBlock(List.empty, List.empty)) {
                    case (a, b) => a.combine(b)
                }
            )

    val findGearRatioSum =
        Warp.startAt[EngineBlock]
            .move(engine =>
                engine.parts
                    .map(_.toGear)
                    .collect {
                        case Some(gear) => gear
                    }
                    .map(gear => engine.tags.filter(_.tags(gear)))
                    .collect {
                        case first :: second :: Nil =>
                            first.number * second.number
                    }
                    .sum
                    .toString
            )

    val solve: Warp[Input, String] =
        Input.readLines
            .warp(parseEngineBlock)
            .warp(findGearRatioSum)
