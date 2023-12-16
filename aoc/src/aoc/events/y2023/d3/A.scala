package aoc.events.y2023.d3

import aoc.*

object A:

    val parseEngineBlock =
        Warp.startAt[List[String]].move(_.zipWithIndex)
            .multiWarp(
                Warp.startAt[(String, Int)]
                    .follow:
                        case (line, index) =>
                            EngineBlock.parseLine(index)
                                .jump(line)
            )
            .move(_.map(_._1))
            .move(
                _.foldLeft(EngineBlock(List.empty, List.empty)):
                    case (a, b) => a.combine(b)
            )

    val findSumOfUnusedTags =
        Warp.startAt[EngineBlock]
            .move(engine =>
                engine.tags.filter(tag => engine.parts.exists(tag.tags)))
            .move(_.map(_.number).sum.toString)

    val solve: Warp[Input, String] =
        Input.readLines
            .warp(parseEngineBlock)
            .warp(findSumOfUnusedTags)
