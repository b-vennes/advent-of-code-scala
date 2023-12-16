package aoc.events.y2023.d3

import aoc.*

case class Box(
    topLeftX: Int,
    topLeftY: Int,
    bottomRightX: Int,
    bottomRightY: Int
):
    def contains(pointX: Int, pointY: Int): Boolean =
        pointX >= topLeftX && pointX <= bottomRightX && pointY >= topLeftY && pointY <= bottomRightY

case class Tag(number: Long, x: Int, y: Int):
    val numberLength: Int = number.toString.length
    def tags(part: Part): Boolean =
        Box(x - 1, y - 1, x + numberLength, y + 1).contains(part.x, part.y)

object Tag:
    def parse(initialX: Int, y: Int): Parse[Tag] =
        Parse
            .repeat(Parse.word("."))
            .followedBy(
                Parse.unsignedNum
            )
            .withParsed {
                case (dots, number) => Tag(number, initialX + dots.length, y)
            }

case class Part(symbol: Char, x: Int, y: Int):
    def toGear: Option[Part] =
        if symbol == '*'
        then Some(Part(symbol, x, y))
        else None

object Part:
    def parse(initialX: Int, y: Int): Parse[Part] =
        Parse
            .repeat(Parse.word("."))
            .followedBy(
                Warp.startAt[String]
                    .calculate(text =>
                        text.toList match
                        case h :: tail if !h.isDigit && !(h == '.') =>
                            Warp.toLocation(h -> tail.mkString)
                        case _ =>
                            Warp.doomed(
                                RuntimeException(
                                    s"Expected part to not be a number or a '.'")
                            )
                    )
            )
            .withParsed {
                case (dots, symbol) => Part(symbol, initialX + dots.length, y)
            }

case class EngineBlock(tags: List[Tag], parts: List[Part]):
    def prependTag(tag: Tag): EngineBlock = copy(tags = tag :: tags)

    def prependPart(part: Part): EngineBlock = copy(parts = part :: parts)

    def combine(engineBlock: EngineBlock): EngineBlock =
        EngineBlock(tags ++ engineBlock.tags, parts ++ engineBlock.parts)

object EngineBlock:
    def parseTagOrPart(initialX: Int, y: Int): Parse[Tag | Part | Unit] =
        Parse.fallback(
            Parse.fallback[Tag | Part](
                Tag.parse(initialX, y),
                Part.parse(initialX, y)
            ),
            Parse.empty
        )

    def parseLine(y: Int, initialX: Int = 0): Parse[EngineBlock] =
        parseTagOrPart(initialX, y)
            .calculate {
                case (Tag(num, x, y), remaining) =>
                    if remaining.nonEmpty
                    then
                        Warp.toPoint(parseLine(y, x + num.toString.length)
                            .withParsed(_.prependTag(Tag(num, x, y)))
                            .jump(remaining))
                    else
                        Warp.toLocation(EngineBlock(
                            List(Tag(num, x, y)),
                            List.empty) -> "")
                case (Part(symbol, x, y), remaining) =>
                    if remaining.nonEmpty
                    then
                        Warp.toPoint(parseLine(y, x + 1)
                            .withParsed(_.prependPart(Part(symbol, x, y)))
                            .jump(remaining))
                    else
                        Warp.toLocation(
                            EngineBlock(
                                List.empty,
                                List(Part(symbol, x, y))) -> ""
                        )
                case (_, remaining) =>
                    Warp.toLocation(EngineBlock(
                        List.empty,
                        List.empty) -> remaining)
            }
