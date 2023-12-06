package aoc.events.y2023.d5

import aoc.*
import aoc.tools.*

case class TypeMap(in: String, out: String, mappings: List[TypeMap.Mapping]):
    def add(mapping: TypeMap.Mapping): TypeMap =
        copy(mappings = mapping :: mappings)

    def map(value: Long): Long =
        mappings
            .map(_.map(value))
            .collectFirst {
                case Some(out) => out
            }
            .getOrElse(value)

object TypeMap:
    def apply(in: String, out: String): TypeMap =
        new TypeMap(in, out, List.empty)

    case class Mapping(outStart: Long, inStart: Long, range: Long):
        def map(value: Long): Option[Long] =
            val diff = value - inStart
            if diff >= 0 && diff < range then Some(outStart + diff)
            else None

    val parse: Parse[TypeMap] =
        Parse.letters
            .ignoring(Parse.word("-to-"))
            .followedBy(Parse.letters)
            .withParsed {
                case (inName, outName) => TypeMap(inName, outName)
            }
            .ignoring(Parse.word(s" map:&&"))
            .followedBy(
                Parse.splitRepeated(
                    Parse
                        .unsignedNum
                        .ignoring(Parse.word(" "))
                        .followedBy(Parse.unsignedNum)
                        .ignoring(Parse.word(" "))
                        .followedBy(Parse.unsignedNum),
                    "&&"
                )
            )
            .withParsed {
                case (starting, parsedItems) =>
                    parsedItems.foldLeft(starting) {
                        case (combined, ((inStart, outStart), range)) =>
                            combined.add(Mapping(inStart, outStart, range))
                    }
            }
