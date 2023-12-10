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

    def mapRange(range: ContRange): List[ContRange] =
        val (untouched, newRanges) =
            mappings.foldLeft(List(range) -> List.empty[ContRange]) {
                case ((unprocessed, processed), mapping) =>
                    unprocessed.map(mapping.mapRange)
                        .foldLeft(List.empty[ContRange] -> processed) {
                            case ((unprocessed, processed), (Some(next), moreUnprocessed)) =>
                                (unprocessed ++ moreUnprocessed) -> (next :: processed)
                            case ((unprocessed, processed), (_, more)) =>
                                (unprocessed ++ more) -> processed
                        }
            }
        untouched ++ newRanges

object TypeMap:
    def apply(in: String, out: String): TypeMap =
        new TypeMap(in, out, List.empty)

    case class Mapping(outStart: Long, inStart: Long, range: Long):
        private val diff = outStart - inStart

        def map(value: Long): Option[Long] =
            val diff = value - inStart
            if diff >= 0 && diff < range then Some(outStart + diff)
            else None

        def mapRange(contRange: ContRange): (Option[ContRange], List[ContRange]) =
            contRange.splice(ContRange(inStart, range)) match
            case Some(toMap) -> ignored =>
                Some(toMap.startAt(toMap.start + diff)) -> ignored
            case None -> ignored => None -> ignored

        override def toString: String =
            s"${ContRange(inStart, range)} >> ${ContRange(outStart, range)}"

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
