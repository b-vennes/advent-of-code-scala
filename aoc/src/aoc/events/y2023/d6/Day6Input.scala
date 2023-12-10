package aoc.events.y2023.d6

import aoc.*
import aoc.tools.*

object Day6Input:

    val parseTimesAndDistances: Parse[(List[Long], List[Long])] =
        Parse.word("Time:")
            .ignoring(Parse.repeat(Parse.word(" ")))
            .followedBy(Parse.splitRepeated(Parse.unsignedNum, ' '))
            .ignoring(Parse.word("#Distance:"))
            .ignoring(Parse.repeat(Parse.word(" ")))
            .followedBy(Parse.splitRepeated(Parse.unsignedNum, ' '))
            .withParsed {
                case ((_, times), distances) => times -> distances
            }

    val parseManyRaces: Parse[List[Race]] =
        parseTimesAndDistances
            .withParsed(parsed => parsed._1.zip(parsed._2).map(Race.apply))

    val parseBigRace: Parse[Race] =
        parseTimesAndDistances
            .withParsed {
                case (time, distance) => Race(time.mkString.toLong, distance.mkString.toLong)
            }

    val asMultipleRaces: Warp[Input, List[Race]] =
        Input.readAll
            .move(_.replaceAll(System.lineSeparator(), "#"))
            .warp(parseManyRaces)
            .move(_._1)

    val superRace: Warp[Input, Race] =
        Input.readAll
            .move(_.replaceAll(System.lineSeparator(), "#"))
            .warp(parseBigRace)
            .move(_._1)
