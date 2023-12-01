package aoc.events.y2018.d1

import scala.annotation.tailrec
import scala.util.Try

import aoc.*
import aoc.tools.*

object B:
    @tailrec
    def findRepeatedFrequency(
        seen: List[Long],
        current: Long,
        changes: List[Long]
    ): Long =
        if seen.contains(current) then current
        else
            changes match
            case head :: tail =>
                findRepeatedFrequency(current :: seen, head + current, tail :+ head)
            case _ => throw RuntimeException("The list of changes can't be empty")

    /** This takes about 5 minutes of running to solve.
      */
    val solve: Warp[Input, String] =
        Warp.startAt[Input]
            .calculate(_.readLines)
            .multiverseWarp(Day1.parseChange)
            .move(parsedChanges => findRepeatedFrequency(List.empty, 0L, parsedChanges))
            .move(_.toString)
