package aoc.events.y2023.d6

import aoc.*

import scala.annotation.tailrec

case class Race(time: Long, distance: Long):
    import Race.*

    def waysToBeat: Int =
        @tailrec
        def calcWaysToBeat(next: Long, aggregated: Int = 0): Int =
            if next == time then aggregated
            else
                val nextDistance = getDistance(next, time)
                if nextDistance > distance then calcWaysToBeat(next + 1, aggregated + 1)
                else calcWaysToBeat(next + 1, aggregated)

        calcWaysToBeat(1L)

object Race:
    def getDistance(holding: Long, totalTime: Long): Long =
        (totalTime - holding) * holding
