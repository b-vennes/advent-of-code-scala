package aoc

import aoc.*
import aoc.tools.*

case class Problem(event: String, day: Int, section: String)

object Problem:
    val parseArgs: Warp[Seq[String], Problem] =
        Warp.calculate { args =>
            args.toList match
            case event :: day :: section :: _ =>
                Warp.toLocation(day.toInt)
                    .move(day => Problem(event, day, section))
            case _ =>
                Warp.doomed(RuntimeException("Failed to parse problem from program arguments."))
        }
