package aoc

import aoc.tools.{*, given}
import scala.concurrent.*
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.*

@main
def main(args: String*): Unit =
    Problem
        .parseArgs
        .warp(Solutions.solve)
        .jump(args)
        .onComplete {
            case Success(result) =>
                println(s"result = $result")
            case Failure(e: Throwable) =>
                e.printStackTrace
        }
