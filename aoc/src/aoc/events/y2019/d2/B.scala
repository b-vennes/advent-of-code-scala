package aoc.events.y2019.d2

import aoc.tools.*
import aoc.*

object B:

    val findParams: Warp[(Array[Int], Int, Int, Int), (Int, Int)] =
        Warp.calculate { (program, target, noun, verb) =>
            Warp.toPoint(
                Day2
                    .runUntilHalt
                    .jump(
                        (0, program.updated(1, noun).updated(2, verb))
                    )
            )
                .move(result => result(0))
                .calculate {
                    case value if value == target => Warp.toLocation(noun -> verb)
                    case _ if verb >= noun =>
                        Warp.toPoint(findParams.jump(program, target, noun + 1, 0))
                    case _ if verb > 99 =>
                        Warp.doomed(RuntimeException("Valid verb and noun not found for target."))
                    case _ => Warp.toPoint(findParams.jump(program, target, noun, verb + 1))
                }
        }

    val solve: Warp[Input, String] =
        Day2.parseProgram
            .follow(program => findParams.jump((program, 19690720, 0, 0)))
            .move(_.toString)
