package aoc.events.y2019.d2

import aoc.tools.{*, given}
import aoc.*

object Day2:
    val parseProgram: Warp[Input, Array[Int]] =
        Warp.startAt[Input]
            .calculate(_.readAll)
            .move(_.split(",").toList)
            .multiverseWarp(Parse.unsignedNum)
            .move(_.map(_._1).map(_.toInt))
            .move(_.toArray)

    enum Op:
        case Add(xIdx: Int, yIdx: Int, into: Int)
        case Mult(xIdx: Int, yIdx: Int, into: Int)
        case Halt

    object Op:
        object Add:
            val read: Warp[(Int, Array[Int]), Op] =
                Warp.startAt[(Int, Array[Int])]
                    .move: (pos, program) =>
                        Op.Add(program(pos + 1), program(pos + 2), program(pos + 3))

        object Mult:
            val read: Warp[(Int, Array[Int]), Op] =
                Warp.startAt[(Int, Array[Int])]
                    .move: (pos, program) =>
                        Op.Mult(program(pos + 1), program(pos + 2), program(pos + 3))

        val read: Warp[(Int, Array[Int]), Op] =
            Warp.calculate[(Int, Array[Int]), Op]: (pos, array) =>
                array(pos) match
                case 1  => Warp.toLocation(pos, array).warp(Op.Add.read)
                case 2  => Warp.toLocation(pos, array).warp(Op.Mult.read)
                case 99 => Warp.toLocation(Op.Halt)

    val crank: Warp[(Int, Array[Int]), Either[Array[Int], Array[Int]]] =
        Warp.calculate: (pos, program) =>
            Warp.toPoint:
                Op
                    .read
                    .move:
                        case Op.Add(x, y, into) =>
                            Left(program.updated(into, program(x) + program(y)))
                        case Op.Mult(x, y, into) =>
                            Left(program.updated(into, program(x) * program(y)))
                        case Op.Halt => Right(program)
                    .jump((pos, program))

    val runUntilHalt: Warp[(Int, Array[Int]), Array[Int]] =
        Warp.calculate: (pos, program) =>
            Warp.toPoint:
                crank
                    .calculate:
                        case Left(program)  => Warp.toPoint(runUntilHalt.jump(pos + 4, program))
                        case Right(program) => Warp.toLocation(program)
                    .jump(pos, program)
