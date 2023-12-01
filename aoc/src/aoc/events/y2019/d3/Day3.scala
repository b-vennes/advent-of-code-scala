package aoc.events.y2019.d3

import aoc.tools.*

object Day3:

    enum Direction:
        case Up, Down, Left, Right

    case class PathSection(direction: Direction, length: Int)

    val parsePathSection: Warp[String, PathSection] = Warp.doomed(NotImplementedError())
