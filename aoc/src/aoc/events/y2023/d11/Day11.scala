package aoc.events.y2023.d11

import aoc.*

import scala.annotation.tailrec

case class Galaxy(x: Int, y: Int):
    def inRow(row: Int): Boolean =
        y == row

    def inColumn(column: Int): Boolean =
        x == column

    def distanceTo(other: Galaxy): Long =
        Math.abs(other.x.toLong - x) + Math.abs(other.y.toLong - y)

    def distancesTo(all: List[Galaxy]): Long =
        all.map(distanceTo).sum

extension (galaxies: List[Galaxy])
    def emptyRow(row: Int): Boolean =
        galaxies.forall(!_.inRow(row))

    def emptyColumn(column: Int): Boolean =
        galaxies.forall(!_.inColumn(column))

    def expandAfterColumn(column: Int, size: Int = 2): List[Galaxy] =
        galaxies.map:
            case Galaxy(x, y) if x > column =>
                Galaxy(x + size - 1, y)
            case galaxy => galaxy

    def expandAfterRow(row: Int, size: Int = 2): List[Galaxy] =
        galaxies.map:
            case Galaxy(x, y) if y > row => Galaxy(x, y + size - 1)
            case galaxy                  => galaxy

    def runExpansion(size: Int = 2): List[Galaxy] =
        val cols = galaxies.map(_.x)
        val rows = galaxies.map(_.y)
        val colRange = cols.min.to(cols.max)
        val rowRange = rows.min.to(cols.max)

        val colExpanded: List[Galaxy] = colRange.foldRight(galaxies):
            case (col, galaxies) if galaxies.emptyColumn(col) =>
                galaxies.expandAfterColumn(col, size)
            case (_, galaxies) => galaxies

        rowRange.foldRight(colExpanded):
            case (row, galaxies) if galaxies.emptyRow(row) =>
                galaxies.expandAfterRow(row, size)
            case (_, galaxies) => galaxies

    def sumPaths: Long =
        @tailrec
        def sumEachPair(current: List[Galaxy] = galaxies, sum: Long = 0): Long =
            current match
            case Nil => sum
            case first :: others =>
                sumEachPair(others, sum + first.distancesTo(others))

        sumEachPair()

def parseGalaxies(y: Int, x: Int = 0): Parse[List[Galaxy]] =
    Parse.fallback(
        Parse
            .repeat(Parse.word("."))
            .followedBy(Parse.word("#"))
            .withParsed:
                case (dots, _) => Galaxy(x + dots.length, y)
            .followedByWith(galaxy => parseGalaxies(y, galaxy.x + 1))
            .withParsed:
                case head -> tail => head :: tail
        ,
        Parse.empty.withParsed(_ => List.empty[Galaxy])
    )

val parseGalaxiesAtRow: Warp[(String, Int), List[Galaxy]] =
    Warp.startAt[(String, Int)]
        .calculate: (text, y) =>
            Warp.toLocation(text)
                .warp(parseGalaxies(y).move(_._1))

val solvePartA: Solution =
    Input
        .readLines
        .move(_.zipWithIndex)
        .multiWarp(parseGalaxiesAtRow)
        .move(_.flatten)
        .move(_.runExpansion())
        .move(_.sumPaths)
        .move(_.toString)

val solvePartB: Solution =
    Input
        .readLines
        .move(_.zipWithIndex)
        .multiWarp(parseGalaxiesAtRow)
        .move(_.flatten)
        .move(_.runExpansion(1_000_000))
        .move(_.sumPaths)
        .move(_.toString)
