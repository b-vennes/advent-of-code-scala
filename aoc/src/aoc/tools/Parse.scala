package aoc.tools

import scala.concurrent.Future
import scala.util.Try
import scala.annotation.tailrec

object Parse:
    type Success[A] = (A, String)

    object Symbols:
        enum Sign:
            case Plus, Minus

    val sign: Warp[String, (Symbols.Sign, String)] =
        Warp.calculate: from =>
            from.toList match
            case '+' :: rem => Warp.toLocation(Parse.Symbols.Sign.Plus -> rem.mkString)
            case '-' :: rem =>
                Warp.toLocation(Parse.Symbols.Sign.Minus -> rem.mkString)
            case _ => Warp.doomed(RuntimeException("Failed to parse symbol."))

    val unsignedNum: Warp[String, (Long, String)] =
        def parseDigits(
            parsed: List[Char],
            from: List[Char]
        ): (List[Char], List[Char]) =
            from match
            case head :: tail if head.isDigit => parseDigits(parsed :+ head, tail)
            case value                        => parsed -> value

        Warp: from =>
            val (num, rem) = parseDigits(List.empty, from.toList)
            Future.fromTry(Try(num.mkString.toLong -> rem.mkString))

    val digitOrLetter: Warp[String, Success[(Int | Char)]] =
        Warp.calculate: text =>
            text.toList match
            case head :: tail if head.isDigit =>
                Warp.toLocation(head.toString.toInt -> tail.mkString)
            case head :: tail if head.isLetter =>
                Warp.toLocation(head -> tail.mkString)
            case _ => Warp.doomed(
                    RuntimeException(s"did not match a digit or letter at the start of ${text}")
                )

    def repeat[A](parse: Warp[String, Success[A]]): Warp[String, Success[List[A]]] =
        parse.calculate:
            case (next, remaining) if remaining.isEmpty => Warp.toLocation(List(next) -> remaining)
            case (next, remaining) =>
                Warp.toPoint(repeat(parse).jump(remaining))
                    .move: (result, remaining) =>
                        (next :: result) -> remaining
