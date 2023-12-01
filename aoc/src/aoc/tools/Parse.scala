package aoc.tools

import scala.concurrent.Future
import scala.util.Try

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
