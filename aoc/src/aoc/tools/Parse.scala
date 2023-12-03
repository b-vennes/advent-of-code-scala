package aoc.tools

import scala.concurrent.Future
import scala.util.Try
import scala.annotation.tailrec

type Parse[A] = Warp[String, Parse.Success[A]]

object Parse:
    type Success[A] = (A, String)

    object Symbols:
        enum Sign:
            case Plus, Minus

    val sign: Parse[Symbols.Sign] =
        Warp.calculate { from =>
            from.toList match
            case '+' :: rem => Warp.toLocation(Symbols.Sign.Plus -> rem.mkString)
            case '-' :: rem =>
                Warp.toLocation(Symbols.Sign.Minus -> rem.mkString)
            case _ => Warp.doomed(RuntimeException("Failed to parse symbol."))
        }

    val unsignedNum: Parse[Long] =
        @tailrec
        def parseDigits(
            parsed: List[Char],
            from: List[Char]
        ): (List[Char], List[Char]) =
            from match
            case head :: tail if head.isDigit => parseDigits(parsed :+ head, tail)
            case value                        => parsed -> value

        Warp { from =>
            val (num, rem) = parseDigits(List.empty, from.toList)
            Future.fromTry(Try(num.mkString.toLong -> rem.mkString))
        }

    val digitOrLetter: Parse[(Int | Char)] =
        Warp.calculate { text =>
            text.toList match
            case head :: tail if head.isDigit =>
                Warp.toLocation(head.toString.toInt -> tail.mkString)
            case head :: tail if head.isLetter =>
                Warp.toLocation(head -> tail.mkString)
            case _ => Warp.doomed(
                    RuntimeException(s"did not match a digit or letter at the start of ${text}")
                )
        }

    def word(text: String): Parse[String] =
        Warp.calculate { input =>
            if input.startsWith(text) then Warp.toLocation(text -> input.drop(text.length))
            else Warp.doomed(RuntimeException(s"'$input' does not start with '$text'"))
        }

    def repeat[A](parse: Parse[A]): Parse[List[A]] =
        parse.calculate {
            case (next, remaining) if remaining.isEmpty => Warp.toLocation(List(next) -> remaining)
            case (next, remaining) =>
                Warp.toPoint(repeat(parse).jump(remaining))
                    .move { (result, remaining) =>
                        (next :: result) -> remaining
                    }
        }

    def words[A](values: String*): Parse[String] =
        Warp.calculate { input =>
            values
                .find(input.startsWith)
                .map(value => value -> input.drop(value.length))
                .fold(Warp.doomed(
                    RuntimeException(s"'$input' does not start with one of ${values.mkString(",")}")
                ))(
                    Warp.toLocation
                )
        }

    def split[A](parse: Parse[A], separator: String): Parse[List[A]] =
        parse
            .calculate {
                case (parsed, remaining) if remaining.startsWith(separator) =>
                    Warp.toPoint(
                        split(parse, separator)
                            .move {
                                case (parsedValues, remaining) =>
                                    (parsed :: parsedValues) -> remaining
                            }
                            .jump(remaining.drop(separator.length))
                    )
                case (parsed, remaining) => Warp.toLocation(List(parsed) -> remaining)
            }

extension [A](parse: Parse[A])
    def followedBy[B](parseB: Parse[B]): Parse[(A, B)] =
        parse.follow { case (a, remaining) =>
            parseB
                .move { case (b, remaining) =>
                    (a -> b) -> remaining
                }
                .jump(remaining)
        }

    def withParsed[B](f: A => B): Parse[B] =
        parse.move {
            case (a, remaining) => f(a) -> remaining
        }
