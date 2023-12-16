package aoc

import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

type Parse[A] = Warp[String, Parse.Success[A]]

object Parse:
    type Success[A] = (A, String)

    object Symbols:
        enum Sign:
            case Plus, Minus

    val sign: Parse[Symbols.Sign] =
        Warp.calculate: from =>
            from.toList match
            case '+' :: rem =>
                Warp.toLocation(Symbols.Sign.Plus -> rem.mkString)
            case '-' :: rem =>
                Warp.toLocation(Symbols.Sign.Minus -> rem.mkString)
            case _ => Warp.doomed(RuntimeException("Failed to parse symbol."))

    val unsignedNum: Parse[Long] =
        @tailrec
        def parseDigits(
            parsed: List[Char],
            from: List[Char]
        ): (List[Char], List[Char]) =
            from match
            case head :: tail if head.isDigit =>
                parseDigits(parsed :+ head, tail)
            case value => parsed -> value

        Warp: from =>
            val (num, rem) = parseDigits(List.empty, from.toList)
            Future.fromTry(Try(num.mkString.toLong -> rem.mkString))

    val digitOrLetter: Parse[(Int | Char)] =
        Warp.calculate: text =>
            text.toList match
            case head :: tail if head.isDigit =>
                Warp.toLocation(head.toString.toInt -> tail.mkString)
            case head :: tail if head.isLetter =>
                Warp.toLocation(head -> tail.mkString)
            case _ => Warp.doomed(
                    RuntimeException(
                        s"did not match a digit or letter at the start of ${text}"))

    val letters: Parse[String] =
        Warp.startAt[String]
            .move: text =>
                val parsed = text.takeWhile(_.isLetter)
                parsed -> text.drop(parsed.length)

    def word(text: String): Parse[String] =
        Warp.calculate: input =>
            if input.startsWith(text) then
                Warp.toLocation(text -> input.drop(text.length))
            else
                Warp.doomed(
                    RuntimeException(s"'$input' does not start with '$text'"))

    def repeat[A](parse: Parse[A]): Parse[List[A]] =
        Warp.evade(
            parse.calculate:
                case (next, remaining) if remaining.isEmpty =>
                    Warp.toLocation(List(next) -> remaining)
                case (next, remaining) =>
                    Warp.toPoint(repeat(parse).jump(remaining))
                        .move: (result, remaining) =>
                            (next :: result) -> remaining
            ,
            Warp.startAt[String].move(List.empty[A] -> _)
        )

    def words[A](values: String*): Parse[String] =
        Warp.calculate: input =>
            values
                .find(input.startsWith)
                .map(value => value -> input.drop(value.length))
                .fold(Warp.doomed(RuntimeException(
                    s"'$input' does not start with one of ${values.mkString(",")}")))(
                    Warp.toLocation)

    def split[A](parse: Parse[A], separator: String): Parse[List[A]] =
        Warp.evade(
            parse
                .calculate:
                    case (parsed, remaining)
                    if remaining.startsWith(separator) =>
                        Warp.toPoint(
                            split(parse, separator)
                                .move: (parsedValues, remaining) =>
                                    (parsed :: parsedValues) -> remaining
                                .jump(remaining.drop(separator.length))
                        )
                    case (parsed, remaining) =>
                        Warp.toLocation(List(parsed) -> remaining)
            ,
            Warp.startAt[String].move(List.empty[A] -> _)
        )

    def splitRepeated[A](
        parse: Parse[A],
        repeatedSeparator: String
    ): Parse[List[A]] =
        @tailrec
        def dropWhileStartsWithSeparator(input: String): String =
            if input.startsWith(repeatedSeparator) then
                dropWhileStartsWithSeparator(
                    input.drop(repeatedSeparator.length))
            else input

        Warp.evade(
            parse
                .calculate:
                    case (parsed, remaining)
                    if remaining.startsWith(s"$repeatedSeparator") =>
                        Warp.toPoint(
                            splitRepeated(parse, repeatedSeparator)
                                .move: (parsedValues, remaining) =>
                                    (parsed :: parsedValues) -> remaining
                                .jump(dropWhileStartsWithSeparator(remaining))
                        )
                    case (parsed, remaining) =>
                        Warp.toLocation(List(parsed) -> remaining)
            ,
            Warp.startAt[String].move(List.empty[A] -> _)
        )

    def splitRepeated[A](
        parse: Parse[A],
        repeatedSeparator: Char
    ): Parse[List[A]] =
        splitRepeated(parse, s"$repeatedSeparator")

    def fallback[A](first: Parse[A], second: Parse[A]): Parse[A] =
        Warp.startAt[String]
            .calculate: text =>
                given ExecutionContext = first.drive.toContext
                Warp.toPoint(
                    first
                        .jump(text)
                        .recoverWith:
                            case firstError =>
                                second
                                    .jump(text)
                                    .recoverWith:
                                        case secondError =>
                                            Future.failed(
                                                RuntimeException(
                                                    s"Failed to run multiple parsers: '$firstError' and '$secondError'"))
                )

    val whitespace: Parse[String] =
        Warp.startAt[String]
            .move: s =>
                val spaces = s.takeWhile(_ == ' ')
                val remaining = s.drop(spaces.length)
                spaces -> remaining

    val empty: Parse[Unit] =
        Warp.startAt[String]
            .move(() -> _)

    val newline: Parse[Unit] =
        Warp.startAt[String]
            .calculate: s =>
                if s.startsWith(System.lineSeparator()) then
                    Warp.toLocation(() -> s.drop(System.lineSeparator().length))
                else
                    Warp.doomed(RuntimeException(
                        s"Failed to find newline at the start of $s"))

extension [A](parse: Parse[A])
    def followedBy[B](parseB: Parse[B]): Parse[(A, B)] =
        parse.follow: (a, remaining) =>
            parseB
                .move: (b, remaining) =>
                    (a -> b) -> remaining
                .jump(remaining)

    def followedByWith[B](f: A => Parse[B]): Parse[(A, B)] =
        parse.follow: (a, remaining) =>
            f(a)
                .move: (b, remaining) =>
                    (a -> b) -> remaining
                .jump(remaining)

    def ignoring[B](parseB: Parse[B]): Parse[A] =
        followedBy(parseB)
            .withParsed(_._1)

    def withParsed[B](f: A => B): Parse[B] =
        parse.move: (a, remaining) =>
            f(a) -> remaining
