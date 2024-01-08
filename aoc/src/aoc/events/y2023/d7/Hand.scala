package aoc.events.y2023.d7

import aoc.*

case class Hand(
    first: Card,
    second: Card,
    third: Card,
    fourth: Card,
    fifth: Card
):

    val toList: List[Card] = List(first, second, third, fourth, fifth)

    val sortedCardCounts: List[Int] =
        toList
            .groupMapReduce(identity)(_ => 1)(_ + _)
            .values
            .toList
            .sorted

    val maxRepeated: Int =
        toList
            .groupMapReduce(identity)(_ => 1)(_ + _)
            .values
            .max

    def isFiveOfAKind: Boolean =
        maxRepeated == 5

    def isFourOfAKind: Boolean =
        maxRepeated == 4

    def isFullHouse: Boolean =
        sortedCardCounts == List(2, 3)

    def isThreeOfAKind: Boolean =
        maxRepeated == 3

    def isTwoPair: Boolean =
        sortedCardCounts == List(1, 2, 2)

    def isPair: Boolean =
        sortedCardCounts == List(1, 1, 1, 2)

    val setStrength: Int =
        if isFiveOfAKind then 6
        else if isFourOfAKind then 5
        else if isFullHouse then 4
        else if isThreeOfAKind then 3
        else if isTwoPair then 2
        else if isPair then 1
        else 0

    def beats(other: Hand): Boolean =
        if setStrength > other.setStrength then true
        else if setStrength == other.setStrength then
            toList
                .zip(other.toList)
                .foldLeft[Option[Boolean]](None):
                    case None -> (ours -> theirs)
                    if ours.strength > theirs.strength =>
                        Some(true)
                    case None -> (ours -> theirs)
                    if ours.strength < theirs.strength =>
                        Some(false)
                    case None -> _   => None
                    case result -> _ => result
                .getOrElse(true) // fallback to true if full tie
        else false

object Hand:
    val parse: Parse[Hand] =
        Card.parse
            .followedBy(Card.parse)
            .followedBy(Card.parse)
            .followedBy(Card.parse)
            .followedBy(Card.parse)
            .withParsed:
                case ((((first, second), third), fourth), fifth) =>
                    Hand(
                        first,
                        second,
                        third,
                        fourth,
                        fifth
                    )
