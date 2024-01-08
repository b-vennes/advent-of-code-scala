package aoc.events.y2023.d7

import aoc.*

case class HandAndBid(hand: Hand, bid: Long)

object HandAndBid:
    val parse: Parse[HandAndBid] =
        Hand.parse
            .ignoring(Parse.whitespace)
            .followedBy(Parse.unsignedNum)
            .withParsed: (hand, bid) =>
                HandAndBid(hand, bid)
