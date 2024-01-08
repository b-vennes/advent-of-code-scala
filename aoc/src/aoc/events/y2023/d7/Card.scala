package aoc.events.y2023.d7

import aoc.*

enum Card:
    case A, K, Q, J, T, Nine, Eight, Seven, Six, Five, Four, Three, Two

object Card:
    extension (card: Card)
        def strength: Int =
            card match
            case Two   => 0
            case Three => 1
            case Four  => 2
            case Five  => 3
            case Six   => 4
            case Seven => 5
            case Eight => 6
            case Nine  => 7
            case T     => 8
            case J     => 9
            case Q     => 10
            case K     => 11
            case A     => 12

    val parse: Parse[Card] =
        Parse.words(
            "A",
            "K",
            "Q",
            "J",
            "T",
            "9",
            "8",
            "7",
            "6",
            "5",
            "4",
            "3",
            "2"
        ).withParsed:
            case "A" => Card.A
            case "K" => Card.K
            case "Q" => Card.Q
            case "J" => Card.J
            case "T" => Card.T
            case "9" => Card.Nine
            case "8" => Card.Eight
            case "7" => Card.Seven
            case "6" => Card.Six
            case "5" => Card.Five
            case "4" => Card.Four
            case "3" => Card.Three
            case "2" => Card.Two
