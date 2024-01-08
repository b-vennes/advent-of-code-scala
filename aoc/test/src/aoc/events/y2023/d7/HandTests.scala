package aoc.events.y2023.d7

class HandTests extends munit.FunSuite:
    test("five of a kind beats assortment"):
        val allTwos = Hand(
            Card.Two,
            Card.Two,
            Card.Two,
            Card.Two,
            Card.Two
        )

        val assortment = Hand(
            Card.Two,
            Card.T,
            Card.Eight,
            Card.Nine,
            Card.Q
        )

        assert(allTwos.beats(assortment))

    test("assortment with higher left beats other assortment"):
        val highAssortment = Hand(
            Card.A,
            Card.Two,
            Card.Three,
            Card.Four,
            Card.Five
        )

        val assortment = Hand(
            Card.Two,
            Card.T,
            Card.Eight,
            Card.Nine,
            Card.Q
        )

        assert(highAssortment.beats(assortment))

    test("2, 2, 3, 3, 6 is two pair"):
        assert(
            Hand(
                Card.Two,
                Card.Two,
                Card.Three,
                Card.Three,
                Card.Six
            ).isTwoPair
        )

    test("2, 5, 3, 4, 5 is pair"):
        assert(
            Hand(
              Card.Two,
              Card.Five,
              Card.Three,
              Card.Four,
              Card.Five
            ).isPair
        )

    test("3, 3, 2, 3, 2 is full house"):
        assert(
            Hand(
              Card.Three,
              Card.Three,
              Card.Two,
              Card.Three,
              Card.Two
            ).isFullHouse
        )
