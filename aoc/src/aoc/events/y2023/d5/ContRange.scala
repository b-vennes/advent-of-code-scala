package aoc.events.y2023.d5

// | 1 -> 100 | >> | 4 -> 10 | == Some(| 4 -> 10 |), [ | 1 -> 3 |, | 11 -> 100 |]

// | 1 -> 100 | >> | 51 -> 110 | == Some(| 51 -> 100|), [ | 1 -> 50 | ]

// | 50 -> 100 | >> | 1 -> 75 | ==

case class ContRange(start: Long, length: Long):
    val end: Long = start + (length - 1)

    def splice(other: ContRange): (Option[ContRange], List[ContRange]) =
        // this is left of other
        if end < other.start then None -> List(ContRange(start, length))
        // this is right of other
        else if start > other.end then None -> List(ContRange(start, length))
        // there is an overlap with this hanging off the left edge
        else if start < other.start && end < other.end then
            val left = ContRange.from(start).upToExclusive(other.start)
            val right = ContRange.from(other.start).to(end)
            Some(right) -> List(left)
        // there is an overlap with this hanging off the right edge
        else if other.start < start && other.end < end then
            val left = ContRange.from(start).to(other.end)
            val right = ContRange.fromExcluding(other.end).to(end)
            Some(left) -> List(right)
        // the other block is in the middle
        else if start < other.start && other.end < end then
            val left = ContRange.from(start).upToExclusive(other.start)
            val center = other
            val right = ContRange.fromExcluding(other.end).to(end)
            Some(center) -> List(left, right)
        // the starts are the same
        else if start == other.start && other.end < end then
            val left = ContRange.from(start).to(other.end)
            val right = ContRange.fromExcluding(other.end).to(end)
            Some(left) -> List(right)
        // the ends are the same
        else if other.end == end && start < other.start then
            val left = ContRange.from(start).upToExclusive(other.start)
            val right = ContRange.from(other.start).to(end)
            Some(right) -> List(left)
        else Some(ContRange(start, length)) -> List.empty

    def startAt(newStart: Long): ContRange =
        ContRange(newStart, length)

    override def toString: String =
        s"[$start - $end]"

object ContRange:
    class ContRangeBuilder(startingAt: Long):
        def upToExclusive(end: Long): ContRange =
            ContRange(startingAt, end - startingAt)

        def to(end: Long): ContRange =
            ContRange(startingAt, end - startingAt + 1)

    def from(value: Long): ContRangeBuilder =
        new ContRangeBuilder(value)

    def fromExcluding(value: Long): ContRangeBuilder =
        new ContRangeBuilder(value + 1)
