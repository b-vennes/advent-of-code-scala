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
            val left = ContRange(start, other.start - start)
            val right = ContRange(other.start, end - other.start + 1)
            Some(right) -> List(left)
        // there is an overlap with this hanging off the right edge
        else if other.start < start && other.end < end then
            val left = ContRange(start, other.end - start + 1)
            val right = ContRange(other.end, end - other.end + 1)
            Some(left) -> List(right)
        // the other block is in the middle
        else if start < other.start && other.end < end then
            val left = ContRange(start, other.start - start)
            val center = other
            val right = ContRange(other.end, end - other.end)
            Some(center) -> List(left, right)
        else throw NotImplementedError("I haven't decided what to do here yet")
