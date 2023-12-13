package aoc.events.y2023.d12

enum Status:
    case Known, Unknown

opaque type DamagedGroup = List[Status]

object DamagedGroup:

    def apply(statuses: Status*): DamagedGroup =
        statuses.toList

    extension (group: DamagedGroup)
        def setKnown(index: Int): DamagedGroup =
            group
                .zipWithIndex
                .map {
                    case (_, i) if i == index => Status.Known
                    case (element, _)         => element
                }

        def delete(index: Int): List[DamagedGroup] =
            group.drop(index + 1) :: group.take(index) :: Nil

        def createGroupsFromDelete(index: Int): List[List[KnownGroup]] =
            delete(index)
                .filter(_.nonEmpty)
                .map(_.options())
                .foldLeft(List.empty[List[KnownGroup]]) {
                    case (Nil, next) => next
                    case (collected, next) =>
                        collected.flatMap(opt =>
                            next.map(n => opt ++ n)
                        )
                }

        def options(): List[List[KnownGroup]] =
            KnownGroup.fromDamaged(group)
                .map { k =>
                    List(List(k))
                }
                .getOrElse(
                    group
                        .zipWithIndex
                        .flatMap {
                            case (Status.Unknown, index) =>
                                setKnown(index).options() ++
                                    createGroupsFromDelete(index)
                            case (Status.Known, _) => List.empty
                        }
                )

opaque type KnownGroup = Int

object KnownGroup:

    def withSize(size: Int): KnownGroup = size

    def fromDamaged(group: DamagedGroup): Option[KnownGroup] =
        Option.when(group.forall(_ == Status.Known))(group.length)

    extension (group: KnownGroup)
        def size: Int = group

case class DamagedRecord(groups: List[DamagedGroup], metadata: List[Int]):
    def options(): List[Record] =
        groups
            .map(group =>
                val result = group.options()
                println(s"generated options $result from $group")
                result
            )
            .foldLeft(List.empty[Record]) {
                case (Nil, groupOptions) =>
                    groupOptions.map(ks => Record(ks, metadata))
                case (records, groupOptions) =>
                    records.flatMap(record =>
                        groupOptions.map(option =>
                            record.copy(
                                groups = record.groups ++ option
                            )
                        )
                    )
            }


case class Record(groups: List[KnownGroup], metadata: List[Int]):
    val isValid: Boolean =
        groups
            .zip(metadata)
            .forall {
                case (group, size) => group.size == size
            }
