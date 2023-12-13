package aoc

import scala.annotation.targetName
import scala.concurrent.*
import scala.util.*

/** A warp drive provides the power to make a warp jump.
  *
  * It's just an execution context, but gets passed around in the explicit context instead of the
  * implicit one.
  */
opaque type Drive = ExecutionContext

object Drive:

    /** The basic global execution context. Works great for most warps.
      */
    val standard: Drive = scala.concurrent.ExecutionContext.global

    /** Summons a warp drive from an implicit [[ExecutionContext]].
      */
    def summon(using context: ExecutionContext): Drive = context

    extension (drive: Drive)
        /** Converts a warp drive back into an [[ExecutionContext]]. This is often helpful when
          * dealing with the underlying [[Future]] values in a [[Warp]].
          */
        def toContext: ExecutionContext = drive

/** A warp provides a mechanism to perform a jump from the input type [[A]] to the output type
  * [[B]].
  *
  * The jump will take an indefinite amount of time, therefore the warp path concludes at a
  * [[Future[B]]] instead of a standard [[B]] value.
  *
  * @param path
  *   the path the warp will follow when a jump occurs
  * @param drive
  *   the warp drive used to power the warp jump
  */
case class Warp[-A, +B](path: A => Future[B], drive: Drive):
    private given ExecutionContext = drive.toContext

    /** Appends a move step to the end of the warp path. A [[move]] is the most basic way to
      * transform a warp path.
      *
      * @param move
      *   a function which travels from a value [[B]] to a value [[C]]
      * @return
      *   a new warp with an updated path
      */
    def move[C](move: B => C): Warp[A, C] =
        Warp { (a: A) =>
            val futureB = path(a)
            val futureC = futureB.map(move)
            futureC
        }
            .swapDrive(drive)

    /** Appends a follow step to the end of the warp path. A [[follow]] step is more complicated
      * than a [[move]], in that it can take an indefinite amount of time.
      *
      * @param follow
      *   a function which travels from a value [[B]] to some eventual value [[C]].
      * @return
      *   a new warp with an updated path
      */
    def follow[C](follow: B => Future[C]): Warp[A, C] =
        Warp { (a: A) =>
            val futureB = path(a)
            val futureC = futureB.flatMap(follow)
            futureC
        }
            .swapDrive(drive)

    /** Performs an additional warp after the current warp is completed. Aka a "double warp".
      *
      * @param warp
      *   the next warp to perform after the first warp is completed
      * @return
      *   a new warp with an updated path
      */
    def warp[C](warp: Warp[B, C]): Warp[A, C] =
        Warp { (a: A) =>
            val futureB = path(a)
            val futureC = futureB
                .flatMap(b =>
                    warp.jump(b)
                )
            futureC
        }
            .swapDrive(drive)

    /** Calculates the next warp to perform once the current warp is completed.
      *
      * Similar to warp, but provides the final location of the initial warp. Also, requires a warp
      * that can occur from anywhere ([[Warp.FromAnywhere]]).
      *
      * @param calculate
      *   a function used to calculate what the next warp jump should be.
      * @return
      *   a new warp with a path that will be updated once the warp is completed
      */
    def calculate[C](calculate: B => Warp.FromAnywhere[C]): Warp[A, C] =
        Warp { (a: A) =>
            path(a).map(calculate).flatMap(_.go)
        }
            .swapDrive(drive)

    /** The starting location is all set and we are ready to jump.
      *
      * @param start
      *   the starting location to perform the jump from
      * @return
      *   a Scala [[Future]] which yields the final location of the warp once the jump is completed
      */
    def jump(start: A): Future[B] =
        path(start)

    /** Swaps the warp drive with the provided one. The warp will use the provided drive when making
      * a jump.
      *
      * @param drive
      *   the new drive to use to power the warp
      * @return
      *   a new warp using the provided warp drive
      */
    def swapDrive(drive: Drive): Warp[A, B] = copy(drive = drive)

object Warp:

    type FromAnywhere[A] = Warp[Any, A]

    /** Creates a basic warp which uses the given path of travel.
      */
    def apply[A, B](path: A => Future[B]): Warp[A, B] =
        new Warp(path, Drive.standard)

    /** Defines a starting location for the warp jump. The ending location of the jump will be the
      * same as the starting location.
      *
      * Often used to create a foundational warp to start building off of.
      */
    def startAt[A]: Warp[A, A] = new Warp(a => Future.fromTry(Try(a)), Drive.standard)

    /** Defines the warp at jump-time.
      */
    def calculate[A, B](calculate: A => Warp.FromAnywhere[B]): Warp[A, B] =
        def path(a: A): Future[B] =
            calculate(a).go
        new Warp(path, Drive.standard)

    def toPoint[A](to: => Future[A]): Warp.FromAnywhere[A] =
        new Warp((_: Any) => to, Drive.standard)

    def toLocation[A](location: => A): Warp.FromAnywhere[A] =
        new Warp((_: Any) => Future.fromTry(Try(location)), Drive.standard)

    /** It's a warp that's doomed to fail.
      */
    def doomed(failure: => Throwable, drive: Drive = Drive.standard): Warp[Any, Nothing] =
        new Warp(any => Future.failed(failure), drive)

    def debug[A](message: A => String): Warp[A, A] =
        Warp.startAt[A]
            .move { a =>
                println(message(a))
                a
            }

    def evade[A, B](warp: Warp[A, B], evade: Warp[A, B]): Warp[A, B] =
        given ExecutionContext = warp.drive.toContext
        Warp(a =>
            warp.jump(a).recoverWith {
                case _ => Warp.startAt[A]
                        .warp(evade)
                        .jump(a)
            }
        )

    extension [A](warp: Warp.FromAnywhere[A])
        /** Punch it!
          */
        def go: Future[A] = warp.jump(())

    extension [A, B](listWarp: Warp[A, List[B]])
        /** Splits the universe into many pieces and runs warp jumps from each location the initial
          * warp sends you to.
          */
        def multiWarp[C](warp: Warp[B, C]): Warp[A, List[C]] =
            given ExecutionContext = listWarp.drive.toContext
            listWarp
                .follow(list =>
                    list.map(warp.jump).foldRight(Future.successful(List.empty[C])) {
                        case (nextFuture, futureList) =>
                            nextFuture.flatMap(next => futureList.map(next :: _))
                    }
                )
