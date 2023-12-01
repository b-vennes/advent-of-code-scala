package aoc.tools

import scala.util.*
import scala.concurrent.*
import scala.annotation.targetName

opaque type Drive = ExecutionContext

object Drive:
    val standard: Drive = scala.concurrent.ExecutionContext.global

    def summon(using context: ExecutionContext): Drive = context

    extension (drive: Drive)
        def toContext: ExecutionContext = drive

case class Warp[-A, +B](path: A => Future[B], drive: Drive):
    given ExecutionContext = drive.toContext

    def move[C](move: B => C): Warp[A, C] =
        Warp: (a: A) =>
            val futureB = path(a)
            val futureC = futureB.map(move)
            futureC
        .withDrive(drive)

    def follow[C](follow: B => Future[C]): Warp[A, C] =
        Warp: (a: A) =>
            val futureB = path(a)
            val futureC = futureB.flatMap(follow)
            futureC
        .withDrive(drive)

    def warp[C](warp: Warp[B, C]): Warp[A, C] =
        Warp: (a: A) =>
            val futureB = path(a)
            val futureC = futureB
                .flatMap(b =>
                    warp.jump(b)
                )
            futureC
        .withDrive(drive)

    def calculate[C](calculate: B => Warp.FromAnywhere[C]): Warp[A, C] =
        Warp: (a: A) =>
            path(a).map(calculate).flatMap(_.go)
        .withDrive(drive)

    def jump(start: A): Future[B] =
        path(start)

    def withDrive(drive: Drive): Warp[A, B] = copy(drive = drive)

object Warp:

    type FromAnywhere[A] = Warp[Any, A]

    def apply[A, B](path: A => Future[B]): Warp[A, B] =
        new Warp(path, Drive.standard)

    def startAt[A]: Warp[A, A] = new Warp(a => Future.fromTry(Try(a)), Drive.standard)

    def calculate[A, B](calculate: A => Warp.FromAnywhere[B]): Warp[A, B] =
        def path(a: A): Future[B] =
            calculate(a).go
        new Warp(path, Drive.standard)

    def toPoint[A](to: => Future[A]): Warp.FromAnywhere[A] =
        new Warp((_: Any) => to, Drive.standard)

    def toLocation[A](location: => A): Warp.FromAnywhere[A] =
        new Warp((_: Any) => Future.fromTry(Try(location)), Drive.standard)

    def doomed(failure: => Throwable, drive: Drive = Drive.standard): Warp[Any, Nothing] =
        new Warp(any => Future.failed(failure), drive)

    extension [A](warp: Warp.FromAnywhere[A])
        def go: Future[A] = warp.jump(())

    extension [A, B](listWarp: Warp[A, List[B]])
        def multiverseWarp[C](warp: Warp[B, C]): Warp[A, List[C]] =
            given ExecutionContext = listWarp.drive.toContext
            listWarp
                .follow(list =>
                    list.map(warp.jump).foldRight(Future.successful(List.empty[C])):
                        case (nextFuture, futureList) =>
                            nextFuture.flatMap(next => futureList.map(next :: _))
                )
