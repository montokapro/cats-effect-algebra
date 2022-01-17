package montokapro.algebra
package lattice

import algebra.lattice.Bool
import cats.{Eq, Traverse}
import cats.effect.{Deferred, Ref}
import cats.effect.{IO, _}
import cats.implicits._

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.DurationInt

object ConcurrentBool {
  def parMeet[F[_]: Concurrent, G[_]: Traverse, A: Bool: Eq](
    fas: G[F[A]]
  ): F[A] = {
    def incrementRunning(running: Ref[F, Long]): F[Unit] =
      running
        .update(_ + 1)

    def decrementRunning(
      running: Ref[F, Long],
      done: Deferred[F, Option[Throwable]]
    ): F[Unit] =
      running
        .updateAndGet(_ - 1)
        .flatMap {
          case 0 =>
            done.complete(None).void
          case _ =>
            Concurrent[F].unit
        }

    def background(running: Ref[F, Long], fa: F[Unit]): Resource[F, Unit] =
      Resource
        .make(incrementRunning(running) *> Concurrent[F].start(fa))(_.cancel)
        .void

    def finish(
      fa: F[A],
      acc: Ref[F, A],
      done: Deferred[F, Option[Throwable]],
      running: Ref[F, Long]
    ): F[Unit] = {
      fa.attempt
        .flatMap {
          case Left(e) =>
            done.complete(Some(e)).void
          case Right(v) =>
            acc.updateAndGet(Bool[A].and(_, v)).flatMap { z =>
              if (Bool[A].isZero(z)) {
                done.complete(None).void
              } else {
                Concurrent[F].unit
              }
            }
        }
        .flatMap(_ => decrementRunning(running, done))
    }

    for {
      acc <- Ref.of(Bool[A].one)
      done <- Deferred[F, Option[Throwable]]
      running <- Ref.of(1L)
      promise <- fas
        .traverse(fa => background(running, finish(fa, acc, done, running)))
        .use(_ => decrementRunning(running, done) *> done.get)
      result <- promise match {
        case Some(e) => Concurrent[F].raiseError(e)
        case None => acc.get
      }
    } yield result
  }
}
