package montokapro.algebra
package lattice

import cats.Traverse
import cats.effect.{Deferred, Ref}
import cats.effect.{IO, _}
import cats.implicits._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.DurationInt

class ConcurrentSetBoolSpec extends AnyFlatSpec with Matchers {
  def parMeetSets[F[_]: Concurrent, G[_]: Traverse, A](
    fas: G[F[Either[Set[A], Set[A]]]]
  ): F[Either[Set[A], Set[A]]] = {
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
          // Replace quard with literal type in Scala 3
          case remaining if remaining == 0 =>
            done.complete(None).void
          case _ =>
            Concurrent[F].unit
        }

    def background(running: Ref[F, Long], fa: F[Unit]): Resource[F, Unit] =
      Resource
        .make(incrementRunning(running) *> Concurrent[F].start(fa))(_.cancel)
        .void

    def reduce(
      set: Either[Set[A], Set[A]]
    )(acc: Either[Set[A], Set[A]]): Either[Set[A], Set[A]] =
      acc match {
        case Left(a) =>
          set match {
            case Left(s) => Left(a | s)
            case Right(s) => Right(s -- a)
          }
        case Right(a) =>
          set match {
            case Left(s) => Right(a -- s)
            case Right(s) => Right(s & a)
          }
      }

    def finish(
      fa: F[Either[Set[A], Set[A]]],
      acc: Ref[F, Either[Set[A], Set[A]]],
      done: Deferred[F, Option[Throwable]],
      running: Ref[F, Long]
    ): F[Unit] = {
      fa.attempt
        .flatMap {
          case Left(e) =>
            done.complete(Some(e)).void
          case Right(set) =>
            acc.updateAndGet(reduce(set)).flatMap {
              // Replace quard with literal type in Scala 3
              case Right(positive) if positive.isEmpty =>
                done.complete(None).void
              case _ =>
                Concurrent[F].unit
            }
        }
        .flatMap(_ => decrementRunning(running, done))
    }

    for {
      acc <- Ref.of(Either.left[Set[A], Set[A]](Set.empty[A]))
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

  import cats.effect.unsafe.implicits.global

  it should "empty" in {
    val list: List[IO[Either[Set[Int], Set[Int]]]] =
      List()

    parMeetSets(list).unsafeRunSync() shouldBe Left(Set.empty)
  }

  it should "single result" in {
    val list: List[IO[Either[Set[Int], Set[Int]]]] =
      List(IO(Right(Set(1))))

    parMeetSets(list).unsafeRunSync() shouldBe Right(Set(1))
  }

  it should "single zero" in {
    val list: List[IO[Either[Set[Int], Set[Int]]]] =
      List(IO(Right(Set.empty)))

    parMeetSets(list).unsafeRunSync() shouldBe Right(Set.empty)
  }

  it should "single error" in {
    val list: List[IO[Either[Set[Int], Set[Int]]]] =
      List(IO.raiseError(new Exception("error")))

    assertThrows[Exception](parMeetSets(list).unsafeRunSync())
  }

  it should "single inverse" in {
    val list: List[IO[Either[Set[Int], Set[Int]]]] =
      List(IO(Left(Set(1))))

    parMeetSets(list).unsafeRunSync() shouldBe Left(Set(1))
  }

  it should "combine zero" in {
    val list: List[IO[Either[Set[Int], Set[Int]]]] =
      List(IO(Right(Set.empty)), IO(Right(Set(1))))

    parMeetSets(list).unsafeRunSync() shouldBe Right(Set.empty)
  }

  it should "combine inverse" in {
    val list: List[IO[Either[Set[Int], Set[Int]]]] =
      List(IO(Left(Set(1))), IO(Right(Set(1, 2))))

    parMeetSets(list).unsafeRunSync() shouldBe Right(Set(2))
  }

  it should "multiple result" in {
    val list: List[IO[Either[Set[Int], Set[Int]]]] =
      List(IO(Right(Set(1, 2, 3))), IO(Right(Set(2, 3, 4))))

    parMeetSets(list).unsafeRunSync() shouldBe Right(Set(2, 3))
  }

  it should "multiple zero" in {
    val list: List[IO[Either[Set[Int], Set[Int]]]] =
      List(IO(Right(Set.empty)), IO(Right(Set.empty)))

    parMeetSets(list).unsafeRunSync() shouldBe Right(Set.empty)
  }

  it should "multiple inverse" in {
    val list: List[IO[Either[Set[Int], Set[Int]]]] =
      List(IO(Left(Set(1))), IO.sleep(1.second) >> IO(Left(Set(2))))

    parMeetSets(list).unsafeRunSync() shouldBe Left(Set(1, 2))
  }

  it should "multiple error" in {
    val list: List[IO[Either[Set[Int], Set[Int]]]] =
      List(IO.raiseError(new Exception("error")), IO(Right(Set(1))))

    assertThrows[Exception](parMeetSets(list).unsafeRunSync())
  }
}
