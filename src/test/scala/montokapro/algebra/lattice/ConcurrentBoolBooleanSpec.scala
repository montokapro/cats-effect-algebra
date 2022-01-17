package montokapro.algebra
package lattice

import algebra.instances.boolean._
import cats.Traverse
import cats.effect.{Deferred, Ref}
import cats.effect.{IO, _}
import cats.implicits._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import cats.effect.unsafe.implicits.global

class ConcurrentBoolBooleanSpec extends AnyFlatSpec with Matchers {
  it should "empty" in {
    val list: List[IO[Boolean]] =
      List()

    ConcurrentBool.parMeet(list).unsafeRunSync() shouldBe true
  }

  it should "single false" in {
    val list: List[IO[Boolean]] =
      List(false).map(IO(_))

    ConcurrentBool.parMeet(list).unsafeRunSync() shouldBe false
  }

  it should "single true" in {
    val list: List[IO[Boolean]] =
      List(true).map(IO(_))

    ConcurrentBool.parMeet(list).unsafeRunSync() shouldBe true
  }

  it should "multiple false" in {
    val list: List[IO[Boolean]] =
      List(false, false).map(IO(_))

    ConcurrentBool.parMeet(list).unsafeRunSync() shouldBe false
  }

  it should "multiple true" in {
    val list: List[IO[Boolean]] =
      List(true, true).map(IO(_))

    ConcurrentBool.parMeet(list).unsafeRunSync() shouldBe true
  }

  it should "mixed" in {
    val list: List[IO[Boolean]] =
      List(false, true, false, true).map(IO(_))

    ConcurrentBool.parMeet(list).unsafeRunSync() shouldBe false
  }
}
