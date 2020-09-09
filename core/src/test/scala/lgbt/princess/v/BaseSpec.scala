package lgbt.princess.v

import org.scalactic.source.Position
import org.scalatest.{Assertion, OptionValues}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.reflect.ClassTag

abstract class BaseSpec extends AnyFlatSpec with Matchers with OptionValues {
  def nameOf[A](implicit tag: ClassTag[A]): String = tag.runtimeClass.getSimpleName

  implicit final class ShouldPossiblyMatch[A](a: A) {
    def shouldMatch[U](pf: PartialFunction[A, U])(implicit pos: Position): Assertion =
      pf.isDefinedAt(a) shouldBe true

    def shouldNotMatch[U](pf: PartialFunction[A, U])(implicit pos: Position): Assertion =
      pf.isDefinedAt(a) shouldBe false
  }
}
