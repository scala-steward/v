package lgbt.princess.v
package semver

import lgbt.princess.v.semver.Identifiers.{Build => B, PreRelease => PR}
import org.scalactic.source.Position
import org.scalatest.Assertion

class SemVerTest extends BaseSpec {
  behavior of nameOf[SemVer]

  private implicit final class ShouldBeEquiv[A](self: A) {
    def shouldBeEquiv(that: A)(implicit ord: Ordering[A], pos: Position): Assertion =
      ord.equiv(self, that) shouldBe true
  }

  private implicit final class OrderedEquiv[A](self: Ordered[A]) {
    def equiv(that: A): Boolean = (self compare that) == 0
  }

  it should "construct correctly using secondary constructors" in {
    SemVer(Core(1, 2, 3)) shouldEqual SemVer(Core(1, 2, 3), None, None)
    SemVer(Core(1, 2, 3), PR("alpha")) shouldEqual SemVer(Core(1, 2, 3), Some(PR("alpha")), None)
    SemVer(Core(1, 2, 3), B("12")) shouldEqual SemVer(Core(1, 2, 3), None, Some(B("12")))
    SemVer(Core(1, 2, 3), PR("alpha"), B("12")) shouldEqual SemVer(Core(1, 2, 3), Some(PR("alpha")), Some(B("12")))
  }

  it should "be ordered correctly" in {
    SemVer(Core(1, 2, 3)) should be < SemVer(Core(1, 2, 4), PR("alpha"))
    SemVer(Core(1, 2, 3)) should be < SemVer(Core(1, 3, 0), PR("alpha"))
    SemVer(Core(1, 2, 3)) should be < SemVer(Core(2, 0, 0), PR("alpha"))
    SemVer(Core(1, 2, 3)) should be < SemVer(Core(1, 2, 4))
    SemVer(Core(1, 2, 3)) should be < SemVer(Core(1, 3, 0))
    SemVer(Core(1, 2, 3)) should be < SemVer(Core(2, 0, 0))
    SemVer(Core(1, 2, 3), PR("alpha")) should be < SemVer(Core(1, 2, 3))

    SemVer(Core(1, 2, 3), PR("alpha")) should be < SemVer(Core(1, 2, 3), PR("beta"))
    SemVer(Core(1, 2, 3)) shouldBeEquiv SemVer(Core(1, 2, 3), B("12"))
    SemVer(Core(1, 2, 3), B("11")) shouldBeEquiv SemVer(Core(1, 2, 3), B("12"))
    SemVer(Core(1, 2, 3), PR("alpha"), B("11")) shouldBeEquiv SemVer(Core(1, 2, 3), PR("alpha"), B("12"))
    SemVer(Core(1, 2, 3), PR("alpha"), B("11")) should be < SemVer(Core(1, 2, 3), PR("beta"), B("12"))
    SemVer(Core(1, 2, 3), PR("alpha"), B("12")) should be < SemVer(Core(1, 2, 3), PR("beta"), B("12"))

    SemVer(Core(1, 2, 3)) < SemVer(Core(1, 2, 4), PR("alpha")) shouldBe true
    SemVer(Core(1, 2, 3)) < SemVer(Core(1, 3, 0), PR("alpha")) shouldBe true
    SemVer(Core(1, 2, 3)) < SemVer(Core(2, 0, 0), PR("alpha")) shouldBe true
    SemVer(Core(1, 2, 3)) < SemVer(Core(1, 2, 4)) shouldBe true
    SemVer(Core(1, 2, 3)) < SemVer(Core(1, 3, 0)) shouldBe true
    SemVer(Core(1, 2, 3)) < SemVer(Core(2, 0, 0)) shouldBe true
    SemVer(Core(1, 2, 3), PR("alpha")) < SemVer(Core(1, 2, 3)) shouldBe true

    SemVer(Core(1, 2, 3), PR("alpha")) < SemVer(Core(1, 2, 3), PR("beta")) shouldBe true
    SemVer(Core(1, 2, 3)) equiv SemVer(Core(1, 2, 3), B("12")) shouldBe true
    SemVer(Core(1, 2, 3), B("11")) equiv SemVer(Core(1, 2, 3), B("12")) shouldBe true
    SemVer(Core(1, 2, 3), PR("alpha"), B("11")) equiv SemVer(Core(1, 2, 3), PR("alpha"), B("12")) shouldBe true
    SemVer(Core(1, 2, 3), PR("alpha"), B("11")) < SemVer(Core(1, 2, 3), PR("beta"), B("12")) shouldBe true
    SemVer(Core(1, 2, 3), PR("alpha"), B("12")) < SemVer(Core(1, 2, 3), PR("beta"), B("12")) shouldBe true
  }

  it should "render as a string correctly" in {
    SemVer(Core(1, 2, 3)).toString shouldBe "1.2.3"
    SemVer(Core(1, 2, 3), PR("alpha")).toString shouldBe "1.2.3-alpha"
    SemVer(Core(1, 2, 3), B("12")).toString shouldBe "1.2.3+12"
    SemVer(Core(1, 2, 3), PR("alpha"), B("12")).toString shouldBe "1.2.3-alpha+12"
  }

  it should "parse as an Option" in {
    SemVer.parse("1.2.3").value shouldEqual SemVer(Core(1, 2, 3))
    SemVer.parse("1.2.3-alpha").value shouldEqual SemVer(Core(1, 2, 3), PR("alpha"))
    SemVer.parse("1.2.3+12").value shouldEqual SemVer(Core(1, 2, 3), B("12"))
    SemVer.parse("1.2.3-alpha+12").value shouldEqual SemVer(Core(1, 2, 3), PR("alpha"), B("12"))

    SemVer.parse("-1.2.3") shouldBe empty
    SemVer.parse("1.-2.3") shouldBe empty
    SemVer.parse("1.2.3-01") shouldBe empty
    SemVer.parse("1.2.3+foo..bar") shouldBe empty
    SemVer.parse("1.2.3-alpha+foo_bar") shouldBe empty
  }

  it should "parse unsafely" in {
    SemVer.unsafeParse("1.2.3") shouldEqual SemVer(Core(1, 2, 3))
    SemVer.unsafeParse("1.2.3-alpha") shouldEqual SemVer(Core(1, 2, 3), PR("alpha"))
    SemVer.unsafeParse("1.2.3+12") shouldEqual SemVer(Core(1, 2, 3), B("12"))
    SemVer.unsafeParse("1.2.3-alpha+12") shouldEqual SemVer(Core(1, 2, 3), PR("alpha"), B("12"))

    a[VersionFormatException] should be thrownBy SemVer.unsafeParse("-1.2.3")
    a[VersionFormatException] should be thrownBy SemVer.unsafeParse("1.-2.3")
    a[VersionFormatException] should be thrownBy SemVer.unsafeParse("1.2.3-01")
    a[VersionFormatException] should be thrownBy SemVer.unsafeParse("1.2.3+foo..bar")
    a[VersionFormatException] should be thrownBy SemVer.unsafeParse("1.2.3-alpha+foo_bar")
  }

  it should "extract using symbols correctly" in {
    SemVer(Core(1, 2, 3), PR("alpha")) shouldMatch { case Core(1, 2, 3) - PR("alpha") => }
    SemVer(Core(1, 2, 3), B("12")) shouldMatch { case Core(1, 2, 3) + B("12") => }
    SemVer(Core(1, 2, 3), PR("alpha"), B("12")) shouldMatch { case Core(1, 2, 3) :- PR("alpha") + B("12") => }

    SemVer(Core(1, 2, 3)) shouldNotMatch { case _ - _ => }
    SemVer(Core(1, 2, 3), B("12")) shouldNotMatch { case _ - _ => }
    SemVer(Core(1, 2, 3), PR("alpha"), B("12")) shouldNotMatch { case _ - _ => }
    SemVer(Core(1, 2, 3)) shouldNotMatch { case _ + _ => }
    SemVer(Core(1, 2, 3), PR("alpha")) shouldNotMatch { case _ + _ => }
    SemVer(Core(1, 2, 3), PR("alpha"), B("12")) shouldNotMatch { case _ + _ => }
    SemVer(Core(1, 2, 3)) shouldNotMatch { case _ :- _ + _ => }
    SemVer(Core(1, 2, 3), PR("alpha")) shouldNotMatch { case _ :- _ + _ => }
    SemVer(Core(1, 2, 3), B("12")) shouldNotMatch { case _ :- _ + _ => }
  }
}
