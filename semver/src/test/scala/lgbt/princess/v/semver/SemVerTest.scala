package lgbt.princess.v
package semver

import lgbt.princess.v.semver.Identifiers.{Build => B, PreRelease => PR}

class SemVerTest extends BaseSpec {
  behavior of nameOf[SemVer]

  it should "construct correctly using secondary constructors" in {
    SemVer(Core(1, 2, 3)) shouldEqual SemVer(Core(1, 2, 3), None, None)
    SemVer(Core(1, 2, 3), PR("alpha")) shouldEqual SemVer(Core(1, 2, 3), Some(PR("alpha")), None)
    SemVer(Core(1, 2, 3), B("12")) shouldEqual SemVer(Core(1, 2, 3), None, Some(B("12")))
    SemVer(Core(1, 2, 3), PR("alpha"), B("12")) shouldEqual SemVer(Core(1, 2, 3), Some(PR("alpha")), Some(B("12")))
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

  it should "extract from a string correctly" in {
    "1.2.3" shouldMatch { case SemVer(Core(1, 2, 3), None, None) => }
    "1.2.3-alpha" shouldMatch { case SemVer(Core(1, 2, 3), Some(PR("alpha")), None) => }
    "1.2.3+12" shouldMatch { case SemVer(Core(1, 2, 3), None, Some(B("12"))) => }
    "1.2.3-alpha+12" shouldMatch { case SemVer(Core(1, 2, 3), Some(_), Some(B("12"))) => }

    "-1.2.3" shouldNotMatch { case SemVer(_, _, _) => }
    "1.-2.3" shouldNotMatch { case SemVer(_, _, _) => }
    "1.2.3-01" shouldNotMatch { case SemVer(_, _, _) => }
    "1.2.3+foo..bar" shouldNotMatch { case SemVer(_, _, _) => }
    "1.2.3-alpha+foo_bar" shouldNotMatch { case SemVer(_, _, _) => }
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
