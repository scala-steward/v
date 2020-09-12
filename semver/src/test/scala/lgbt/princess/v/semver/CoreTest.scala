package lgbt.princess.v
package semver

import lgbt.princess.v.semver.Identifiers.{Build => B, PreRelease => PR}

import scala.collection.immutable.ArraySeq

class CoreTest extends BaseSpec {
  behavior of nameOf[Core]

  it should "not allow negative values" in {
    an[IllegalArgumentException] should be thrownBy Core(-1, 2, 3)
    an[IllegalArgumentException] should be thrownBy Core(1, -2, 3)
    an[IllegalArgumentException] should be thrownBy Core(1, 2, -3)
  }

  it should "compare equality correctly" in {
    Core(1, 2, 3) shouldEqual Core(1, 2, 3)
    Core(1, 2, 3) shouldEqual V3(1, 2, 3)
    Core(1, 2, 3) shouldEqual Variable(1, 2, 3)

    Core(1, 2, 3) should not equal Core(1, 2, 4)
    Core(1, 2, 3) should not equal Core(1, 3, 3)
    Core(1, 2, 3) should not equal Core(2, 2, 3)
    Core(1, 2, 3) should not equal V3(1, 2, 4)
    Core(1, 2, 3) should not equal V3(1, 3, 3)
    Core(1, 2, 3) should not equal V3(2, 2, 3)
    Core(1, 2, 3) should not equal Variable(1, 2, 4)
    Core(1, 2, 3) should not equal Variable(1, 3, 3)
    Core(1, 2, 3) should not equal Variable(2, 2, 3)
    Core(1, 2, 3) should not equal Variable(1, 2, 3, 4)
    Core(1, 2, 3) should not equal new AnyRef
  }

  it should "have a consistent hash code" in {
    Core(1, 2, 3).hashCode shouldEqual Core(1, 2, 3).hashCode
    Core(1, 2, 3).hashCode shouldEqual V3(1, 2, 3).hashCode
    Core(1, 2, 3).hashCode shouldEqual Variable(1, 2, 3).hashCode
  }

  it should "be ordered correctly" in {
    Core(1, 2, 3) should be < Core(1, 2, 4)
    Core(1, 2, 3) should be < Core(1, 3, 0)
    Core(1, 2, 3) should be < Core(2, 0, 0)

    Core(1, 2, 3) < Core(1, 2, 4) shouldBe true
    Core(1, 2, 3) < Core(1, 3, 0) shouldBe true
    Core(1, 2, 3) < Core(2, 0, 0) shouldBe true
  }

  it should "convert from other versions safely (as `Option`s)" in {
    Core.from(V3(1, 2, 3)).value shouldBe Core(1, 2, 3)
    Core.from(Variable(1, 2, 3)).value shouldBe Core(1, 2, 3)

    Core from Variable(1, 2) shouldBe empty
    Core from V2(1, 2) shouldBe empty
    Core from V4(1, 2, 3, 4) shouldBe empty

    val instance = Core(1, 2, 3)
    Core.from(instance).value should be theSameInstanceAs instance
  }

  it should "convert from other versions unsafely (throwing exceptions)" in {
    Core unsafeFrom V3(1, 2, 3) shouldBe Core(1, 2, 3)
    Core unsafeFrom Variable(1, 2, 3) shouldBe Core(1, 2, 3)

    an[IncompatibleVersionException] should be thrownBy { Core unsafeFrom Variable(1, 2) }
    an[IncompatibleVersionException] should be thrownBy { Core unsafeFrom V2(1, 2) }
    an[IncompatibleVersionException] should be thrownBy { Core unsafeFrom V4(1, 2, 3, 4) }

    val instance = Core(1, 2, 3)
    Core unsafeFrom instance should be theSameInstanceAs instance
  }

  it should "be converted to from other versions safely (as `Option`s)" in {
    V3(1, 2, 3).to(Core).value shouldBe Core(1, 2, 3)
    Variable(1, 2, 3).to(Core).value shouldBe Core(1, 2, 3)

    Variable(1, 2) to Core shouldBe empty
    V2(1, 2) to Core shouldBe empty
    V4(1, 2, 3, 4) to Core shouldBe empty
    V3(-1, 2, 3) to Core shouldBe empty
    Variable(-1, 2, 3) to Core shouldBe empty

    val instance = Core(1, 2, 3)
    instance.to(Core).value should be theSameInstanceAs instance
  }

  it should "be converted to from other versions unsafely (throwing exceptions)" in {
    V3(1, 2, 3) unsafeTo Core shouldBe Core(1, 2, 3)
    Variable(1, 2, 3) unsafeTo Core shouldBe Core(1, 2, 3)

    an[IncompatibleVersionException] should be thrownBy { Variable(1, 2) unsafeTo Core }
    an[IncompatibleVersionException] should be thrownBy { V2(1, 2) unsafeTo Core }
    an[IncompatibleVersionException] should be thrownBy { V4(1, 2, 3, 4) unsafeTo Core }
    an[IncompatibleVersionException] should be thrownBy { V3(-1, 2, 3) unsafeTo Core }
    an[IncompatibleVersionException] should be thrownBy { Variable(-1, 2, 3) unsafeTo Core }

    val instance = Core(1, 2, 3)
    instance unsafeTo Core should be theSameInstanceAs instance
  }

  it should "convert from sequences safely (as `Option`s)" in {
    Core.fromSeq(ArraySeq(1, 2, 3)).value shouldBe Core(1, 2, 3)

    Core.fromSeq(ArraySeq()) shouldBe empty
    Core.fromSeq(ArraySeq(1)) shouldBe empty
    Core.fromSeq(ArraySeq(1, 2)) shouldBe empty
    Core.fromSeq(ArraySeq(1, 2, 3, 4)) shouldBe empty
    Core.fromSeq(ArraySeq(-1, 2, 3)) shouldBe empty
  }

  it should "convert from sequences unsafely (throwing exceptions)" in {
    Core unsafeFromSeq ArraySeq(1, 2, 3) shouldBe Core(1, 2, 3)

    an[IllegalArgumentException] should be thrownBy { Core unsafeFromSeq ArraySeq() }
    an[IncompatibleVersionException] should be thrownBy { Core unsafeFromSeq ArraySeq(1) }
    an[IncompatibleVersionException] should be thrownBy { Core unsafeFromSeq ArraySeq(1, 2) }
    an[IncompatibleVersionException] should be thrownBy { Core unsafeFromSeq ArraySeq(1, 2, 3, 4) }
    an[IncompatibleVersionException] should be thrownBy { Core unsafeFromSeq ArraySeq(-1, 2, 3) }
  }

  it should "parse versions safely (as `Option`s)" in {
    Core.parse("1.2.3").value shouldBe Core(1, 2, 3)
    Core.parse("0.1.0").value shouldBe Core(0, 1, 0)

    Core.parse("") shouldBe empty
    Core.parse(" ") shouldBe empty
    Core.parse("-1.2.3") shouldBe empty
    Core.parse("1.2") shouldBe empty
    Core.parse("1.2.3.") shouldBe empty
    Core.parse(".1.2.3") shouldBe empty
    Core.parse("1.2.3.4") shouldBe empty
    Core.parse("not a version") shouldBe empty
  }

  it should "parse versions unsafely (throwing exceptions)" in {
    Core unsafeParse "1.2.3" shouldBe Core(1, 2, 3)
    Core unsafeParse "0.1.0" shouldBe Core(0, 1, 0)

    a[VersionFormatException] should be thrownBy { Core unsafeParse "" }
    a[VersionFormatException] should be thrownBy { Core unsafeParse " " }
    a[VersionFormatException] should be thrownBy { Core unsafeParse "-1.2.3" }
    a[VersionFormatException] should be thrownBy { Core unsafeParse "1.2" }
    a[VersionFormatException] should be thrownBy { Core unsafeParse "1.2.3." }
    a[VersionFormatException] should be thrownBy { Core unsafeParse ".1.2.3" }
    a[VersionFormatException] should be thrownBy { Core unsafeParse "1.2.3.4" }
    a[VersionFormatException] should be thrownBy { Core unsafeParse "not a version" }
  }

  it should "construct SemVer versions using symbolic and extension methods" in {
    Core(1, 2, 3).toSemVer shouldEqual SemVer(Core(1, 2, 3))
    (Core(1, 2, 3) - PR("alpha"): SemVer) shouldEqual SemVer(Core(1, 2, 3), PR("alpha"))
    (Core(1, 2, 3) - PR("alpha")).toSemVer shouldEqual SemVer(Core(1, 2, 3), PR("alpha"))
    (Core(1, 2, 3) - PR("alpha")).withoutMetadata shouldEqual SemVer(Core(1, 2, 3), PR("alpha"))
    Core(1, 2, 3) + B("12") shouldEqual SemVer(Core(1, 2, 3), B("12"))
    Core(1, 2, 3) - PR("alpha") + B("12") shouldEqual SemVer(Core(1, 2, 3), PR("alpha"), B("12"))
  }
}
