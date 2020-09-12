package lgbt.princess.v

import scala.collection.immutable.ArraySeq

class V2Test extends BaseSpec {
  behavior of nameOf[V2]

  it should "allow negative values" in {
    noException should be thrownBy V2(-1, -2)
  }

  it should "have product-/tuple-like accessors" in {
    val v = V2(1, 2)
    v._1 shouldBe 1
    v._2 shouldBe 2
  }

  it should "compare equality correctly" in {
    V2(1, 2) shouldEqual V2(1, 2)
    V2(1, 2) shouldEqual Variable(1, 2)

    V2(1, 2) should not equal V2(1, 3)
    V2(1, 2) should not equal V2(2, 2)
    V2(1, 2) should not equal Variable(1, 3)
    V2(1, 2) should not equal Variable(2, 2)
    V2(1, 2) should not equal Variable(1, 2, 3)
    V2(1, 2) should not equal new AnyRef
  }

  it should "have a consistent hash code" in {
    V2(1, 2).hashCode shouldEqual V2(1, 2).hashCode
    V2(1, 2).hashCode shouldEqual Variable(1, 2).hashCode
  }

  it should "be ordered correctly" in {
    V2(1, 2) should be < V2(1, 3)
    V2(1, 2) should be < V2(2, 0)

    V2(1, 2) < V2(1, 3) shouldBe true
    V2(1, 2) < V2(2, 0) shouldBe true
  }

  it should "convert from other versions safely (as `Option`s)" in {
    V2.from(Variable(1, 2)).value shouldBe V2(1, 2)

    V2 from Variable(1, 2, 3) shouldBe empty
    V2 from V3(1, 2, 3) shouldBe empty
    V2 from V4(1, 2, 3, 4) shouldBe empty

    val instance = V2(1, 2)
    V2.from(instance).value should be theSameInstanceAs instance
  }

  it should "convert from other versions unsafely (throwing exceptions)" in {
    V2 unsafeFrom Variable(1, 2) shouldBe V2(1, 2)

    an[IncompatibleVersionException] should be thrownBy { V2 unsafeFrom Variable(1, 2, 3) }
    an[IncompatibleVersionException] should be thrownBy { V2 unsafeFrom V3(1, 2, 3) }
    an[IncompatibleVersionException] should be thrownBy { V2 unsafeFrom V4(1, 2, 3, 4) }

    val instance = V2(1, 2)
    V2 unsafeFrom instance should be theSameInstanceAs instance
  }

  it should "be converted to from other versions safely (as `Option`s)" in {
    Variable(1, 2).to(V2).value shouldBe V2(1, 2)

    Variable(1, 2, 3) to V2 shouldBe empty
    V3(1, 2, 3) to V2 shouldBe empty
    V4(1, 2, 3, 4) to V2 shouldBe empty

    val instance = V2(1, 2)
    instance.to(V2).value should be theSameInstanceAs instance
  }

  it should "be converted to from other versions unsafely (throwing exceptions)" in {
    Variable(1, 2) unsafeTo V2 shouldBe V2(1, 2)

    an[IncompatibleVersionException] should be thrownBy { Variable(1, 2, 3) unsafeTo V2 }
    an[IncompatibleVersionException] should be thrownBy { V3(1, 2, 3) unsafeTo V2 }
    an[IncompatibleVersionException] should be thrownBy { V4(1, 2, 3, 4) unsafeTo V2 }

    val instance = V2(1, 2)
    instance unsafeTo V2 should be theSameInstanceAs instance
  }

  it should "convert from sequences safely (as `Option`s)" in {
    V2.fromSeq(ArraySeq(1, 2)).value shouldBe V2(1, 2)

    V2.fromSeq(ArraySeq()) shouldBe empty
    V2.fromSeq(ArraySeq(1)) shouldBe empty
    V2.fromSeq(ArraySeq(1, 2, 3)) shouldBe empty
    V2.fromSeq(ArraySeq(1, 2, 3, 4)) shouldBe empty
  }

  it should "convert from sequences unsafely (throwing exceptions)" in {
    V2 unsafeFromSeq ArraySeq(1, 2) shouldBe V2(1, 2)

    an[IllegalArgumentException] should be thrownBy { V2 unsafeFromSeq ArraySeq() }
    an[IncompatibleVersionException] should be thrownBy { V2 unsafeFromSeq ArraySeq(1) }
    an[IncompatibleVersionException] should be thrownBy { V2 unsafeFromSeq ArraySeq(1, 2, 3) }
    an[IncompatibleVersionException] should be thrownBy { V2 unsafeFromSeq ArraySeq(1, 2, 3, 4) }
  }

  it should "parse versions safely (as `Option`s)" in {
    V2.parse("1.2").value shouldBe V2(1, 2)
    V2.parse("0.1").value shouldBe V2(0, 1)
    V2.parse("-1.-2").value shouldBe V2(-1, -2)

    V2.parse("") shouldBe empty
    V2.parse(" ") shouldBe empty
    V2.parse("1") shouldBe empty
    V2.parse("1.2.") shouldBe empty
    V2.parse(".1.2") shouldBe empty
    V2.parse("1.2.3") shouldBe empty
    V2.parse("not a version") shouldBe empty
  }

  it should "parse versions unsafely (throwing exceptions)" in {
    V2 unsafeParse "1.2" shouldBe V2(1, 2)
    V2 unsafeParse "0.1" shouldBe V2(0, 1)
    V2 unsafeParse "-1.-2" shouldBe V2(-1, -2)

    a[VersionFormatException] should be thrownBy { V2 unsafeParse "" }
    a[VersionFormatException] should be thrownBy { V2 unsafeParse " " }
    a[VersionFormatException] should be thrownBy { V2 unsafeParse "1" }
    a[VersionFormatException] should be thrownBy { V2 unsafeParse "1.2." }
    a[VersionFormatException] should be thrownBy { V2 unsafeParse ".1.2" }
    a[VersionFormatException] should be thrownBy { V2 unsafeParse "1.2.3" }
    a[VersionFormatException] should be thrownBy { V2 unsafeParse "not a version" }
  }
}
