package lgbt.princess.v

import scala.collection.immutable.ArraySeq

class V3Test extends BaseSpec {
  behavior of nameOf[V3]

  it should "allow negative values" in {
    noException should be thrownBy V3(-1, -2, -3)
  }

  it should "have product-/tuple-like accessors" in {
    val v = V3(1, 2, 3)
    v._1 shouldBe 1
    v._2 shouldBe 2
    v._3 shouldBe 3
  }

  it should "compare equality correctly" in {
    V3(1, 2, 3) shouldEqual V3(1, 2, 3)
    V3(1, 2, 3) shouldEqual Variable(1, 2, 3)

    V3(1, 2, 3) should not equal V3(1, 2, 4)
    V3(1, 2, 3) should not equal V3(1, 3, 3)
    V3(1, 2, 3) should not equal V3(2, 2, 3)
    V3(1, 2, 3) should not equal Variable(1, 2, 4)
    V3(1, 2, 3) should not equal Variable(1, 3, 3)
    V3(1, 2, 3) should not equal Variable(2, 2, 3)
    V3(1, 2, 3) should not equal Variable(1, 2, 3, 4)
    V3(1, 2, 3) should not equal new AnyRef
  }

  it should "have a consistent hash code" in {
    V3(1, 2, 3).hashCode shouldEqual V3(1, 2, 3).hashCode
    V3(1, 2, 3).hashCode shouldEqual Variable(1, 2, 3).hashCode
  }

  it should "be ordered correctly" in {
    V3(1, 2, 3) should be < V3(1, 2, 4)
    V3(1, 2, 3) should be < V3(1, 3, 0)
    V3(1, 2, 3) should be < V3(2, 0, 0)

    V3(1, 2, 3) < V3(1, 2, 4) shouldBe true
    V3(1, 2, 3) < V3(1, 3, 0) shouldBe true
    V3(1, 2, 3) < V3(2, 0, 0) shouldBe true
  }

  it should "convert from other versions safely (as `Option`s)" in {
    V3.from(Variable(1, 2, 3)).value shouldBe V3(1, 2, 3)

    V3 from Variable(1, 2) shouldBe empty
    V3 from V2(1, 2) shouldBe empty
    V3 from V4(1, 2, 3, 4) shouldBe empty

    val instance = V3(1, 2, 3)
    V3.from(instance).value should be theSameInstanceAs instance
  }

  it should "convert from other versions unsafely (throwing exceptions)" in {
    V3 unsafeFrom Variable(1, 2, 3) shouldBe V3(1, 2, 3)

    an[IncompatibleVersionException] should be thrownBy { V3 unsafeFrom Variable(1, 2) }
    an[IncompatibleVersionException] should be thrownBy { V3 unsafeFrom V2(1, 2) }
    an[IncompatibleVersionException] should be thrownBy { V3 unsafeFrom V4(1, 2, 3, 4) }

    val instance = V3(1, 2, 3)
    V3 unsafeFrom instance should be theSameInstanceAs instance
  }

  it should "be converted to from other versions safely (as `Option`s)" in {
    Variable(1, 2, 3).to(V3).value shouldBe V3(1, 2, 3)

    Variable(1, 2) to V3 shouldBe empty
    V2(1, 2) to V3 shouldBe empty
    V4(1, 2, 3, 4) to V3 shouldBe empty

    val instance = V3(1, 2, 3)
    instance.to(V3).value should be theSameInstanceAs instance
  }

  it should "be converted to from other versions unsafely (throwing exceptions)" in {
    Variable(1, 2, 3) unsafeTo V3 shouldBe V3(1, 2, 3)

    an[IncompatibleVersionException] should be thrownBy { Variable(1, 2) unsafeTo V3 }
    an[IncompatibleVersionException] should be thrownBy { V2(1, 2) unsafeTo V3 }
    an[IncompatibleVersionException] should be thrownBy { V4(1, 2, 3, 4) unsafeTo V3 }

    val instance = V3(1, 2, 3)
    instance unsafeTo V3 should be theSameInstanceAs instance
  }

  it should "convert from sequences safely (as `Option`s)" in {
    V3.fromSeq(ArraySeq(1, 2, 3)).value shouldBe V3(1, 2, 3)

    V3.fromSeq(ArraySeq()) shouldBe empty
    V3.fromSeq(ArraySeq(1)) shouldBe empty
    V3.fromSeq(ArraySeq(1, 2)) shouldBe empty
    V3.fromSeq(ArraySeq(1, 2, 3, 4)) shouldBe empty
  }

  it should "convert from sequences unsafely (throwing exceptions)" in {
    V3 unsafeFromSeq ArraySeq(1, 2, 3) shouldBe V3(1, 2, 3)

    an[IllegalArgumentException] should be thrownBy { V3 unsafeFromSeq ArraySeq() }
    an[IncompatibleVersionException] should be thrownBy { V3 unsafeFromSeq ArraySeq(1) }
    an[IncompatibleVersionException] should be thrownBy { V3 unsafeFromSeq ArraySeq(1, 2) }
    an[IncompatibleVersionException] should be thrownBy { V3 unsafeFromSeq ArraySeq(1, 2, 3, 4) }
  }

  it should "parse versions safely (as `Option`s)" in {
    V3.parse("1.2.3").value shouldBe V3(1, 2, 3)
    V3.parse("0.1.0").value shouldBe V3(0, 1, 0)
    V3.parse("-1.-2.-3").value shouldBe V3(-1, -2, -3)

    V3.parse("") shouldBe empty
    V3.parse(" ") shouldBe empty
    V3.parse("1.2") shouldBe empty
    V3.parse("1.2.3.") shouldBe empty
    V3.parse(".1.2.3") shouldBe empty
    V3.parse("1.2.3.4") shouldBe empty
    V3.parse("not a version") shouldBe empty
  }

  it should "parse versions unsafely (throwing exceptions)" in {
    V3 unsafeParse "1.2.3" shouldBe V3(1, 2, 3)
    V3 unsafeParse "0.1.0" shouldBe V3(0, 1, 0)
    V3 unsafeParse "-1.-2.-3" shouldBe V3(-1, -2, -3)

    a[VersionFormatException] should be thrownBy { V3 unsafeParse "" }
    a[VersionFormatException] should be thrownBy { V3 unsafeParse " " }
    a[VersionFormatException] should be thrownBy { V3 unsafeParse "1.2" }
    a[VersionFormatException] should be thrownBy { V3 unsafeParse "1.2.3." }
    a[VersionFormatException] should be thrownBy { V3 unsafeParse ".1.2.3" }
    a[VersionFormatException] should be thrownBy { V3 unsafeParse "1.2.3.4" }
    a[VersionFormatException] should be thrownBy { V3 unsafeParse "not a version" }
  }
}
