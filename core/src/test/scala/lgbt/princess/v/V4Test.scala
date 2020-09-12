package lgbt.princess.v

import scala.collection.immutable.ArraySeq

class V4Test extends BaseSpec {
  behavior of nameOf[V4]

  it should "allow negative values" in {
    noException should be thrownBy V4(-1, -2, -3, -4)
  }

  it should "have product-/tuple-like accessors" in {
    val v = V4(1, 2, 3, 4)
    v._1 shouldBe 1
    v._2 shouldBe 2
    v._3 shouldBe 3
    v._4 shouldBe 4
  }

  it should "compare equality correctly" in {
    V4(1, 2, 3, 4) shouldEqual V4(1, 2, 3, 4)
    V4(1, 2, 3, 4) shouldEqual Variable(1, 2, 3, 4)

    V4(1, 2, 3, 4) should not equal V4(1, 2, 3, 5)
    V4(1, 2, 3, 4) should not equal V4(1, 2, 4, 4)
    V4(1, 2, 3, 4) should not equal V4(1, 3, 3, 4)
    V4(1, 2, 3, 4) should not equal V4(2, 2, 3, 4)
    V4(1, 2, 3, 4) should not equal Variable(1, 2, 3, 5)
    V4(1, 2, 3, 4) should not equal Variable(1, 2, 4, 4)
    V4(1, 2, 3, 4) should not equal Variable(1, 3, 3, 4)
    V4(1, 2, 3, 4) should not equal Variable(2, 2, 3, 4)
    V4(1, 2, 3, 4) should not equal Variable(1, 2, 3, 4, 5)
    V4(1, 2, 3, 4) should not equal new AnyRef
  }

  it should "have a consistent hash code" in {
    V4(1, 2, 3, 4).hashCode shouldEqual V4(1, 2, 3, 4).hashCode
    V4(1, 2, 3, 4).hashCode shouldEqual Variable(1, 2, 3, 4).hashCode
  }

  it should "be ordered correctly" in {
    V4(1, 2, 3, 4) should be < V4(1, 2, 3, 5)
    V4(1, 2, 3, 4) should be < V4(1, 2, 4, 0)
    V4(1, 2, 3, 4) should be < V4(1, 3, 0, 0)
    V4(1, 2, 3, 4) should be < V4(2, 0, 0, 0)

    V4(1, 2, 3, 4) < V4(1, 2, 3, 5) shouldBe true
    V4(1, 2, 3, 4) < V4(1, 2, 4, 0) shouldBe true
    V4(1, 2, 3, 4) < V4(1, 3, 0, 0) shouldBe true
    V4(1, 2, 3, 4) < V4(2, 0, 0, 0) shouldBe true
  }

  it should "convert from other versions safely (as `Option`s)" in {
    V4.from(Variable(1, 2, 3, 4)).value shouldBe V4(1, 2, 3, 4)

    V4 from Variable(1, 2) shouldBe empty
    V4 from V2(1, 2) shouldBe empty
    V4 from V3(1, 2, 3) shouldBe empty

    val instance = V4(1, 2, 3, 4)
    V4.from(instance).value should be theSameInstanceAs instance
  }

  it should "convert from other versions unsafely (throwing exceptions)" in {
    V4 unsafeFrom Variable(1, 2, 3, 4) shouldBe V4(1, 2, 3, 4)

    an[IncompatibleVersionException] should be thrownBy { V4 unsafeFrom Variable(1, 2) }
    an[IncompatibleVersionException] should be thrownBy { V4 unsafeFrom V2(1, 2) }
    an[IncompatibleVersionException] should be thrownBy { V4 unsafeFrom V3(1, 2, 3) }

    val instance = V4(1, 2, 3, 4)
    V4 unsafeFrom instance should be theSameInstanceAs instance
  }

  it should "be converted to from other versions safely (as `Option`s)" in {
    Variable(1, 2, 3, 4).to(V4).value shouldBe V4(1, 2, 3, 4)

    Variable(1, 2) to V4 shouldBe empty
    V2(1, 2) to V4 shouldBe empty
    V3(1, 2, 3) to V4 shouldBe empty

    val instance = V4(1, 2, 3, 4)
    instance.to(V4).value should be theSameInstanceAs instance
  }

  it should "be converted to from other versions unsafely (throwing exceptions)" in {
    Variable(1, 2, 3, 4) unsafeTo V4 shouldBe V4(1, 2, 3, 4)

    an[IncompatibleVersionException] should be thrownBy { Variable(1, 2) unsafeTo V4 }
    an[IncompatibleVersionException] should be thrownBy { V2(1, 2) unsafeTo V4 }
    an[IncompatibleVersionException] should be thrownBy { V3(1, 2, 3) unsafeTo V4 }

    val instance = V4(1, 2, 3, 4)
    instance unsafeTo V4 should be theSameInstanceAs instance
  }

  it should "convert from sequences safely (as `Option`s)" in {
    V4.fromSeq(ArraySeq(1, 2, 3, 4)).value shouldBe V4(1, 2, 3, 4)

    V4.fromSeq(ArraySeq()) shouldBe empty
    V4.fromSeq(ArraySeq(1)) shouldBe empty
    V4.fromSeq(ArraySeq(1, 2)) shouldBe empty
    V4.fromSeq(ArraySeq(1, 2, 3)) shouldBe empty
    V4.fromSeq(ArraySeq(1, 2, 3, 4, 5)) shouldBe empty
  }

  it should "convert from sequences unsafely (throwing exceptions)" in {
    V4 unsafeFromSeq ArraySeq(1, 2, 3, 4) shouldBe V4(1, 2, 3, 4)

    an[IllegalArgumentException] should be thrownBy { V4 unsafeFromSeq ArraySeq() }
    an[IncompatibleVersionException] should be thrownBy { V4 unsafeFromSeq ArraySeq(1) }
    an[IncompatibleVersionException] should be thrownBy { V4 unsafeFromSeq ArraySeq(1, 2) }
    an[IncompatibleVersionException] should be thrownBy { V4 unsafeFromSeq ArraySeq(1, 2, 3) }
    an[IncompatibleVersionException] should be thrownBy { V4 unsafeFromSeq ArraySeq(1, 2, 3, 4, 5) }
  }

  it should "parse versions safely (as `Option`s)" in {
    V4.parse("1.2.3.4").value shouldBe V4(1, 2, 3, 4)
    V4.parse("0.1.0.0").value shouldBe V4(0, 1, 0, 0)
    V4.parse("-1.-2.-3.-4").value shouldBe V4(-1, -2, -3, -4)

    V4.parse("") shouldBe empty
    V4.parse(" ") shouldBe empty
    V4.parse("1.2.3") shouldBe empty
    V4.parse("1.2.3.4.") shouldBe empty
    V4.parse(".1.2.3.4") shouldBe empty
    V4.parse("1.2.3.4.5") shouldBe empty
    V4.parse("not a version") shouldBe empty
  }

  it should "parse versions unsafely (throwing exceptions)" in {
    V4 unsafeParse "1.2.3.4" shouldBe V4(1, 2, 3, 4)
    V4 unsafeParse "0.1.0.0" shouldBe V4(0, 1, 0, 0)
    V4 unsafeParse "-1.-2.-3.-4" shouldBe V4(-1, -2, -3, -4)

    a[VersionFormatException] should be thrownBy { V4 unsafeParse "" }
    a[VersionFormatException] should be thrownBy { V4 unsafeParse " " }
    a[VersionFormatException] should be thrownBy { V4 unsafeParse "1.2.3" }
    a[VersionFormatException] should be thrownBy { V4 unsafeParse "1.2.3.4." }
    a[VersionFormatException] should be thrownBy { V4 unsafeParse ".1.2.3.4" }
    a[VersionFormatException] should be thrownBy { V4 unsafeParse "1.2.3.4.5" }
    a[VersionFormatException] should be thrownBy { V4 unsafeParse "not a version" }
  }
}
