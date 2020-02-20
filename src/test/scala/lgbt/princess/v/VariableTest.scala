package lgbt.princess.v

import scala.collection.immutable.ArraySeq

class VariableTest extends BaseSpec {
  nameOf[Variable] should "allow arbitrary arity versions" in {
    noException should be thrownBy {
      Variable(1, 2, 3, 4)
      Variable(1)
      Variable(2, 1)
      Variable(0, 1, 0)
      Variable(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
    }
  }

  it should "allow negative values" in {
    noException should be thrownBy Variable(-1, -2, -3)
  }

  it should "forbid an empty seq of values" in {
    an[IllegalArgumentException] should be thrownBy Variable()
  }

  it should "not re-create sequences of values unnecessarily" in {
    val values = Vector(1, 2, 3)
    Variable(values).seq should be theSameInstanceAs values
  }

  it should "be ordered correctly" in {
    Variable(1, 2) should be < Variable(1, 2, 0)
    Variable(1, 2) should be < Variable(1, 3)
    Variable(1, 2) should be < Variable(2, 0)
  }

  it should "convert from other versions safely (as `Option`s)" in {
    Variable.from(V2(1, 2)).value shouldBe Variable(1, 2)
    Variable.from(V3(1, 2, 3)).value shouldBe Variable(1, 2, 3)
    Variable.from(V4(1, 2, 3, 4)).value shouldBe Variable(1, 2, 3, 4)

    val instance = Variable(1, 2, 3, 4, 5)
    Variable.from(instance).value should be theSameInstanceAs instance
  }

  it should "convert from other versions unsafely (throwing exceptions)" in {
    Variable unsafeFrom V2(1, 2) shouldBe Variable(1, 2)
    Variable unsafeFrom V3(1, 2, 3) shouldBe Variable(1, 2, 3)
    Variable unsafeFrom V4(1, 2, 3, 4) shouldBe Variable(1, 2, 3, 4)

    val instance = Variable(1, 2, 3, 4, 5)
    Variable unsafeFrom instance should be theSameInstanceAs instance
  }

  it should "be converted to from other versions safely (as `Option`s)" in {
    V2(1, 2).to(Variable).value shouldBe Variable(1, 2)
    V3(1, 2, 3).to(Variable).value shouldBe Variable(1, 2, 3)
    V4(1, 2, 3, 4).to(Variable).value shouldBe Variable(1, 2, 3, 4)

    val instance = Variable(1, 2, 3, 4, 5)
    instance.to(Variable).value should be theSameInstanceAs instance
  }

  it should "be converted to from other versions unsafely (throwing exceptions)" in {
    V2(1, 2) unsafeTo Variable shouldBe Variable(1, 2)
    V3(1, 2, 3) unsafeTo Variable shouldBe Variable(1, 2, 3)
    V4(1, 2, 3, 4) unsafeTo Variable shouldBe Variable(1, 2, 3, 4)

    val instance = Variable(1, 2, 3, 4, 5)
    instance unsafeTo Variable should be theSameInstanceAs instance
  }

  it should "convert from sequences safely (as `Option`s)" in {
    Variable.fromSeq(ArraySeq(1, 2)).value shouldBe Variable(1, 2)
    Variable.fromSeq(ArraySeq(1, 2, 3)).value shouldBe Variable(1, 2, 3)
    Variable.fromSeq(ArraySeq(1, 2, 3, 4)).value shouldBe Variable(1, 2, 3, 4)

    Variable.fromSeq(ArraySeq()) shouldBe empty
  }

  it should "convert from sequences unsafely (throwing exceptions)" in {
    Variable unsafeFromSeq ArraySeq(1, 2) shouldBe Variable(1, 2)
    Variable unsafeFromSeq ArraySeq(1, 2, 3) shouldBe Variable(1, 2, 3)
    Variable unsafeFromSeq ArraySeq(1, 2, 3, 4) shouldBe Variable(1, 2, 3, 4)

    an[IllegalArgumentException] should be thrownBy { Variable unsafeFromSeq ArraySeq() }
  }

  it should "parse versions safely (as `Option`s)" in {
    Variable.parse("1").value shouldBe Variable(1)
    Variable.parse("0.1.0").value shouldBe Variable(0, 1, 0)
    Variable.parse("-3.2.-4").value shouldBe Variable(-3, 2, -4)

    Variable.parse("") shouldBe empty
    Variable.parse("1.2.") shouldBe empty
    Variable.parse(".1.2") shouldBe empty
    Variable.parse("not a version") shouldBe empty
  }

  it should "parse versions unsafely (throwing exceptions)" in {
    Variable unsafeParse "1" shouldBe Variable(1)
    Variable unsafeParse "0.1.0" shouldBe Variable(0, 1, 0)
    Variable unsafeParse "-3.2.-4" shouldBe Variable(-3, 2, -4)

    a[VersionFormatException] should be thrownBy { Variable unsafeParse "" }
    a[VersionFormatException] should be thrownBy { Variable unsafeParse "1.2." }
    a[VersionFormatException] should be thrownBy { Variable unsafeParse ".1.2" }
    a[VersionFormatException] should be thrownBy { Variable unsafeParse "not a version" }
  }
}
