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

  it should "have the correct productPrefix" in {
    Variable(1, 2, 3).productPrefix shouldBe "Variable"
  }

  it should "have the correct productArity" in {
    Variable(1, 2).productArity shouldBe 2
    Variable(1, 2, 3).productArity shouldBe 3
    Variable(1, 2, 3, 4).productArity shouldBe 4
  }

  it should "have the correct productElement" in {
    val v = Variable(1, 2, 3)
    (0 until v.productArity).map(v.productElement).toList shouldEqual v.seq
  }

  it should "have the correct productIterator" in {
    val v = Variable(1, 2, 3)
    v.productIterator.toList shouldEqual v.seq
  }

  it should "compare equality correctly" in {
    Variable(1, 2, 3) shouldEqual Variable(1, 2, 3)
    Variable(1, 2) shouldEqual V2(1, 2)
    Variable(1, 2, 3) shouldEqual V3(1, 2, 3)
    Variable(1, 2, 3, 4) shouldEqual V4(1, 2, 3, 4)

    Variable(1, 2, 3) should not equal Variable(1, 2, 4)
    Variable(1, 2, 3) should not equal Variable(1, 3, 3)
    Variable(1, 2, 3) should not equal Variable(2, 2, 3)
    Variable(1, 2, 3) should not equal Variable(1, 2)
    Variable(1, 2, 3) should not equal Variable(1, 2, 3, 4)
    Variable(1, 2, 3) should not equal V3(1, 2, 4)
    Variable(1, 2, 3) should not equal V3(1, 3, 3)
    Variable(1, 2, 3) should not equal V3(2, 2, 3)
    Variable(1, 2, 3, 4) should not equal V3(1, 2, 3)
    Variable(1, 2, 3) should not equal new AnyRef
  }

  it should "have a consistent hash code" in {
    Variable(1, 2, 3).hashCode shouldEqual Variable(1, 2, 3).hashCode
    Variable(1, 2, 3).hashCode shouldEqual V3(1, 2, 3).hashCode
  }

  it should "be ordered correctly" in {
    Variable(1, 2) should be < Variable(1, 2, 0)
    Variable(1, 2) should be < Variable(1, 3)
    Variable(1, 2) should be < Variable(2, 0)

    Variable(1, 2) < Variable(1, 2, 0) shouldBe true
    Variable(1, 2) < Variable(1, 3) shouldBe true
    Variable(1, 2) < Variable(2, 0) shouldBe true
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
    Variable.parse(" ") shouldBe empty
    Variable.parse("1.2.") shouldBe empty
    Variable.parse(".1.2") shouldBe empty
    Variable.parse("not a version") shouldBe empty
  }

  it should "parse versions unsafely (throwing exceptions)" in {
    Variable unsafeParse "1" shouldBe Variable(1)
    Variable unsafeParse "0.1.0" shouldBe Variable(0, 1, 0)
    Variable unsafeParse "-3.2.-4" shouldBe Variable(-3, 2, -4)

    a[VersionFormatException] should be thrownBy { Variable unsafeParse "" }
    a[VersionFormatException] should be thrownBy { Variable unsafeParse " " }
    a[VersionFormatException] should be thrownBy { Variable unsafeParse "1.2." }
    a[VersionFormatException] should be thrownBy { Variable unsafeParse ".1.2" }
    a[VersionFormatException] should be thrownBy { Variable unsafeParse "not a version" }
  }

  it should "extract from instances of itself" in {
    Variable(1, 2) shouldMatch { case Variable(1, 2) => }
    Variable(1, 2, 3) shouldMatch { case Variable(1, 2, 3) => }
    Variable(1, 2, 3, 4) shouldMatch { case Variable(1, 2, 3, 4) => }

    Variable(1, 2) shouldNotMatch { case Variable(_, _, _) => }
    Variable(1, 2, 3, 4) shouldNotMatch { case Variable(_, _, _) => }
  }
}
