package lgbt.princess.v
package semver

import lgbt.princess.v.semver.Identifiers._

class IdentifiersTest extends BaseSpec {
  behavior of "Identifiers.PreRelease"

  it should "construct identifiers as an Option from a Seq" in {
    PreRelease
      .from(Vector("0", "16", "alpha", "abc123", "-foo2"))
      .value
      .values shouldEqual Seq("0", "16", "alpha", "abc123", "-foo2")

    PreRelease.from(Vector("", "foo")) shouldBe empty
    PreRelease.from(Vector("foo", "01")) shouldBe empty
    PreRelease.from(Vector("_a")) shouldBe empty
  }

  it should "construct identifiers unsafely from a Seq" in {
    val expected = Seq("0", "16", "alpha", "abc123", "-foo2")
    PreRelease.unsafeFrom(Vector("0", "16", "alpha", "abc123", "-foo2")).values shouldEqual expected

    an[IllegalArgumentException] should be thrownBy PreRelease.unsafeFrom(Vector("", "foo"))
    an[IllegalArgumentException] should be thrownBy PreRelease.unsafeFrom(Vector("foo", "01"))
    an[IllegalArgumentException] should be thrownBy PreRelease.unsafeFrom(Vector("_a"))
  }

  it should "construct identifiers with varargs" in {
    PreRelease("0", "16", "alpha", "abc123", "-foo2").values shouldEqual Seq("0", "16", "alpha", "abc123", "-foo2")

    an[IllegalArgumentException] should be thrownBy PreRelease("", "foo")
    an[IllegalArgumentException] should be thrownBy PreRelease("foo", "01")
    an[IllegalArgumentException] should be thrownBy PreRelease("_a")
  }

  it should "parse identifiers as an Option" in {
    PreRelease.parse("0.16.alpha.abc123.-foo2").value.values shouldEqual Seq("0", "16", "alpha", "abc123", "-foo2")

    PreRelease.parse(".foo") shouldBe empty
    PreRelease.parse("foo.01") shouldBe empty
    PreRelease.parse("_a") shouldBe empty
  }

  it should "parse identifiers unsafely" in {
    PreRelease.unsafeParse("0.16.alpha.abc123.-foo2").values shouldEqual Seq("0", "16", "alpha", "abc123", "-foo2")

    an[IllegalArgumentException] should be thrownBy PreRelease.unsafeParse(".foo")
    an[IllegalArgumentException] should be thrownBy PreRelease.unsafeParse("foo.01")
    an[IllegalArgumentException] should be thrownBy PreRelease.unsafeParse("_a")
  }

  it should "extract identifiers correctly" in {
    PreRelease("alpha") shouldMatch { case PreRelease("alpha") => }
    PreRelease("alpha") shouldNotMatch { case PreRelease("beta") => }
  }

  it should "have a sensible string representation" in {
    PreRelease("0", "16", "alpha", "abc123", "-foo2").toString shouldBe "0.16.alpha.abc123.-foo2"
  }

  it should "compare equality correctly" in {
    PreRelease("foo") shouldEqual PreRelease("foo")
    PreRelease("foo", "bar") shouldEqual PreRelease("foo", "bar")
    PreRelease("foo") should not equal PreRelease("bar")
    PreRelease("foo", "bar") should not equal PreRelease("bar", "foo")
    PreRelease("foo") should not equal Build("foo")
    PreRelease("foo") should not equal new AnyRef
  }

  it should "produce consistent hash codes" in {
    def check(a: PreRelease, b: PreRelease): Unit =
      a.hashCode() shouldEqual b.hashCode()

    check(PreRelease("foo"), PreRelease("foo"))
    check(PreRelease("foo", "bar", "baz"), PreRelease("foo", "bar", "baz"))
  }

  behavior of "Identifiers.Build"

  it should "construct identifiers as an Option from a Seq" in {
    Build
      .from(Vector("0", "16", "01", "alpha", "abc123", "-foo2"))
      .value
      .values shouldEqual Seq("0", "16", "01", "alpha", "abc123", "-foo2")

    Build.from(Vector("", "foo")) shouldBe empty
    Build.from(Vector("_a")) shouldBe empty
  }

  it should "construct identifiers unsafely from a Seq" in {
    Build
      .unsafeFrom(Vector("0", "16", "01", "alpha", "abc123", "-foo2"))
      .values should contain theSameElementsInOrderAs Seq("0", "16", "01", "alpha", "abc123", "-foo2")

    an[IllegalArgumentException] should be thrownBy Build.unsafeFrom(Vector("", "foo"))
    an[IllegalArgumentException] should be thrownBy Build.unsafeFrom(Vector("_a"))
  }

  it should "construct build identifiers with varargs" in {
    val expected = Seq("0", "16", "01", "alpha", "abc123", "-foo2")
    Build("0", "16", "01", "alpha", "abc123", "-foo2").values shouldEqual expected

    an[IllegalArgumentException] should be thrownBy Build("", "foo")
    an[IllegalArgumentException] should be thrownBy Build("_a")
  }

  it should "parse identifiers as an Option" in {
    Build.parse("0.16.01.alpha.abc123.-foo2").value.values shouldEqual Seq("0", "16", "01", "alpha", "abc123", "-foo2")

    Build.parse(".foo") shouldBe empty
    Build.parse("_a") shouldBe empty
  }

  it should "parse identifiers unsafely" in {
    Build.unsafeParse("0.16.01.alpha.abc123.-foo2").values shouldEqual Seq("0", "16", "01", "alpha", "abc123", "-foo2")

    an[IllegalArgumentException] should be thrownBy Build.unsafeParse(".foo")
    an[IllegalArgumentException] should be thrownBy Build.unsafeParse("_a")
  }

  it should "extract identifiers correctly" in {
    Build("1") shouldMatch { case Build("1") => }
    Build("1") shouldNotMatch { case Build("2") => }
  }

  it should "have a sensible string representation" in {
    Build("0", "16", "01", "alpha", "abc123", "-foo2").toString shouldBe "0.16.01.alpha.abc123.-foo2"
  }

  it should "compare equality correctly" in {
    Build("foo") shouldEqual Build("foo")
    Build("foo", "bar") shouldEqual Build("foo", "bar")
    Build("foo") should not equal Build("bar")
    Build("foo", "bar") should not equal Build("bar", "foo")
    Build("foo") should not equal PreRelease("foo")
    Build("foo") should not equal new AnyRef
  }

  it should "produce consistent hash codes" in {
    def check(a: Build, b: Build): Unit =
      a.hashCode() shouldEqual b.hashCode()

    check(Build("foo"), Build("foo"))
    check(Build("foo", "bar", "baz"), Build("foo", "bar", "baz"))
  }
}
