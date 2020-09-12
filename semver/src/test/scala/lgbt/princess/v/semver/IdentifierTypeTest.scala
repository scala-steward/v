package lgbt.princess.v
package semver

class IdentifierTypeTest extends BaseSpec {
  behavior of "IdentifierType.PreRelease"

  it should "be implicitly available" in {
    implicitly[IdentifierType.PreRelease]
  }

  it should "validate identifiers" in {
    import IdentifierType.PreRelease.isValidIdentifier

    isValidIdentifier("0") shouldBe true
    isValidIdentifier("16") shouldBe true
    isValidIdentifier("alpha") shouldBe true
    isValidIdentifier("abc123") shouldBe true
    isValidIdentifier("-foo2") shouldBe true

    isValidIdentifier("") shouldBe false
    isValidIdentifier("01") shouldBe false
    isValidIdentifier("_a") shouldBe false
  }

  it should "create the correct type of Identifiers" in {
    import IdentifierType.PreRelease.uncheckedFrom

    uncheckedFrom(Vector("foo", "bar")) shouldEqual new Identifiers.PreRelease(Vector("foo", "bar"))
  }

  it should "have the correct name" in {
    IdentifierType.PreRelease.name shouldBe "pre-release"
  }

  behavior of "IdentifierType.Build"

  it should "be implicitly available" in {
    implicitly[IdentifierType.Build]
  }

  it should "validate identifiers" in {
    import IdentifierType.Build.isValidIdentifier

    isValidIdentifier("0") shouldBe true
    isValidIdentifier("16") shouldBe true
    isValidIdentifier("01") shouldBe true
    isValidIdentifier("alpha") shouldBe true
    isValidIdentifier("abc123") shouldBe true
    isValidIdentifier("-foo2") shouldBe true

    isValidIdentifier("") shouldBe false
    isValidIdentifier("_a") shouldBe false
  }

  it should "create the correct type of Identifiers" in {
    import IdentifierType.Build.uncheckedFrom

    uncheckedFrom(Vector("foo", "bar")) shouldEqual new Identifiers.Build(Vector("foo", "bar"))
  }

  it should "have the correct name" in {
    IdentifierType.Build.name shouldBe "build"
  }
}
