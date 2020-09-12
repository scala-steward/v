package lgbt.princess.v
package semver

import lgbt.princess.v.semver.Identifiers.{Build => B, PreRelease => PR}
import lgbt.princess.v.semver.StringsAsUnsafeIdentifiers._

class StringsAsUnsafeIdentifiersTest extends BaseSpec {

  behavior of "StringsAsUnsafeIdentifiers"

  it should "implicitly convert strings to pre-release identifiers" in {
    @inline def coerce(identifiers: PR): PR = identifiers

    coerce("0.16.alpha.abc123.-foo2") shouldEqual PR("0", "16", "alpha", "abc123", "-foo2")

    an[IllegalArgumentException] should be thrownBy coerce(".foo")
    an[IllegalArgumentException] should be thrownBy coerce("foo.01")
    an[IllegalArgumentException] should be thrownBy coerce("_a")
  }

  it should "implicitly convert strings to build identifiers" in {
    @inline def coerce(identifiers: B): B = identifiers

    coerce("0.16.01.alpha.abc123.-foo2") shouldEqual B("0", "16", "01", "alpha", "abc123", "-foo2")

    an[IllegalArgumentException] should be thrownBy coerce(".foo")
    an[IllegalArgumentException] should be thrownBy coerce("_a")
  }

  it should "allow construction of SemVer versions from strings using symbolic and extension methods" in {
    (Core(1, 2, 3) - "alpha": SemVer) shouldEqual SemVer(Core(1, 2, 3), PR("alpha"))
    (Core(1, 2, 3) - "alpha").toSemVer shouldEqual SemVer(Core(1, 2, 3), PR("alpha"))
    (Core(1, 2, 3) - "alpha").withoutMetadata shouldEqual SemVer(Core(1, 2, 3), PR("alpha"))
    Core(1, 2, 3) + "12" shouldEqual SemVer(Core(1, 2, 3), B("12"))
    Core(1, 2, 3) - "alpha" + "12" shouldEqual SemVer(Core(1, 2, 3), PR("alpha"), B("12"))
  }
}
