package lgbt.princess.v
package semver

import lgbt.princess.v.semver.Identifiers.{Build, PreRelease}

import scala.collection.immutable.ArraySeq

/**
 * A SemVer version core.
 *
 * @param major the major version number
 * @param minor the minor version number
 * @param patch the patch number
 */
final case class Core(major: Int, minor: Int, patch: Int) extends Version with Ordered[Core] {
  require(major >= 0 && minor >= 0 && patch >= 0, "SemVer core identifiers must be non-negative")

  type Self = Core

  /**
   * @return a pre-release SemVer version with the given identifiers
   *         to which build identifiers can be added using the
   *         [[SemVerPreReleaseIntermediate.+ `+`]] operator.
   */
  def -(preRelease: PreRelease): SemVerPreReleaseIntermediate =
    new SemVerPreReleaseIntermediate(SemVer(this, preRelease))

  /** @return a release SemVer version with the given build identifiers. */
  def +(build: Build): SemVer = SemVer(this, build)

  /** @return a release SemVer version with no build identifiers. */
  def toSemVer: SemVer = SemVer(this)

  def seq: IndexedSeq[Int] = ArraySeq(major, minor, patch)

  def factory: VersionFactory[Core] = Core

  override def compare(that: Core): Int = Core.ordering.compare(this, that)

  override def equals(that: Any): Boolean =
    that match {
      case that: Core =>
        this.major == that.major &&
          this.minor == that.minor &&
          this.patch == that.patch
      case v: Version if v.productArity == 3 =>
        val s = v.seq
        this.major == s(0) &&
        this.minor == s(1) &&
        this.patch == s(2)
      case _ => false
    }

  override def toString: String = s"$major.$minor.$patch"
}

object Core extends VersionFactory[Core] with VersionFactory.FixedSize {
  implicit val ordering: Ordering[Core] = new Ordering[Core] {
    def compare(x: Core, y: Core): Int = {
      val rMajor = Integer.compare(x.major, y.major)
      if (rMajor != 0) return rMajor
      val rMinor = Integer.compare(x.minor, y.minor)
      if (rMinor != 0) rMinor
      else Integer.compare(x.patch, y.patch)
    }
  }

  protected def versionTypeDescription: String                     = "SemVer version core"
  protected[this] def arity: Int                                   = 3
  protected[this] def isValidSeq(seq: IndexedSeq[Int]): Boolean    = seq.forall(_ >= 0)
  protected[this] def uncheckedFromSeq(seq: IndexedSeq[Int]): Core = apply(seq(0), seq(1), seq(2))

}
