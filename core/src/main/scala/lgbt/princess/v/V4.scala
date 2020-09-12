package lgbt.princess.v

import scala.collection.immutable.ArraySeq

/**
 * A version with 4 identifiers.
 *
 * @param major the major version number
 * @param minor the minor version number
 * @param patch the patch number
 * @param build the build number
 */
final case class V4(major: Int, minor: Int, patch: Int, build: Int) extends Version with Ordered[V4] {
  type Self = V4

  /** The first version number; equivalent to [[major]]. */
  @inline def _1: Int = major

  /** The second version number; equivalent to [[minor]]. */
  @inline def _2: Int = minor

  /** The third version number; equivalent to [[patch]]. */
  @inline def _3: Int = patch

  /** The fourth version number; equivalent to [[build]]. */
  @inline def _4: Int = build

  def seq: IndexedSeq[Int] = ArraySeq(major, minor, patch, build)

  override def factory: VersionFactory[V4] = V4

  override def compare(that: V4): Int = V4.ordering.compare(this, that)

  override def equals(that: Any): Boolean =
    that match {
      case that: V4 =>
        this.major == that.major &&
          this.minor == that.minor &&
          this.patch == that.patch &&
          this.build == that.build
      case v: Version if v.productArity == 4 =>
        val s = v.seq
        this.major == s(0) &&
        this.minor == s(1) &&
        this.patch == s(2) &&
        this.build == s(3)
      case _ => false
    }

  override def toString: String = s"$major.$minor.$patch.$build"
}

object V4 extends VersionFactory[V4] with VersionFactory.FixedSize with VersionFactory.UnconstrainedValues {
  implicit val ordering: Ordering[V4] = new Ordering[V4] {
    def compare(x: V4, y: V4): Int = {
      val rMajor = Integer.compare(x.major, y.major)
      if (rMajor != 0) return rMajor
      val rMinor = Integer.compare(x.minor, y.minor)
      if (rMinor != 0) return rMinor
      val rPatch = Integer.compare(x.patch, y.patch)
      if (rPatch != 0) rPatch
      else Integer.compare(x.build, y.build)
    }
  }

  protected[this] def versionTypeDescription: String = "version of size 4"
  protected[this] def arity: Int                     = 4

  protected[this] def uncheckedFromSeq(seq: IndexedSeq[Int]): V4 =
    apply(seq(0), seq(1), seq(2), seq(3))
}
