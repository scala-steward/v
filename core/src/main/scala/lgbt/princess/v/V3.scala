package lgbt.princess.v

import scala.collection.immutable.ArraySeq

/**
 * A version with 3 identifiers.
 *
 * @param major
 *   the major version number
 * @param minor
 *   the minor version number
 * @param patch
 *   the patch number
 */
final case class V3(major: Int, minor: Int, patch: Int) extends Version with Ordered[V3] {
  type Self = V3

  /** The first version number; equivalent to [[major]]. */
  @inline def _1: Int = major

  /** The second version number; equivalent to [[minor]]. */
  @inline def _2: Int = minor

  /** The third version number; equivalent to [[patch]]. */
  @inline def _3: Int = patch

  override def seq: IndexedSeq[Int] = ArraySeq(major, minor, patch)

  override def factory: VersionFactory[V3] = V3

  override def compare(that: V3): Int = V3.ordering.compare(this, that)

  override def equals(that: Any): Boolean =
    that match {
      case that: V3 =>
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

object V3 extends VersionFactory[V3] with VersionFactory.FixedSize with VersionFactory.UnconstrainedValues {
  implicit val ordering: Ordering[V3] = new Ordering[V3] {
    def compare(x: V3, y: V3): Int = {
      val rMajor = Integer.compare(x.major, y.major)
      if (rMajor != 0) return rMajor
      val rMinor = Integer.compare(x.minor, y.minor)
      if (rMinor != 0) rMinor
      else Integer.compare(x.patch, y.patch)
    }
  }

  /** An extractor for valid `V3` strings. */
  final class StringExtractor private[V3] {
    def unapply(version: String): Option[(Int, Int, Int)] =
      parse(version).map(v => (v.major, v.minor, v.patch))
  }

  /**
   * An extractor for valid `V3` strings.
   *
   * @example
   *   {{{
   * "1.2.3" match {
   *   case V3.string(1, 2, 5) => // does not match this
   *   case V3.string(1, 2, _) => // matches this
   * }
   *   }}}
   */
  val string: StringExtractor = new StringExtractor

  protected[this] def versionTypeDescription: String             = "version of size 3"
  protected[this] def arity: Int                                 = 3
  protected[this] def uncheckedFromSeq(seq: IndexedSeq[Int]): V3 = apply(seq(0), seq(1), seq(2))
}
