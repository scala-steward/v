package lgbt.princess.v

import scala.collection.immutable.ArraySeq

/**
 * A version of size 2.
 *
 * @param major the major version number
 * @param minor the minor version number
 */
final case class V2(major: Int, minor: Int) extends Version with Ordered[V2] {
  type Self = V2

  /** The first version number; equivalent to [[major]]. */
  @inline def _1: Int = major

  /** The second version number; equivalent to [[minor]]. */
  @inline def _2: Int = minor

  override def seq: IndexedSeq[Int] = ArraySeq(major, minor)

  override def factory: VersionFactory[V2] = V2

  def compare(that: V2): Int = V2.ordering.compare(this, that)

  override def equals(that: Any): Boolean =
    that match {
      case that: V2 => this.major == that.major && this.minor == that.minor
      case v: Version if v.productArity == 2 =>
        val s = v.seq
        this.major == s(0) && this.minor == s(1)
      case _ => false
    }

  override def toString: String = s"$major.$minor"
}

object V2 extends VersionFactory[V2] with VersionFactory.FixedSize with VersionFactory.UnconstrainedValues {
  implicit val ordering: Ordering[V2] =
    (x: V2, y: V2) => {
      val rMajor = Integer.compare(x.major, y.major)
      if (rMajor != 0) rMajor
      else Integer.compare(x.minor, y.minor)
    }

  protected[this] def versionTypeDescription: String = "version of size 2"
  protected[this] def arity: Int                     = 2

  protected[this] def uncheckedFromSeq(seq: IndexedSeq[Int]): V2 = apply(seq(0), seq(1))
}
