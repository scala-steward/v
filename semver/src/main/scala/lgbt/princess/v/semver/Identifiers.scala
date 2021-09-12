package lgbt.princess.v
package semver

import scala.collection.SeqFactory
import scala.collection.SeqFactory.UnapplySeqWrapper
import scala.collection.immutable.ArraySeq

/**
 * Identifiers for a pre-release or for build metadata.
 *
 * @param values the sequence of identifiers
 */
sealed abstract class Identifiers private[semver] (val values: IndexedSeq[String]) {
  protected type Self <: Identifiers

  protected def identifierType: IdentifierType[Self]

  override def hashCode(): Int = identifierType.hashCode() * 43 + values.hashCode()

  override def toString: String = values mkString "."
}

object Identifiers {
  private final val numericRegex = "[0-9]+".r

  /** Identifiers for a pre-release. */
  final class PreRelease private[semver] (_values: IndexedSeq[String])
      extends Identifiers(_values)
      with Ordered[PreRelease] {
    protected type Self = PreRelease

    protected def identifierType: IdentifierType[PreRelease] = IdentifierType.PreRelease

    def compare(that: PreRelease): Int = PreRelease.ordering.compare(this, that)

    override def equals(obj: Any): Boolean =
      obj match {
        case that: PreRelease => this.values == that.values
        case _                => false
      }
  }

  /** Identifiers for a build metadata. */
  final class Build private[semver] (_values: IndexedSeq[String]) extends Identifiers(_values) {
    protected type Self = Build

    protected def identifierType: IdentifierType[Build] = IdentifierType.Build

    override def equals(obj: Any): Boolean =
      obj match {
        case that: Build => this.values == that.values
        case _           => false
      }
  }

  @throws[IllegalArgumentException]
  @inline private def invalidIdentifiers(identifiers: String, tpe: IdentifierType[_]): Nothing =
    throw new IllegalArgumentException(s"invalid series of ${tpe.name} identifiers: '$identifiers'")

  @inline private[this] def splitOnDots(identifiers: String): Array[String] =
    identifiers.split("""\.""", -1)

  @inline private[this] def buildIdentifiers[I <: Identifiers](arr: Array[String])(implicit tpe: IdentifierType[I]): I =
    tpe.uncheckedFrom(ArraySeq.unsafeWrapArray(arr))

  /** A factory for creating identifiers of a given type. */
  sealed abstract class Factory[I <: Identifiers](implicit tpe: IdentifierType[I]) {

    /**
     * Creates identifiers of this factory's type from a sequence of strings.
     *
     * @param values the string values of the identifiers
     * @return an [[scala.Option Option]] from the identifiers if they were
     *         valid for this factory's type, or there are no identifiers
     */
    final def from(values: Seq[String]): Option[I] =
      if (values.nonEmpty && values.forall(tpe.isValidIdentifier)) Some(tpe.uncheckedFrom(values))
      else None

    /**
     * @param values the string values of the identifiers
     * @return identifiers of this factory's type from a sequence of strings
     * @throws scala.IllegalArgumentException if the any of the identifiers were not valid
     *                                        for this factory's type, or there are no identifiers
     */
    @throws[IllegalArgumentException]
    final def unsafeFrom(values: Seq[String]): I =
      if (values.nonEmpty && values.forall(tpe.isValidIdentifier)) tpe.uncheckedFrom(values)
      else invalidIdentifiers(values.toString, tpe)

    /**
     * @param values the string values of the identifiers
     * @return identifiers of this factory's type from a sequence of strings
     * @throws scala.IllegalArgumentException if the any of the identifiers were not valid
     *                                        for this factory's type, or there are no identifiers
     */
    @throws[IllegalArgumentException]
    @inline final def apply(values: String*): I = unsafeFrom(values)

    /**
     * Parses identifiers of this factory's type from a string.
     *
     * @param identifiers the string representing the identifiers
     * @return an [[scala.Option Option]] from the identifiers if they were
     *         valid for this factory's type
     */
    final def parse(identifiers: String): Option[I] = {
      val arr = splitOnDots(identifiers)
      if (arr forall tpe.isValidIdentifier) Some(buildIdentifiers(arr)) else None
    }

    /**
     * @param identifiers the string representing the identifiers
     * @return identifiers of this factory's type from the string
     * @throws scala.IllegalArgumentException if the string did not represent valid
     *                                        identifiers of this factory's type
     */
    @throws[IllegalArgumentException]
    final def unsafeParse(identifiers: String): I = {
      val arr = splitOnDots(identifiers)
      if (arr.forall(tpe.isValidIdentifier)) buildIdentifiers(arr) else invalidIdentifiers(identifiers, tpe)
    }

    /** Extracts the sequence of string identifiers from an instance. */
    final def unapplySeq(identifiers: I): SeqFactory.UnapplySeqWrapper[String] =
      new UnapplySeqWrapper(identifiers.values)
  }

  /** Factory for [[PreRelease pre-release identifiers]]. */
  object PreRelease extends Factory[PreRelease] {
    implicit val ordering: Ordering[PreRelease] = new Ordering[PreRelease] {
      def compare(x: PreRelease, y: PreRelease): Int = {
        val a = x.values.iterator
        val b = y.values.iterator
        while (a.hasNext && b.hasNext) {
          val c        = a.next()
          val d        = b.next()
          val cNumeric = numericRegex.matches(c)
          val dNumeric = numericRegex.matches(d)
          val res1     = java.lang.Boolean.compare(!cNumeric, !dNumeric)
          if (res1 != 0) return res1
          if (cNumeric /* && dNumeric */ ) {
            try {
              val cLong = c.toLong
              val dLong = d.toLong
              val res2  = java.lang.Long.compare(cLong, dLong)
              if (res2 != 0) return res2
            } catch {
              case _: NumberFormatException => // exceeds max size of a `Long`
                val cBig = BigInt(c)
                val dBig = BigInt(d)
                val res2 = cBig compare dBig
                if (res2 != 0) return res2
            }
          } else /* !cNumeric && !dNumeric */
            {
              val res2 = c compareTo d
              if (res2 != 0) return res2
            }
        }
        java.lang.Boolean.compare(a.hasNext, b.hasNext)
      }
    }
  }

  /** Factory for [[Build build identifiers]]. */
  object Build extends Factory[Build]
}
