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

  /** Identifiers for a pre-release. */
  final class PreRelease private[semver] (_values: IndexedSeq[String]) extends Identifiers(_values) {
    protected type Self = PreRelease

    protected def identifierType: IdentifierType[PreRelease] = IdentifierType.PreRelease

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
     *         valid for this factory's type
     */
    final def from(values: Seq[String]): Option[I] =
      if (values.nonEmpty && values.forall(tpe.isValidIdentifier)) Some(tpe.uncheckedFrom(values))
      else None

    /**
     * @param values the string values of the identifiers
     * @return identifiers of this factory's type from a sequence of strings
     * @throws IllegalArgumentException if the any of the identifiers were not valid
     *                                  for this factory's type
     */
    @throws[IllegalArgumentException]
    final def unsafeFrom(values: Seq[String]): I =
      if (values.nonEmpty && values.forall(tpe.isValidIdentifier)) tpe.uncheckedFrom(values)
      else invalidIdentifiers(values.toString, tpe)

    /**
     * @param values the string values of the identifiers
     * @return identifiers of this factory's type from a sequence of strings
     * @throws IllegalArgumentException if the any of the identifiers were not valid
     *                                  for this factory's type
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
     * @throws IllegalArgumentException if the string did not represent valid
     *                                  identifiers of this factory's type
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
  object PreRelease extends Factory[PreRelease]

  /** Factory for [[Build build identifiers]]. */
  object Build extends Factory[Build]
}
