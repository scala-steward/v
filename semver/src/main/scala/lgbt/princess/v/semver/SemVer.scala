package lgbt.princess.v
package semver

import java.lang.{StringBuilder => JStringBuilder}

import lgbt.princess.v.semver.Identifiers._

import scala.collection.mutable.{StringBuilder => SStringBuilder}

/**
 * A SemVer version.
 *
 * This object is [[scala.Ordered `Ordered`]] consistently with the SemVer specification, but not with object equality.
 * If you need to order `SemVer` instances consistently with object equality, use
 * [[SemVer.ObjectEqualityOrdering.ordering]] instead.
 *
 * @param core
 *   the version core
 * @param preRelease
 *   the pre-release identifiers, if any
 * @param build
 *   the build identifiers, if any
 */
final case class SemVer(core: Core, preRelease: Option[PreRelease], build: Option[Build]) extends Ordered[SemVer] {
  import SemVer._

  def compare(that: SemVer): Int = ordering.compare(this, that)

  override def toString: String = {
    val sb = new JStringBuilder()
    sb.append(core)
    appendPrefixed(sb, '-', preRelease)
    appendPrefixed(sb, '+', build)
    sb.toString
  }
}

object SemVer {
  private def optionOrderingEmptyHigher[A](implicit ord: Ordering[A]): Ordering[Option[A]] = {
    case (Some(a), Some(b)) => ord.compare(a, b)
    case (a, b)             => java.lang.Boolean.compare(a.isEmpty, b.isEmpty)
  }

  /**
   * The default [[scala.Ordering `Ordering`]] for SemVer versions. This ordering is consistent with the SemVer
   * specification, but not with object equality. If you need an ordering consistent with object equality, use
   * [[ObjectEqualityOrdering.ordering]].
   */
  implicit val ordering: Ordering[SemVer] =
    Ordering
      .by[SemVer, Core](_.core)
      .orElseBy(_.preRelease)(optionOrderingEmptyHigher)

  object ObjectEqualityOrdering {

    /**
     * A secondary [[scala.Ordering `Ordering`]] for SemVer versions. This `Ordering` is consistent with object
     * equality, but not with the SemVer specification. If you need an ordering consistent with the SemVer
     * specification, use [[SemVer.ordering]] (which is the default).
     *
     * For `SemVer` instances with different precedence according to the SemVer specification, this `Ordering` behaves
     * identically to [[SemVer.ordering]]. For `SemVer` instances with the same precedence according to the SemVer
     * specification, this `Ordering` is only specified insofar as that two instances that are `equal` yield zero when
     * compared, and two instances that are not `equal` yield a non-zero value when compared. The result of comparison
     * is not otherwise specified, and may change between runtime environment executions or between arbitrary library
     * releases.
     */
    implicit val ordering: Ordering[SemVer] = {
      import Ordering.Implicits._

      SemVer.ordering
        .orElseBy(_.build)(optionOrderingEmptyHigher(Ordering.by(_.values)))
    }
  }

  final class StringExtractor private[SemVer] {
    def unapply(version: String): Option[((Int, Int, Int), Option[IndexedSeq[String]], Option[IndexedSeq[String]])] =
      parse(version).map { sv =>
        val core = sv.core
        ((core.major, core.minor, core.patch), sv.preRelease.map(_.values), sv.build.map(_.values))
      }
  }

  /**
   * An extractor for valid SemVer strings.
   *
   * @example
   *   {{{
   * "1.2.3-alpha+abc123" match {
   *   case SemVer.string((1, 2, 3), None, None) => // does not match this
   *   case SemVer.string((1, 2, 3), Some(_), _) => // matches this
   * }
   *   }}}
   */
  val string: StringExtractor = new StringExtractor

  private def appendPrefixed(sb: JStringBuilder, prefix: Char, identifiers: Option[Identifiers]): Unit = {
    if (identifiers.isDefined) {
      sb.append(prefix)
      identifiers.get.values.addString(new SStringBuilder(sb), ".")
    }
  }

  /** @return a release SemVer version with no build identifiers. */
  def apply(core: Core): SemVer = apply(core, None, None)

  /**
   * @return
   *   a pre-release SemVer version with the given pre-release identifiers and no build identifiers.
   */
  def apply(core: Core, preRelease: PreRelease): SemVer = apply(core, Some(preRelease), None)

  /** @return a release SemVer version with the given build identifiers. */
  def apply(core: Core, build: Build): SemVer = apply(core, None, Some(build))

  /**
   * @return
   *   a pre-release SemVer version with the given pre-release and build identifiers.
   */
  def apply(core: Core, preRelease: PreRelease, build: Build): SemVer =
    apply(core, Some(preRelease), Some(build))

  private[this] def splitVersion(version: String): (String, Option[String], Option[String]) = {
    val plusSplit = version.split("""\+""", 2)
    val dashSplit = plusSplit(0).split("-", 2)

    val core       = dashSplit(0)
    val preRelease = if (dashSplit.length == 2) Some(dashSplit(1)) else None
    val build      = if (plusSplit.length == 2) Some(plusSplit(1)) else None

    (core, preRelease, build)
  }

  /**
   * Parses a string representation of a SemVer version.
   *
   * @param version
   *   the string representation of a version
   * @return
   *   an [[scala.Option Option]] containing the SemVer version represented by the string, if it represented a valid
   *   SemVer version
   */
  def parse(version: String): Option[SemVer] = {
    def parseIdentifiers[I <: Identifiers](factory: Factory[I])(identifiers: Option[String]): Option[Option[I]] =
      identifiers match {
        case None      => Some(None)
        case Some(str) => factory.parse(str).map(Some(_))
      }

    val (coreStr, preReleaseStr, buildStr) = splitVersion(version)

    for {
      core       <- Core parse coreStr
      preRelease <- parseIdentifiers(PreRelease)(preReleaseStr)
      build      <- parseIdentifiers(Build)(buildStr)
    } yield apply(core, preRelease, build)
  }

  /**
   * Parses a string representation of a SemVer version.
   *
   * @param version
   *   the string representation of a version
   * @return
   *   the SemVer version represented by the string
   * @throws VersionFormatException
   *   if the string did not represent a valid SemVer version
   */
  @throws[VersionFormatException]
  def unsafeParse(version: String): SemVer = {
    val (core, preRelease, build) = splitVersion(version)

    try {
      apply(
        Core unsafeParse core,
        preRelease map PreRelease.unsafeParse,
        build map Build.unsafeParse,
      )
    } catch {
      case e: IllegalArgumentException => throw new VersionFormatException(version, "SemVer version", e)
    }
  }
}
