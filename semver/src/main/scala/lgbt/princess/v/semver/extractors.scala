package lgbt.princess.v.semver

import lgbt.princess.v.semver.Identifiers._

/**
 * This extractor is for extracting the version core and pre-release identifiers
 * of a SemVer version. If you want to extract build identifiers as well,
 * use the [[:- `:-`]] extractor instead. Sadly, there is no way to have both
 * functionalities in the same extractor.
 */
object - {

  /**
   * Extracts the version core and pre-release identifiers from a SemVer
   * version.
   */
  def unapply(sv: SemVer): Option[(Core, PreRelease)] = {
    if (sv.build.isDefined || sv.preRelease.isEmpty) None
    else Some((sv.core, sv.preRelease.get))
  }
}

/**
 * This extractor is for extracting the version core of a SemVer version,
 * and is for use specifically with the [[+ `+`]] extractor (which
 * extracts the pre-release identifiers and build identifiers). If you
 * only want to extract the core and pre-release identifiers, use the
 * [[- `-`]] extractor instead.
 */
object :- {

  /**
   * Extracts the version core from a SemVer version and leaves the
   * pre-release identifiers and build identifiers partially extracted.
   */
  def unapply(sv: SemVer): Option[(Core, (PreRelease, Option[Build]))] =
    sv match {
      case SemVer(core, Some(preRelease), build) => Some((core, (preRelease, build)))
      case _                                     => None
    }
}

/**
 * This extractor is for extracting the version core and build
 * identifiers of a SemVer version, or for extracting the pre-release
 * identifiers and build-identifiers when the version core has already
 * been extracted by the [[:- `:-`]] extractor.
 */
object + {

  /**
   * Extracts the version core and build identifiers from a SemVer
   * version.
   */
  def unapply(sv: SemVer): Option[(Core, Build)] =
    sv match {
      case SemVer(core, None, Some(build)) => Some((core, build))
      case _                               => None
    }

  /**
   * Extracts the pre-release identifiers and build identifiers from
   * a partially extracted SemVer version.
   */
  def unapply(arg: (PreRelease, Option[Build])): Option[(PreRelease, Build)] =
    arg match {
      case (preRelease, Some(build)) => Some((preRelease, build))
      case _                         => None
    }
}
