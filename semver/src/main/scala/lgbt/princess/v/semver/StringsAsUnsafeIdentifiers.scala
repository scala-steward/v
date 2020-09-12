package lgbt.princess.v.semver

import lgbt.princess.v.semver.Identifiers._

import scala.language.implicitConversions

/** Implicits for treating strings (generally literals) as [[Identifiers]]. */
object StringsAsUnsafeIdentifiers {
  implicit def asPreRelease(s: String): PreRelease = PreRelease.unsafeParse(s)
  implicit def asBuild(s: String): Build           = Build.unsafeParse(s)
}
