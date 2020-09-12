package lgbt.princess.v

/**
 * Thrown when attempting to parse an invalid version string.
 *
 * @param badVersion            the invalid version string
 * @param targetTypeDescription a description of the version type being converted
 *                              to; for example, "version of arbitrary size with only
 *                              positive values"
 * @param cause                 another exception (e.g. a `NumberFormatException`)
 *                              causing the version string to be invalid
 */
class VersionFormatException(badVersion: String, targetTypeDescription: String, cause: Throwable)
    extends IllegalArgumentException(
      s"cannot create a $targetTypeDescription from ${VersionFormatException.renderBadVersion(badVersion)}",
      cause
    ) {

  /**
   * @param badVersion            the invalid version string
   * @param targetTypeDescription a description of the version type being converted
   *                              to; for example, "version of arbitrary size with only
   *                              positive values"
   */
  def this(badVersion: String, targetTypeDescription: String) = this(badVersion, targetTypeDescription, null)
}

private object VersionFormatException {
  private def renderBadVersion(badVersion: String): String =
    if (badVersion.isEmpty) "an empty string"
    else if (badVersion.forall(_.isWhitespace)) "a string containing only whitespace"
    else s"string: $badVersion"
}
