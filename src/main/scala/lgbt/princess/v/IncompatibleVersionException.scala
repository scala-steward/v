package lgbt.princess.v

/** Thrown when attempting to convert to an incompatible version type. */
class IncompatibleVersionException private (version: String)(targetTypeDescription: String)
    extends IllegalArgumentException(s"cannot create a $targetTypeDescription from version: $version") {

  /**
   * @param v                     the version or sequence of values being converted from
   * @param targetTypeDescription a description of the version type being converted
   *                              to; for example, "version of arbitrary size with only
   *                              positive values"
   */
  def this(v: Version)(targetTypeDescription: String) = this(v.toString)(targetTypeDescription)

  private[v] def this(seq: IndexedSeq[Int])(targetTypeDescription: String) =
    this(seq mkString ".")(targetTypeDescription)
}
