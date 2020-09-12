package lgbt.princess.v

/** A version consisting of some number of numeric identifiers. */
trait Version extends Product {
  type Self <: Version

  /** The sequence of numeric identifiers comprising this version. */
  def seq: IndexedSeq[Int]

  /** The factory for creating versions of this type. */
  def factory: VersionFactory[Self]

  /**
   * Converts this version to another type of version.
   *
   * Equivalent to `fact from this`.
   *
   * @param fact a factory for creating the other type of version
   * @tparam V the type of the other type of version
   * @return an [[scala.Option Option]] containing an equivalent version of
   *         the other type, if this version is compatible with the other type
   */
  @inline final def to[V <: Version](fact: VersionFactory[V]): Option[V] = fact from this

  /**
   * Converts this version to another type of version.
   *
   * Equivalent to `fact unsafeFrom this`.
   *
   * @param fact a factory for creating the other type of version
   * @tparam V the type of the other type of version
   * @throws IncompatibleVersionException if this version is not compatible
   *                                      with the other type
   * @return an equivalent version of the other type
   */
  @throws[IncompatibleVersionException]
  @inline final def unsafeTo[V <: Version](fact: VersionFactory[V]): V = fact unsafeFrom this

  override def canEqual(that: Any): Boolean = that.isInstanceOf[Version]

  override def equals(that: Any): Boolean =
    that match {
      case that: Version => (that canEqual this) && this.seq == that.seq
      case _             => false
    }

  override final def hashCode: Int = seq.hashCode() * Version.hashSeed

  override def toString: String = seq mkString "."
}

private object Version {
  private final val hashSeed = "Version".hashCode

  type Aux[V] = Version { type Self = V }
}
