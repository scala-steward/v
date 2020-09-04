package lgbt.princess.v

import scala.collection.immutable.ArraySeq

/** A factory for creating versions of a particular type. */
trait VersionFactory[+V <: Version] {
  import VersionFactory._

  // TODO: improve how this is managed, particularly for constrained versions
  /** The name of the type of version produced by this factory. */
  protected def versionTypeDescription: String

  /**
   * The maximum arity of versions produced by this factory.
   *
   * @return a positive integer representing the maximum number of values
   *         in the [[Version.seq sequences]] of versions produced
   *         by this factory, or `-1` if this factory can produce versions
   *         with sequences of arbitrarily large lengths
   */
  protected[this] def maxArity: Int

  /**
   * Whether or not `thatArity` is a valid arity from which to
   * create a version.
   *
   * `thatArity` will always be positive.
   *
   * @param thatArity the arity of the sequence or other version
   *                  from which to create a version
   * @return whether or not the arity is valid for a version created
   *         by this factory
   */
  protected[this] def isValidArity(thatArity: Int): Boolean

  /**
   * Whether or not `seq` is a valid sequence of values from
   * which to create a version.
   *
   * This method will only be called after `isValidArity` has
   * returned `true`.
   *
   * @param seq the sequence from which to create a version
   * @return whether or not `seq` is a valid sequence of values
   *         for a version created by this factory
   */
  protected[this] def isValidSeq(seq: IndexedSeq[Int]): Boolean

  /**
   * Create a version from a sequence, assuming the sequence
   * to have already been checked to [[isValidArity have a valid arity]]
   * and [[isValidSeq have valid values]].
   *
   * @param seq the sequence from which to create a version
   * @return the version of this factory's type equivalent to the sequence
   */
  protected[this] def uncheckedFromSeq(seq: IndexedSeq[Int]): V

  /**
   * Creates this factory's type of version from another version.
   *
   * @param other the other version
   * @return an [[scala.Option Option]] containing a version of this factory's
   *         type equivalent to the other version, if the other version is
   *         compatible with this factory's type
   */
  final def from(other: Version): Option[V] =
    if (other.factory == this) Some(other.asInstanceOf[V])
    else if (isValidArity(other.productArity)) {
      val seq = other.seq
      Option.when(isValidSeq(seq))(uncheckedFromSeq(seq))
    } else None

  /**
   * Creates this factory's type of version from another version.
   *
   * @param other the other version
   * @throws IncompatibleVersionException if the other version is not compatible
   *                                      with this factory's type
   * @return a version of this factory's type equivalent to the other version
   */
  @throws[IncompatibleVersionException]
  final def unsafeFrom(other: Version): V = {
    @inline def fail(): Nothing =
      throw new IncompatibleVersionException(other)(versionTypeDescription)
    if (other.factory == this) other.asInstanceOf[V]
    else if (isValidArity(other.productArity)) {
      val seq = other.seq
      if (isValidSeq(seq)) uncheckedFromSeq(other.seq) else fail()
    } else fail()
  }

  /**
   * Creates this factory's type of version from a sequence of values.
   *
   * @param seq the sequence of values
   * @return an [[scala.Option Option]] containing a version of this factory's
   *         type equivalent to the sequence of values, if the sequence is
   *         compatible with this factory's type
   */
  final def fromSeq(seq: IndexedSeq[Int]): Option[V] =
    Option.when(seq.nonEmpty && isValidArity(seq.length) && isValidSeq(seq)) {
      uncheckedFromSeq(seq)
    }

  /**
   * Creates this factory's type of version from a sequence of values.
   *
   * @param seq the sequence of values
   * @throws scala.IllegalArgumentException if the sequence of values is not compatible
   *                                        with this factory's type
   * @return a version of this factory's type equivalent to the sequence of values
   */
  @throws[IllegalArgumentException]
  final def unsafeFromSeq(seq: IndexedSeq[Int]): V = {
    require(seq.nonEmpty, "version sequence cannot be empty")
    if (isValidArity(seq.length) && isValidSeq(seq)) uncheckedFromSeq(seq)
    else throw new IncompatibleVersionException(seq)(versionTypeDescription)
  }

  private[this] def parseInts(strings: Array[String]): Option[V] = {
    val len  = strings.length
    val ints = new Array[Int](len)

    var ok = true
    var i  = 0
    while (ok && i < len) {
      strings(i).toIntOption match {
        case Some(int) => ints(i) = int
        case None      => ok = false
      }
      i += 1
    }

    if (ok) {
      val seq = ArraySeq.unsafeWrapArray(ints)
      Option.when(isValidSeq(seq))(uncheckedFromSeq(seq))
    } else None
  }

  /**
   * Creates this factory's type of version from a string representation of a
   * version.
   *
   * @param version the string representation of a version
   * @return an [[scala.Option Option]] containing a version of this factory's
   *         type equivalent to the string representation of a version, if the
   *         string is compatible with this factory's type
   */
  final def parse(version: String): Option[V] =
    if (version.endsWith(".")) None
    else {
      val max = maxArity
      val strings =
        if (max <= 0) dotRegex.split(version)
        else dotRegex.pattern.split(version, max + 1)
      if (isValidArity(strings.length)) parseInts(strings) else None
    }

  /**
   * Creates this factory's type of version from a string representation of a
   * version.
   *
   * @param version the string representation of a version
   * @throws VersionFormatException if the string is not compatible
   *                                with this factory's type
   * @return a version of this factory's type equivalent to the string
   *         representation of a version
   */
  @throws[VersionFormatException]
  final def unsafeParse(version: String): V = {
    @inline def fail(): Nothing =
      throw new VersionFormatException(badVersion = version, targetTypeDescription = versionTypeDescription)
    if (version.endsWith(".")) fail()
    val max = this.maxArity
    val strings =
      if (max <= 0) dotRegex.split(version)
      else dotRegex.pattern.split(version, max + 1)
    if (isValidArity(strings.length)) {
      val ints =
        try strings map { _.toInt } catch {
          case e: NumberFormatException =>
            throw new VersionFormatException(
              badVersion = version,
              targetTypeDescription = versionTypeDescription,
              cause = e
            )
        }
      val seq = ArraySeq.unsafeWrapArray(ints)
      if (isValidSeq(seq)) uncheckedFromSeq(seq) else fail()
    } else fail()
  }

  /**
   * Extracts a version from a string.
   *
   * @param version the string representation of a version
   * @return an [[scala.Option Option]] containing a version,
   *         if it is valid for this factory
   */
  final def unapply(version: String): Option[V] = parse(version)
}

object VersionFactory {
  private val dotRegex = """\.""".r

  /**
   * A mixin for [[VersionFactory]]s of versions with fixed sizes
   * (e.g. always exactly size 4).
   */
  trait FixedSize { self: VersionFactory[Version] =>

    /**
     * The arity of versions produced by this factory.
     *
     * @return a positive integer representing the maximum number of values
     *         in the [[Version.seq sequences]] of versions produced
     *         by this factory
     */
    protected[this] def arity: Int

    protected[this] final def maxArity: Int                         = arity
    protected[this] final def isValidArity(thatArity: Int): Boolean = thatArity == arity
  }

  /**
   * A mixin for [[VersionFactory]]s of versions whose sequence can contain any values
   * (rather than, say, only non-negative values).
   */
  trait UnconstrainedValues { self: VersionFactory[Version] =>
    override protected[this] final def isValidSeq(seq: IndexedSeq[Int]): Boolean = true
  }
}
