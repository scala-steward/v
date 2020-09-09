package lgbt.princess.v

import scala.collection.SeqFactory

/** A version with an arbitrary (but positive) number of identifiers. */
final class Variable private (val seq: IndexedSeq[Int]) extends Version with Ordered[Variable] {
  type Self = Variable

  override def factory: VersionFactory[Variable] = Variable

  override def productPrefix: String          = "Variable"
  override def productArity: Int              = seq.length
  override def productElement(n: Int): Any    = seq(n)
  override def productIterator: Iterator[Any] = seq.iterator

  def compare(that: Variable): Int = Variable.ordering.compare(this, that)
}

object Variable extends VersionFactory[Variable] with VersionFactory.UnconstrainedValues {
  implicit val ordering: Ordering[Variable] = {
    import scala.math.Ordering.Implicits._
    Ordering.by(_.seq)
  }

  /**
   * Creates a [[Variable variable-sized version]] with the specified
   * [[Variable.seq sequence]] of values.
   *
   * @throws IllegalArgumentException if the sequence of values is empty
   */
  @throws[IllegalArgumentException]
  def apply(seq: IndexedSeq[Int]): Variable = unsafeFromSeq(seq)

  /**
   * Creates a [[Variable variable-sized version]] with the specified
   * values as its [[Variable.seq sequence]].
   *
   * @throws IllegalArgumentException if no values are given
   */
  @throws[IllegalArgumentException]
  def apply(values: Int*): Variable = apply(values.toIndexedSeq)

  /** Extracts the sequence of identifiers from a [[Variable]]. */
  def unapplySeq(variable: Variable): SeqFactory.UnapplySeqWrapper[Int] =
    new SeqFactory.UnapplySeqWrapper(variable.seq)

  protected def versionTypeDescription: String              = "version of arbitrary size"
  protected[this] def maxArity: Int                         = -1
  protected[this] def isValidArity(thatArity: Int): Boolean = thatArity > 0

  protected[this] def uncheckedFromSeq(seq: IndexedSeq[Int]): Variable = new Variable(seq)
}
