package lgbt.princess.v

final class Variable private (val seq: IndexedSeq[Int]) extends Version with Ordered[Variable] {
  type Self = Variable

  override def factory: VersionFactory[Variable] = Variable

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

  def apply(seq: IndexedSeq[Int]): Variable = unsafeFromSeq(seq)
  def apply(values: Int*): Variable         = apply(values to IndexedSeq)

  protected def versionTypeDescription: String              = "version of arbitrary size"
  protected[this] def maxArity: Int                         = -1
  protected[this] def isValidArity(thatArity: Int): Boolean = true

  protected[this] def uncheckedFromSeq(seq: IndexedSeq[Int]): Variable = new Variable(seq)
}
