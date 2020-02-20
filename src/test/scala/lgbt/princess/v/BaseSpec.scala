package lgbt.princess.v

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.OptionValues

import scala.reflect.ClassTag

abstract class BaseSpec extends AnyFlatSpec with Matchers with OptionValues {
  def nameOf[A](implicit tag: ClassTag[A]): String = tag.runtimeClass.getSimpleName
}
