import org.scalatest._

abstract class UnitSpec extends FlatSpec with Matchers with OptionValues with Inside with Inspectors

class ExampleSpec extends UnitSpec {

  "Una lista" should "contener el vaor que se le acaba de agregar" in {
    val l = List()
    val l2 = 1 :: l
    assert(l2.contains(1))

  }

  it should "throw  UnsupportedOperationException si se le hace una operacion max a una lista vacia" in {
    val l = List(1)
    val tail = l.tail
    a[UnsupportedOperationException] should be thrownBy {
      tail.max
    }
  }
}

//para correr el test en eclipse Run As -> Scala Test Suite