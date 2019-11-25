import java.io.FileNotFoundException
import org.scalatest._
import Codigo.Main2
import Modelo._
import org.scalactic.source.Position.apply

abstract class UnitSpec extends FlatSpec with Matchers with OptionValues with Inside with Inspectors

class Test extends  UnitSpec {
  // val generales
  // no sé que onda venian los mains con el mismo nombre.
  val a_importado = Main2.importarAutomata("Input/TestInputs/TestImportar")
  val transformado = Main2.transformer(a_importado)
  
  "El importador" should "leer un automata correctamente si el input cumple con el formato de entrada" in {
    val e1 = new Estado(Set("1"),true,true)
    val e2 = new Estado(Set("2"),true,false)
    val e3 = new Estado(Set("3"),true,false)
    val e4 = new Estado(Set("4"),false,false)
    val t1 = new Transicion(e1,'a',e2)
    val t2 = new Transicion(e1,'b',e3)
    val t3 = new Transicion(e1,'E',e4)
    val t4 = new Transicion(e3,'b',e2)
    val t5 = new Transicion(e4,'a',e3)
    
    val alfabeto = Set('a','b','c')
    val estados = Set(e1,e2,e3,e4)
    val estadosFinales = Set(Set("1"),Set("2"),Set("3"))
    val transiciones = Set(t1,t2,t3,t4,t5)
    
    val a_test = new Automata(alfabeto,estados,estadosFinales,transiciones)
    
    assert(a_importado == a_test)
  }
  
  it should "el resultado AFD debería tener el alfabeto" in {
    val alfabeto = Set('a','b','c')
    assert(alfabeto == transformado.alfabetoInput)
  }
  
  it should "el resultado AFD debería tener los siguientes estados" in {
    val e1 = new Estado(Set("2"),true,false)
    val e2 = new Estado(Set("3"),true,false)
    val e3 = new Estado(Set("3","2"),true,false)
    val e4 = new Estado(Set("1","4"),true,true)
    val e5 = new Estado(Set(),false,false)
    val estados = Set(e1,e2,e3,e4,e5)
    assert(estados == transformado.estados)
  }
  
  it should "el resultado AFD debería tener los estados finales" in {
    val estadosFinales = Set(Set("3","2"),Set("2"),Set("3"),Set("1","4"))
    assert(estadosFinales == transformado.estadosFinales)
  }
  
  it should "el resultado AFD debería tener las siguientes transiciones" in {
    val e1 = new Estado(Set("2"),true,false)
    val e2 = new Estado(Set("3"),true,false)
    val e3 = new Estado(Set("3","2"),true,false)
    val e4 = new Estado(Set("1","4"),true,true)
    val e5 = new Estado(Set(),false,false)
    val t1 = new Transicion(e1,'b',e5)
    val t2 = new Transicion(e2,'b',e1)
    val t3 = new Transicion(e3,'b',e1)
    val t4 = new Transicion(e4,'b',e2)
    val t5 = new Transicion(e2,'a',e5)
    val t6 = new Transicion(e1,'c',e5)
    val t7 = new Transicion(e5,'a',e5)
    val t8 = new Transicion(e4,'c',e5)
    val t9 = new Transicion(e3,'a',e5)
    val t10 = new Transicion(e5,'c',e5)
    val t11 = new Transicion(e5,'b',e5)
    val t12 = new Transicion(e3,'c',e5)
    val t13 = new Transicion(e1,'a',e5)
    val t14 = new Transicion(e4,'a',e3)
    val t15 = new Transicion(e2,'c',e5)
    val transiciones = Set(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15)
    assert(transiciones == transformado.transiciones)
  }
  
  it should "transformar el automata E-AFND a AFD" in {
    val alfabeto = Set('a','b','c')
    val e1 = new Estado(Set("2"),true,false)
    val e2 = new Estado(Set("3"),true,false)
    val e3 = new Estado(Set("3","2"),true,false)
    val e4 = new Estado(Set("1","4"),true,true)
    val e5 = new Estado(Set(),false,false)
    val estados = Set(e1,e2,e3,e4,e5)
    val estadosFinales = Set(Set("3","2"),Set("2"),Set("3"),Set("1","4"))
    val t1 = new Transicion(e1,'b',e5)
    val t2 = new Transicion(e2,'b',e1)
    val t3 = new Transicion(e3,'b',e1)
    val t4 = new Transicion(e4,'b',e2)
    val t5 = new Transicion(e2,'a',e5)
    val t6 = new Transicion(e1,'c',e5)
    val t7 = new Transicion(e5,'a',e5)
    val t8 = new Transicion(e4,'c',e5)
    val t9 = new Transicion(e3,'a',e5)
    val t10 = new Transicion(e5,'c',e5)
    val t11 = new Transicion(e5,'b',e5)
    val t12 = new Transicion(e3,'c',e5)
    val t13 = new Transicion(e1,'a',e5)
    val t14 = new Transicion(e4,'a',e3)
    val t15 = new Transicion(e2,'c',e5)
    val transiciones = Set(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15)
    val a_test = new Automata(alfabeto,estados,estadosFinales,transiciones)
    assert(transformado == a_test)
  }
  
  "El metodo transicionesDe" should "dar las transiciones pertenecientes al estado de input" in {
    val e1 = new Estado(Set("2"),true,false) //  destino
    val e2 = new Estado(Set("3"),true,false) //  a buscar
    val e5 = new Estado(Set(),false,false) //   destino
    val t2 = new Transicion(e2,'b',e1)
    val t5 = new Transicion(e2,'a',e5)
    val t15 = new Transicion(e2,'c',e5)
    val transiciones_test = Set(t2,t5,t15)
    val transicionesAFD = Main2.transicionesDe(e2, transformado.transiciones)
    assert(transiciones_test == transicionesAFD)
  }

  it should "throw  tal si el archivo de input no se encuentra en la lista especificada" in {
    a[FileNotFoundException] should be thrownBy {
      val g_importada = Main2.importarAutomata("rutaInvalida")
    }
  }

}
