import java.io.FileNotFoundException

import org.scalatest._
import Codigo.Importador
import Modelo._

abstract class UnitSpec extends FlatSpec with Matchers with OptionValues with Inside with Inspectors

class ImportadorTest extends  UnitSpec {
  "El importador" should "leer una gramatica correctamente si el input cumple con el formato de entrada" in {
    val g_importada = Importador.importarGramatica("input/Importador/caso1_formatoCorrecto")
    val terminales = Set('a','b','c')
    val variables = Set('S','A','B','C')
    val inicial = 'S'
    val producciones = Set(new Produccion('S',"A"),new Produccion('A',"a"),new Produccion('B',"b"), new Produccion('C',"c"))
    val g_test = new Gramatica(terminales,variables,inicial,producciones)
    assert(g_importada == g_test)

  }

  it should " devolver una gramatica sin producciones si no hay producciones en el input" in{
    val g_importada = Importador.importarGramatica("input/Importador/caso2_sinProducciones")
    val terminales = Set('a','b','c')
    val variables = Set('S','A','B','C')
    val inicial = 'S'
    val g_test = new Gramatica(terminales,variables,inicial,Set())
    assert(g_importada == g_test)

  }

  it should " devolver null si el archivo esta vacio" in {
    val g_importada = Importador.importarGramatica("input/Importador/caso3_vacio")
    assert(null == g_importada)
  }

  it should " eliminar los carasteres que no sean letras o flechas o epsilon"  in {
    val g_importada = Importador.importarGramatica("input/Importador/caso4_caracterresInvalidos")
    val terminales = Set('a','b','c')
    val variables = Set('A','B','C','S')
    val inicial = 'S'
    val producciones = Set(Produccion('S',"a"))
    val g_test = Gramatica(terminales,variables,inicial,producciones)
    assert(g_test == g_importada)
  }

  it should "devolver null si el archivo no cumple con el formato especificado y no puede corregirse" in {
    val g_importada = Importador.importarGramatica("input/Importador/caso5_incorregible")
    assert(null == g_importada)
  }

  it should "devolver null si se indica mas de una variable inicial" in {
    val g_importada = Importador.importarGramatica("input/Importador/caso6_masDeUnInicial")
    assert(null == g_importada)
  }

  it should "devolver null si la variable inicial no esta en el conjunto de variables" in {
    val g_importada = Importador.importarGramatica("input/Importador/caso7_inicialNoEsVariable")
    assert(null == g_importada)
  }

  it should "devolver null si una produccion tiene un simbolo que no es variable ni terminal" in {
    val g_importada = Importador.importarGramatica("input/Importador/caso8_noEsVariableNiTerminal")
    assert(null == g_importada)


  }

  it should "devolver null si no esta especificada la variable inicial" in {
    val g_importada = Importador.importarGramatica("input/Importador/caso9_sinInicial")
    assert(null == g_importada)
  }

  it should "devolver null si no hay ningun terminal valido en la primera linea del archivo" in {
    val g_importada = Importador.importarGramatica("input/Importador/caso10_sinTermianlesValidos")
    assert(null == g_importada)
  }

  it should "devolver null si una produccion esta incompleta" in {
    val g_importada1 = Importador.importarGramatica("input/Importador/caso11.1_produccionIncompletaSinDerecho")
    assert(null == g_importada1)

    val g_importada2 = Importador.importarGramatica("input/Importador/caso11.2_produccionIncompletaSinVariable")
    assert(null == g_importada1)

    val g_importada3 = Importador.importarGramatica("input/Importador/caso11.3_produccionIncompletaSinNada")
    assert(null == g_importada3)
  }




  it should "throw  tal si el archivo de input no se encuentra en la lista especificada" in {
    a[FileNotFoundException] should be thrownBy {
      val g_importada = Importador.importarGramatica("rutaInvalida")
    }


  }

}
