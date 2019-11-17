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

  it should "deveria devolver una gramatica sin producciones si no hay producciones en el input" in{
    val g_importada = Importador.importarGramatica("input/Importador/caso2_sinProducciones")
    val terminales = Set('a','b','c')
    val variables = Set('S','A','B','C')
    val inicial = 'S'
    val g_test = new Gramatica(terminales,variables,inicial,Set())
    assert(g_importada == g_test)

  }

  it should "deveria devolver null si el archivo esta vacio" in {
    val g_importada = Importador.importarGramatica("input/Importador/caso3_vacio")
    assert(null == g_importada)
  }




  it should "throw  tal si el archivo de input no se encuentra en la lista especificada" in {
    a[FileNotFoundException] should be thrownBy {
      val g_importada = Importador.importarGramatica("rutaInvalida")
    }


  }

}
