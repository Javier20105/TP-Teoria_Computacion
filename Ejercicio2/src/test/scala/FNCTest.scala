import org.scalatest._
import Codigo.Importador
import Codigo.FNC
import Modelo._

abstract class UnitSpec extends FlatSpec with Matchers with OptionValues with Inside with Inspectors

class FNCTest extends UnitSpec{

  //crearProduccionesTerminales: Toma una gramatica
  val rutaCrearProduccionesTerminales = "input/FNC/crear_producciones_terminales/"

  "FNC" should "crear producciones para cada terminal" in {
    val g_importada = Importador.importarGramatica(rutaCrearProduccionesTerminales + "caso1_crearProduccionesTerminales")
    val (prodTerminales,sinUsar) = (Set(Produccion('B',"b"),Produccion('C',"c"),Produccion('D',"d")), ('E' to 'Z').toList ::: List('Ñ'))
    assert((prodTerminales,sinUsar) == FNC.crearProduccionesTerminales(g_importada))
  }

  it should "no crear nada si ya hay una produccion para cada terminal" in {
    val g_importada = Importador.importarGramatica(rutaCrearProduccionesTerminales + "caso2_produccionParaCadaTerminal")
    val (prodTerminales,sinUsar) = FNC.crearProduccionesTerminales(g_importada)
    assert(prodTerminales.subsetOf(g_importada.producciones))
    assert(sinUsar == ('E' to 'Z').toList ::: List('Ñ'))
  }

  it should "crear producciones terminales solo para aquellos que no dispongan de una" in {
    val g_importada = Importador.importarGramatica(rutaCrearProduccionesTerminales + "caso3_produccionesTerminalesSoloSiEsNecesario")
    val (prodTerminales,sinUsar) = ((Set(Produccion('B',"b"),Produccion('C',"c"),Produccion('D',"d"))),('E' to 'Z').toList ::: List('Ñ'))
    assert((prodTerminales,sinUsar) == FNC.crearProduccionesTerminales(g_importada))
  }

  //reemplazarTerminales: Toma el conjunto de producciones de una gramatica y sus producciones terminales y realiza el
  //proceso de reemplazar los simbolos terminales en las producciones con lado derecho con longitud mayor o igual a 2
  val rutaReemplazarTerminales = "input/FNC/reemplazar_terminales/"

  "FNC" should "reemplazar todos los terminales talque segun sea necesario por la FNC" in {
    val g_importada = Importador.importarGramatica(rutaReemplazarTerminales + "caso1_reemplazarTerminales")
    val prod_test = Set(Produccion('D',"ABC"),Produccion('E',"FBC"),Produccion('F',"GG"),Produccion('G',"AD"))
    val prod_terminales = Set(Produccion('A',"a"),Produccion('B',"b"),Produccion('C',"c"))
    assert(prod_test == FNC.reemplazarTerminales(g_importada.producciones,prod_terminales))
  }

  it should "no hacer nada si no hay terminales en las procuducciones" in {
    val g_importada = Importador.importarGramatica(rutaReemplazarTerminales + "caso2_produccionesSinTerminales")
    val prod_terminales = Set(Produccion('A',"a"),Produccion('B',"b"),Produccion('C',"c"))
    assert(g_importada.producciones == FNC.reemplazarTerminales(g_importada.producciones,prod_terminales))
  }

  it should "reemplazar todos los simbolos de los lados derechos si estos solo estan compuestos por terminales" in {
    val g_importada = Importador.importarGramatica(rutaReemplazarTerminales + "caso3_reemplazarTodosTerminales")
    val prod_test = Set(Produccion('D',"ABC"),Produccion('E',"ABC"),Produccion('F',"CC"),Produccion('G',"AC"))
    val prod_terminales = Set(Produccion('A',"a"),Produccion('B',"b"),Produccion('C',"c"))
    assert(prod_test == FNC.reemplazarTerminales(g_importada.producciones,prod_terminales))
  }

  it should "no hacer nada si la gramatica no tiene producciones" in {
    val g_importada = Importador.importarGramatica(rutaReemplazarTerminales + "caso4_sinProducciones")
    val prod_terminales = Set(Produccion('A',"a"),Produccion('B',"b"),Produccion('C',"c"))
    assert(g_importada.producciones == FNC.reemplazarTerminales(g_importada.producciones,prod_terminales))
  }


}
