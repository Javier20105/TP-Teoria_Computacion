import org.scalatest._
import Codigo.Importador
import Codigo.CYK
import Modelo._

abstract class UnitSpec extends FlatSpec with Matchers with OptionValues with Inside with Inspectors

class CYKTest extends UnitSpec {
  //generarMatriz: Genera la matriz triangular. La gramatica debe estar limpia y en forma normal de chomsky
  val rutaGenerarMatriz = "input/CYK/generar_matriz/"
  "La matriz" should "generar correctamente la matriz si la cadena es la unica que pertenece al lenguaje" in {
    //es correcto si: sea c una cadena la matriz debe tenee tantos niveles como letras en la cadena, tiene forma triangular
    //la S esta en el nivel superior de la cadena si y solo si c pertenece al lenguaje de la gramatica
    val g_importada = Importador.importarGramatica(rutaGenerarMatriz + "caso1_palabraenLenguaje")
    val cadena = "abab"
    val matriz = List(List(List('A'), List('B'), List('A'), List('B')),   List(List('D'), List(), List('D')),     List(List(), List('C')),    List(List('S')))
    assert(matriz == CYK.generarMatriz(cadena,g_importada))


    //para cualquier otra parlabras no debe estar la S
    val cadena1 = "bbaaba"
    val matriz1 = List(List(List('B'), List('B'), List('A'), List('A'), List('B'), List('A')),     List(List(), List(), List(), List('D'), List()),     List(List(), List(), List(), List()),      List(List(), List(), List()),     List(List(), List()),     List(List()))
    assert(matriz1 == CYK.generarMatriz(cadena1,g_importada))
    val cadena2 = "aab"
    val matriz2 = List(List(List('A'), List('A'), List('B')),      List(List(), List('D')),      List(List()))
    assert(matriz == CYK.generarMatriz(cadena2,g_importada))
   /* val matriz2 = CYK.generarMatriz(cadena2, g_importada).reverse
    assert(!matriz2.head.head.contains('S'))
    assert(matriz2.length == cadena2.length)
    for (n <- 0 to (cadena2.length-1)) assert(matriz2(n).length == n+1)*/


  }

  it should "generar correctamente la matriz si la el lenguaje acepta mas de una cadena" in {

    val g_importada = Importador.importarGramatica(rutaGenerarMatriz + "caso2_masDeUnaPalabraEnLenguaje")
    val cadena = "c"
    val matriz = List(List(List('S')))
    assert(matriz == CYK.generarMatriz(cadena,g_importada))

    val cadena1 = "acb"
    val matriz1 = List(List(List('A'), List('S'), List('B')),      List(List(), List('C')),      List(List('S')))
    assert(matriz1 == CYK.generarMatriz(cadena1,g_importada))
    val cadena2 = "aacbb"
    val matriz2 = List(List(List('A'), List('A'), List('S'), List('B'), List('B')),      List(List(), List(), List('C'), List()),      List(List(), List('S'), List()),      List(List(), List('C')),      List(List('S')))
    assert(matriz2 == CYK.generarMatriz(cadena2,g_importada))

    val cadena3 = "aaacbbb"
    val matriz3 = List(List(List('A'), List('A'), List('A'), List('S'), List('B'), List('B'), List('B')),      List(List(), List(), List(), List('C'), List(), List()),      List(List(), List(), List('S'), List(), List()),      List(List(), List(), List('C'), List()),      List(List(), List('S'), List()),      List(List(), List('C')),      List(List('S')))
    assert(matriz3 == CYK.generarMatriz(cadena3,g_importada))
    //para una palabra que no pertenesca al lenguaje no debe estar la S en el ultimo nivel
    val cadena4 = "aaabbb"
    val matriz4 = List(List(List('A'),List('A'),List('A'),List('B'),List('B'),List('B')),     List(List(),List(),List(),List(),List()),    List(List(),List(),List(),List()),     List(List(),List(),List()),      List(List(),List()),     List(List())      )
    assert(matriz4 == CYK.generarMatriz(cadena4,g_importada))

    val cadena5 = "bacba"
    val matriz5 = List(List(List('B'),List('A'),List('S'),List('B'),List('A')), List(List(),List(),List('C'),List()), List(List(),List('S'),List()),  List(List(),List()),List(List())          )
    assert(matriz5 == CYK.generarMatriz(cadena5,g_importada))

  }

  it should "generar corestamente la matriz si hay mas de una forma de crear la misma cadena" in{
    val g_importada = Importador.importarGramatica(rutaGenerarMatriz + "caso3_redundanciaDeCadena")
    val cadena = "aaab"
    val matriz = List()
    assert(matriz == CYK.generarMatriz(cadena,g_importada))

  }

}
