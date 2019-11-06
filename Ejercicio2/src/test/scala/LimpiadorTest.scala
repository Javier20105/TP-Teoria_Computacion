import org.scalatest._
import Codigo.Importador
import Codigo.Limpiador

abstract class UnitSpec extends FlatSpec with Matchers with OptionValues with Inside with Inspectors

class LimpiadorTest extends UnitSpec {
  //descubrirulleables: Este metodo toma un conjunto de producciones y retorna el conjunto de simbolos nulleables
  val rutaDescubrirNulleables = "input/Limpiador/descubrir_nulleables/"
  "El limpiador" should "encontrar todos los simbolos nulleables de una gramatica " in{
    val g_importada = Importador.importarGramatica(rutaDescubrirNulleables + "caso1_EncontrarTodosNulleables")
    val nulleables_test = Set('S','A','B')
    assert(nulleables_test == Limpiador.descubrirNulleables(g_importada.producciones))
  }

  it should "no encontrar simbolos nulleables si la gramatica no tiene producciones epsilon " in {
    val g_importada = Importador.importarGramatica(rutaDescubrirNulleables + "caso2_SinEpsilon")
    val nulleables_test = Set()
    assert(nulleables_test == Limpiador.descubrirNulleables(g_importada.producciones))
  }

  it should "encontrar a todas la variables como simbolos nulleables si cada una tiene una producción epsilon" in {
    val g_importada = Importador.importarGramatica(rutaDescubrirNulleables + "caso3_casoTodasConEpsilon")
    val nulleables_test = Set('S','A','B','C')
    assert(nulleables_test == Limpiador.descubrirNulleables(g_importada.producciones))
  }

  it should "encontrar a una variable como simbolo nulleable si la parte derecha de una de sus producciones esta hecha de simbolos nulleables"in{
    val g_importada = Importador.importarGramatica(rutaDescubrirNulleables + "caso4_casoNulleablesPorParteDerecha")
    assert(Limpiador.descubrirNulleables(g_importada.producciones).contains('S'))
  }

  it should "encontrar a una todas las variables que cumplan que su parte derecha esta compuesta solo de simbolos nulleables in" in {
    val g_importada = Importador.importarGramatica(rutaDescubrirNulleables + "caso5_casoTodosNulleablesPorParteDerecha")
    assert(Limpiador.descubrirNulleables(g_importada.producciones).contains('S'))
    assert(Limpiador.descubrirNulleables(g_importada.producciones).contains('O'))
    assert(Limpiador.descubrirNulleables(g_importada.producciones).contains('P'))
    assert(Limpiador.descubrirNulleables(g_importada.producciones).contains('Q'))
  }
  it should "encontrar nada si la gramatica no tiene producciones " in{
    val g_importada = Importador.importarGramatica(rutaDescubrirNulleables + "caso6_SinProducciones")
    val nulleables_test = Set()
    assert(nulleables_test == Limpiador.descubrirNulleables(g_importada.producciones))
  }

  //
  
  

}
