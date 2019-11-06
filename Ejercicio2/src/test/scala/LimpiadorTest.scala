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

  //eliminarProduccionesEpsilon: toma una gramatica y un conjunto de simbolos, y los elimina de la gramatica
  val rutaEliminarEpsilon = "input/Limpiador/eliminar_producciones_epsilon/"

  //crearParesUnitarios: toma una gramatica y retorna el conjunto de pares unitarios
  val rutaCrearParesUnitarios = "input/Limpiador/crear_pares_unitarios/"

  "El Limpiador" should "encontrar todos los pares unitarios de un conjunto de producciones" in {
    val g_importada = Importador.importarGramatica(rutaCrearParesUnitarios + "casoEncontraraTodosLosParesUnitarios")
    val pares_test =Set(('S','S'),('A','A'),('B','B'),('C','C'),('D','D'),('A','C'),('B','A'),('B','C'),('A','D'),('B','D'),('C','D'))
    assert(pares_test == Limpiador.crearParesUnitarios(g_importada))
  }
  it should "encontrar solo los pares (A,A) con A en V el conjuntos de variables, si la gramatica no tiene producciones unitarias" in{
    val g_importada = Importador.importarGramatica(rutaCrearParesUnitarios + "casoSinProduccionesUnitarias")
    val pares_test = Set(('S','S'),('A','A'),('B','B'),('C','C'),('D','D'))
    print("Me esta dando: " + Limpiador.crearParesUnitarios(g_importada))
    assert(pares_test == Limpiador.crearParesUnitarios(g_importada))
  }
  it should "encontrar todas las variables tal que A=>*B solo con pares unitarios" in{
    val g_importada = Importador.importarGramatica(rutaCrearParesUnitarios+ "casoDerivacionEncadena")
    val pares_test = Set(('S','S'),('A','A'),('B','B'),('C','C'),('S','A'),('S','B'),('S','C'),('A','B'),('A','C'),('B','C'))
    assert(pares_test == Limpiador.crearParesUnitarios(g_importada))

    val g_importada1 = Importador.importarGramatica(rutaCrearParesUnitarios+ "casoDerivacionEncadenaConBifurcacion")
    val pares_test1 = Set(('S','S'),('A','A'),('B','B'),('C','C'),('D','D'),('E','E'),('F','F'),('S','A'),('S','B'),('S','C'),('S','D'),('S','E'),('S','F'),('A','B'),('A','C'),('B','C'),('D','E'),('D','F'),('E','F'),('A','D'),('A','E'),('A','F'),('F','C'),('E','C'),('D','C'))
    assert(pares_test1 == Limpiador.crearParesUnitarios(g_importada1))

    val g_importada2 = Importador.importarGramatica(rutaCrearParesUnitarios + "casoDerivacionConCiclo")
    val pares_test2 = Set(('S','S'),('A','A'),('B','B'),('C','C'),('S','A'),('S','B'),('S','C'),('A','S'),('A','B'),('A','C'),('B','S'),('B','A'),('B','C'),('C','S'),('C','A'),('C','B'))
    assert(pares_test2 == Limpiador.crearParesUnitarios(g_importada2))
  }


  
  

}
