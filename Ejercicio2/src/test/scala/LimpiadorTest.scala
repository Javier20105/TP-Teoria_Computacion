import org.scalatest._
import Codigo.Importador
import Codigo.Limpiador
import Modelo._

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

  "El Limpiador" should "eliminar todas las producciones epsilon de un gramatica" in {
    val g_importada = Importador.importarGramatica(rutaEliminarEpsilon + "caso1_eliminarNulleables")
    val producciones = Set(Produccion('S',"ABC"),Produccion('S',"AC"),Produccion('S',"BC"),Produccion('S',"C"),Produccion('C',"a"))
    val g_test = Gramatica(g_importada.terminales,g_importada.variables,g_importada.inicial, producciones)
    assert(g_test == Limpiador.eliminarProduccionesEpsilon(g_importada))
  }

  it should "no cambiar nada si la gramatica no tiene producciones epsilon" in {
    val g_importada1 = Importador.importarGramatica(rutaEliminarEpsilon + "caso2.1_sinProduccionesEpsilon")
    assert(g_importada1 == Limpiador.eliminarProduccionesEpsilon(g_importada1))

    val g_importada2 = Importador.importarGramatica(rutaEliminarEpsilon + "caso2.2_sinProduccionesEpsilon")
    assert(g_importada2 == Limpiador.eliminarProduccionesEpsilon(g_importada2))

    val g_importada3 = Importador.importarGramatica(rutaEliminarEpsilon + "caso2.3_sinProducciones")
    assert(g_importada3 == Limpiador.eliminarProduccionesEpsilon(g_importada3))

  }
  it should "eliminar todas las producciones si la gramatica solo tiene producciones epsilon" in {
    val g_importada = Importador.importarGramatica(rutaEliminarEpsilon + "caso3_soloProduccionesEpsilon")
    val g_test = Gramatica(g_importada.terminales,g_importada.variables,g_importada.inicial,Set())
    assert(g_test == Limpiador.eliminarProduccionesEpsilon(g_importada))
  }

  it should "contener todas las versiones de una gramaticas cuyo lado derecho esta compuesto por simbolos nulleables" in {
    val g_importada = Importador.importarGramatica(rutaEliminarEpsilon + "caso4_todasLasVersiones")
    val g_test = Limpiador.eliminarProduccionesEpsilon(g_importada)
    assert(g_test.producciones.contains(Produccion('S',"ABC")))
    assert(g_test.producciones.contains(Produccion('S',"AB")))
    assert(g_test.producciones.contains(Produccion('S',"AC")))
    assert(g_test.producciones.contains(Produccion('S',"A")))
    assert(g_test.producciones.contains(Produccion('S',"BC")))
    assert(g_test.producciones.contains(Produccion('S',"C")))
    assert(g_test.producciones.contains(Produccion('S',"B")))
  }

  it should "mantener el orden realitivo de los simbolos no nulleables en todas las versiones de un gramatica" in {
    val g_importada = Importador.importarGramatica(rutaEliminarEpsilon + "caso5_ordenRelativo")
    val g_test = Limpiador.eliminarProduccionesEpsilon(g_importada)
    assert(g_test.producciones.contains(Produccion('S',"AaBsC")))
    assert(g_test.producciones.contains(Produccion('S',"AaBs")))
    assert(g_test.producciones.contains(Produccion('S',"AasC")))
    assert(g_test.producciones.contains(Produccion('S',"Aas")))
    assert(g_test.producciones.contains(Produccion('S',"aBsC")))
    assert(g_test.producciones.contains(Produccion('S',"asC")))
    assert(g_test.producciones.contains(Produccion('S',"aBs")))
  }


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
