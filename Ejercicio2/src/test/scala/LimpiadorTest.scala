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

  //eliminarProduccionesUnitarias: toma una gramatica y elimina sus producciones unitarias
  val rutaEliminarUnitarios = "input/Limpiador/eliminar_producciones_unitarias/"
  "El Limpiador" should "eliminar todas las producciones unitarias de una gramatica" in {
    val g_importada = Importador.importarGramatica(rutaEliminarUnitarios + "caso1_eliminarProduccionesUnirios")
    val producciones = Set(Produccion('S',"ABC"),Produccion('A',"a"),Produccion('B',"b"),Produccion('C',"c"),Produccion('A',"b"),Produccion('A',"c"),Produccion('B',"c"))
    val g_test = Gramatica(g_importada.terminales,g_importada.variables,g_importada.inicial,producciones)
    assert(g_test == Limpiador.eliminarProduccionesUnitarias(g_importada))
  }

  it should "no cambiar nada si la gramatica no tiene producciones unitarias" in {
    val g_importada1 = Importador.importarGramatica(rutaEliminarUnitarios + "caso2.1_sinProduccionesUnitarias")
    assert(g_importada1 == Limpiador.eliminarProduccionesUnitarias(g_importada1))

    val g_importada2 = Importador.importarGramatica(rutaEliminarUnitarios + "caso2.2_sinProducciones")
    assert(g_importada2 == Limpiador.eliminarProduccionesUnitarias(g_importada2))
  }

  it should "eliminar todas las producciones si la gramatica solo tiene producciones unitarias" in {
    val g_importada = Importador.importarGramatica(rutaEliminarUnitarios + "caso3_todasUnitarias")
    val g_test = Gramatica(g_importada.terminales,g_importada.variables,g_importada.inicial,Set())
    assert(g_test == Limpiador.eliminarProduccionesUnitarias(g_importada))
  }

  it should "contener todas las producciones que se agregan por pares unitarios cuyo segundo elemento tiene una producciones no unitaria " in {
    val g_importada = Importador.importarGramatica(rutaEliminarUnitarios + "caso4_agregarProduccionesDeParesUnitarios")
    val g_test = Limpiador.eliminarProduccionesUnitarias(g_importada)
    assert(g_test.producciones.contains(Produccion('S',"a")))
    assert(g_test.producciones.contains(Produccion('S',"b")))
    assert(g_test.producciones.contains(Produccion('S',"c")))
    assert(g_test.producciones.contains(Produccion('A',"b")))
    assert(g_test.producciones.contains(Produccion('A',"a")))
    assert(g_test.producciones.contains(Produccion('B',"c")))
  }

  //descubrirGeneradores: Toma una gramatica y devuelve el conjunto simbolo generadores
  val rutaDescubrirGeneradores = "input/Limpiador/descubrir_generadores/"

  "El Limpiador" should "descurbrir todos los simbolos generadores de una gramatica" in {
    val g_importada = Importador.importarGramatica(rutaDescubrirGeneradores + "caso1_descubrirTodosLosGeneradores")
    val generadores =Set('a','b','c','A','C')
    assert(generadores == Limpiador.descubrirGeneradores(g_importada))

  }

  it should "descubrir unicamente los terminales si ninguna variables es simbolo generador" in {
    val g_importada = Importador.importarGramatica(rutaDescubrirGeneradores + "caso2.1_sinVariablesGeneradoras")
    val generadores = Set('a','b','c')
    assert(generadores == Limpiador.descubrirGeneradores(g_importada))

    val g_importada2 = Importador.importarGramatica(rutaDescubrirGeneradores + "caso2.2_sinVariables")
    val generadores2 = Set('a','b','c')
    assert(generadores2 == Limpiador.descubrirGeneradores(g_importada2))
  }

  it should "encontrar a una variable como simbolo generador si la parte derecha de una de sus producciones esta hecha de simbolos generadores" in {
    val g_importada = Importador.importarGramatica(rutaDescubrirGeneradores + "caso3_generadoresPorLaParteDerecha")
    val generadores = Limpiador.descubrirGeneradores(g_importada)
    assert(generadores.contains('S'))
    assert(generadores.contains('A'))
    assert(generadores.contains('B'))
  }
  it should "encontrar una variable como simbolo generador si tiene una produccion cuyo lado derecho esta compuesto solo por terminales" in {
    val g_importada = Importador.importarGramatica(rutaDescubrirGeneradores + "caso4_generadoresProTerminalesEnParteDerecha")
    val generadores = Limpiador.descubrirGeneradores(g_importada)
    assert(generadores.contains('A'))
    assert(generadores.contains('B'))


  }

  //eliminarNoGeneradores: Toma una gramatica y elimina los simbolos no generadores
  val rutaEliminarNoGeneradores = "input/Limpiador/eliminar_no_generadores/"

  "El Limpiador" should "eliminar todos los simbolos no generadores de una gramatica" in{
    val g_importada = Importador.importarGramatica(rutaEliminarNoGeneradores + "caso1_eliminarTodosNoGeneradores")
    val producciones = Set(Produccion('A',"a"),Produccion('S',"A"))
    val g_test = Gramatica(g_importada.terminales,g_importada.variables,g_importada.inicial,producciones)
    assert(g_test == Limpiador.eliminarNoGeneradores(g_importada))
  }

  it should "no cambiar nada si todas las variables son generadoras" in {
    val g_importada = Importador.importarGramatica(rutaEliminarNoGeneradores + "caso2_todasVaraiblesGeneradoras")
    assert(g_importada == Limpiador.eliminarNoGeneradores(g_importada))
  }

  it should "eliminar todas las producciones si ninguna variable es generadora" in {
    val g_importada = Importador.importarGramatica(rutaEliminarNoGeneradores + "caso3_ningunaGeneradora")
    val g_test = Gramatica(g_importada.terminales,g_importada.variables,g_importada.inicial,Set())
    assert(g_test == Limpiador.eliminarNoGeneradores(g_importada))

  }
  it should "no hacer nada si la gramatica no tiene producciones" in {
    val g_importada = Importador.importarGramatica(rutaEliminarNoGeneradores + "caso4_sinProducciones")
    assert(g_importada == Limpiador.eliminarNoGeneradores(g_importada))
  }

  it should "incluir todas las producciones cuya parte derecha esta compueste unicamente por terminales o generadores" in {
    val g_importada = Importador.importarGramatica(rutaEliminarNoGeneradores + "caso5_incluirTerminalesParteDerecha")
    val g_test = Limpiador.eliminarNoGeneradores(g_importada)
    assert(g_test.producciones.contains(Produccion('A',"abc")))
    assert(g_test.producciones.contains(Produccion('B',"b")))
    assert(g_test.producciones.contains(Produccion('C',"ca")))
    assert(g_test.producciones.contains(Produccion('S',"ABC")))
    assert(g_test.producciones.contains(Produccion('A',"BC")))
    assert(!g_test.producciones.contains(Produccion('D',"BDCs")))
  }

  //descubrirAlcanzables: Recibe una gramatica y retorna los simbolos alcanzables
  val rutaDescubrirAlcanzables = "input/Limpiador/descubrir_alcanzables/"

  "El Limpiador" should "descubrir todos lo simsbolos alcanzables de una gramatica" in {
    val g_importada = Importador.importarGramatica(rutaDescubrirAlcanzables + "caso1_encontrarTodosLosAlacanzables")
    val alcanzables =Set('S','A','B','C','E')
    assert(alcanzables == Limpiador.descubrirAlcanzables(g_importada))
  }

  it should "encontrar unicamente al simbolo inicial si no hay una produccion que provenga de S" in {
    val g_importada = Importador.importarGramatica(rutaDescubrirAlcanzables + "caso2_sinProduccionesS")
    val alcanzables = Set(g_importada.inicial)
    assert(alcanzables == Limpiador.descubrirAlcanzables(g_importada))

    val g_importada2 = Importador.importarGramatica(rutaDescubrirAlcanzables + "caso2.1_sinProducciones")
    val alcanzables2 = Set(g_importada2.inicial)
    assert(alcanzables2 == Limpiador.descubrirAlcanzables(g_importada2))
  }

  it should "encontrar a una variable o terminal como alcanzable si puede ser derivada desde el simbolo inicial" in {
    val g_importada = Importador.importarGramatica(rutaDescubrirAlcanzables + "caso3_alcanzablePorS")
    val alcanzables = Set('S','A','B','C','a','b')
    assert(alcanzables == Limpiador.descubrirAlcanzables(g_importada))
  }

  //eliminarNoAlcanzables: Toma una gramatica y elimina los simbolos no alcanzables
  val eliminarNoAlcanzables = "input/Limpiador/eliminar_no_alcanzables/"

  "El Limpiador" should "eliminar todas producciones con variables no alcanzables de una gramatica" in {
    val g_importada = Importador.importarGramatica(eliminarNoAlcanzables + "caso1_eliminarTodosNoAlcanzables")
    val producciones = Set(Produccion('S',"ABC"),Produccion('A',"F"),Produccion('F',"G"),Produccion('G',"H"))
    val g_test = Gramatica(g_importada.terminales,g_importada.variables,g_importada.inicial,producciones)
    assert(g_test == Limpiador.eliminarNoAlcanzables(g_importada))
  }

  it should "no cambiar nada si todas las variables y terminales son alcanzables" in {
    val g_importada = Importador.importarGramatica(eliminarNoAlcanzables + "caso2_todosAlcanzables")
    assert(g_importada == Limpiador.eliminarNoGeneradores(g_importada))
  }

  it should "no cambiar nada si la gramatica no tiene producciones" in {
    val g_importada = Importador.importarGramatica(eliminarNoAlcanzables + "caso3_sinProducciones")
    assert(g_importada == Limpiador.eliminarNoGeneradores(g_importada))
  }

  it should "eliminar todas las producciones si no hay ninguna produccion que porvenga del simbolo inicial" in {
    val g_importada = Importador.importarGramatica(eliminarNoAlcanzables + "caso4_sinProduccionesS")
    val g_test = Gramatica(g_importada.terminales,g_importada.variables,g_importada.inicial,Set())
    assert(g_test == Limpiador.eliminarNoAlcanzables(g_importada))
  }



















}
