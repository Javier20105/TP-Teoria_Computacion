import Modelo.Gramatica
import Modelo.Produccion
import scala.io.Source
import java.io._

object App {

  def importarGramatica(ruta: String): Gramatica = {

    def crearProducciones(s: Array[String]): Set[Produccion] = {
      if (s.size == 0) {
        Set()
      } else {
        val partesProd = s(s.size - 1).split("->")
        crearProducciones(s.take(s.size - 1)) + new Produccion(partesProd(0).charAt(0), partesProd(1))
      }
    }
    val lineas = Source.fromFile(ruta).getLines()
    val terminales = Set() ++ lineas.next.toList
    val variables = Set() ++ lineas.next.toList
    val inicial = lineas.next.charAt(0)
    val producciones = crearProducciones(lineas.next().split(","))

    new Gramatica(terminales, variables, inicial, producciones)
  }

  def descubrirNulleables(producciones: Set[Produccion]): Set[Char] = {

    def esNulleable(p: Produccion, n: Set[Char]): Boolean = {
      p.cadena.map(n.contains(_)).fold(true)(_ && _)

    }

    def casoBase(producciones: Set[Produccion]): Set[Char] = {
      producciones.filter(_.esEpsilon).map(_.variable)
    }

    def casoInductivo(anterior: Set[Char], producciones: Set[Produccion]): Set[Char] = {
      anterior ++ producciones.filter(esNulleable(_, anterior)).map(_.variable)
    }

    def recursiva(anterior: Set[Char], actual: Set[Char], producciones: Set[Produccion]): Set[Char] = {
      if (anterior == actual) {
        return actual
      } else {
        val siguiente = casoInductivo(actual, producciones)
        recursiva(actual, siguiente, producciones)
      }
    }

    val paso0 = casoBase(producciones)
    val paso1 = casoInductivo(paso0, producciones)
    recursiva(paso0, paso1, producciones)
  }

  def eliminarProduccionesEpsilon(g: Gramatica, nulleables: Set[Char]): Gramatica = {

    //"-------------------------------------------------------------------"

    def encontrarPrimero(s: String, c: Char): Int = {
      s.indexOf(c)
    }
    def quitar(s: String, index:Int): String = {
      s.slice(0, index) + s.slice(index + 1, s.size)
    }

    def nulleablesEnProduccion(p: Produccion, c: List[Char]): List[Char] = {
      p.cadena.toList.filter(c.contains(_))
    }

    def recursiva(p: Produccion, nulleables: List[Char],index:Int): Set[Produccion] = {
      if (nulleables.size == 0) {
        Set(p)
      } else {
        val n = nulleables.tail
        val i = index + encontrarPrimero(p.cadena.slice(index,p.cadena.size), nulleables.head)
        recursiva(p, n,i) ++ recursiva(new Produccion(p.variable, quitar(p.cadena, i)), n,i)

      }

    }

    def produccionSinEpsilon(p: Produccion, nulleables: List[Char]): Set[Produccion] = {
      recursiva(p, nulleablesEnProduccion(p, nulleables),0)
    }

    val resul = g.producciones.toList.map(produccionSinEpsilon(_, nulleables.toList))
    print("Resul: " + resul)

    null
  }

  def main(args: Array[String]): Unit = {

    val g = importarGramatica("Input/input")
    val nulleables = descubrirNulleables(g.producciones)
    println(g)
    println("Nulleables: " + nulleables)
    val sinEpsilon = eliminarProduccionesEpsilon(g, nulleables)

  }
}