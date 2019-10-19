import Modelo.Gramatica
import Modelo.Produccion
import scala.io.Source
import java.io._

object App {

  def crearProducciones(s: Array[String]): Set[Produccion] = {
    if (s.size == 0) {
      Set()
    } else {
      val partesProd = s(s.size - 1).split("->")
      crearProducciones(s.take(s.size - 1)) + new Produccion(partesProd(0).charAt(0), partesProd(1))
    }
  }

  def importarGramatica(ruta: String): Gramatica = {
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

    def casoInductivo(anterior:Set[Char], producciones:Set[Produccion]):Set[Char] = {
      anterior ++ producciones.filter(esNulleable(_,anterior)).map(_.variable)
    }

    def recursiva(anterior: Set[Char], actual: Set[Char], producciones: Set[Produccion]): Set[Char] = {
      if (anterior == actual) {
        return actual
      } else {
        val siguiente = casoInductivo(actual,producciones)
        recursiva(actual, siguiente, producciones)
      }
    }
    
    val paso0 = casoBase(producciones)
    val paso1 =casoInductivo(paso0,producciones)    
    recursiva(paso0, paso1, producciones)
  }

  def main(args: Array[String]): Unit = {

    val g = importarGramatica("Input/input")
    println(g)
    println("Nulleables: " + descubrirNulleables(g.producciones))
  }
}