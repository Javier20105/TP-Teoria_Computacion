import Modelo.Gramatica
import Modelo.Produccion
import scala.io.Source
import java.io._

object App {

  def crearProducciones(s: Array[String]): Set[Produccion] = {
    if (s.size == 0) {
      Set()
    } else {
      crearProducciones(s.take(s.size - 1)) + new Produccion(s(s.size - 1).split("->")(0).charAt(0), s(s.size - 1).split("->")(1))
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

  def main(args: Array[String]): Unit = {

    print(importarGramatica("Input/input"))
  }
}