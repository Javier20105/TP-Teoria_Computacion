package Codigo

import Modelo.Gramatica
import Modelo.Produccion
import scala.io.Source
import java.io._

object Importador {
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
}