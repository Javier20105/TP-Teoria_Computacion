package Codigo

import Modelo.Gramatica
import Modelo.Produccion

object CYK {

  def generarMatriz(cadena: String, g: Gramatica): List[List[Set[Char]]] = {

    def casoBase(cadena: String, producciones: Set[Produccion]): List[Set[Char]] = {

      if (cadena.size == 0) {
        List()
      } else {
        val r = producciones.filter((p: Produccion) => p.cadena.head == cadena.head && p.cadena.size == 1).map(_.variable)

        r :: casoBase(cadena.tail, producciones)
      }
    }

    def recorrerIJ(i: Int, j: Int, X: List[List[Set[Char]]], producciones: Set[Produccion], k: Int): Set[Char] = {

      if (k == j) {
        Set()
      } else {

        val Xikj = producciones.filter((p: Produccion) => X(k - i)(i).contains(p.cadena.head) && X(j - (k + 1))(k + 1).contains(p.cadena.tail.head)).map(_.variable)
        Xikj ++ recorrerIJ(i, j, X, producciones, k + 1)
      }
    }
    def casoInductivo(X: List[List[Set[Char]]], producciones: Set[Produccion], j: Int, cadenaLen: Int, i: Int = 0): List[Set[Char]] = {

      if (j == cadenaLen) {
        List()
      } else {
        val Xij = recorrerIJ(i, j, X, producciones, i)
        Xij :: casoInductivo(X, producciones, j + 1, cadenaLen, i + 1)

      }

    }

    def recursiva(X: List[List[Set[Char]]], cadena: String, producciones: Set[Produccion], nivel: Int = 1): List[List[Set[Char]]] = {
      if (nivel == cadena.size) {
        X
      } else {
        val fila = casoInductivo(X, producciones, nivel, cadena.size)

        recursiva(X ::: List(fila), cadena, producciones, nivel + 1)
      }

    }

    val paso0 = List(casoBase(cadena, g.producciones))

    val p = g.producciones.filter(_.cadena.size > 1)

    val resultado = recursiva(paso0, cadena, p)
    resultado.foreach { println }
    resultado

  }

  def perteneceA(cadena: String, g: Gramatica): Boolean = {
    if (cadena == "") {
      false
    } else {
      val matriz = generarMatriz(cadena, g)
      matriz.reverse.head.head.contains(g.inicial)
    }

  }

  def main(args: Array[String]): Unit = {
    val g = Importador.importarGramatica("Input/cyk_input")
    println("Sea G: ")
    println(g)
    print("La palabra abab pertenece a g? " + perteneceA("abab", g))
  }
}