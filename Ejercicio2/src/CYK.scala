import Modelo.Gramatica
import Modelo.Produccion

object CYK {

  def generarMatriz(cadena: String, g: Gramatica): List[List[List[Char]]] = {

    def casoBase(cadena: String, producciones: Set[Produccion]): List[List[Char]] = {

      if (cadena.size == 0) {
        List()
      } else {
        val r = producciones.toList.filter((p: Produccion) => p.cadena.head == cadena.head && p.cadena.size == 1).map(_.variable)

        r :: casoBase(cadena.tail, producciones)
      }
    }

    def recorrerIJ(i: Int, j: Int, X: List[List[List[Char]]], producciones: Set[Produccion], k: Int): List[Char] = {

      if (k == j) {
        List()
      } else {
        /*println("I: " + i + " K: " + k + " J:" + j)
        println("X" + i + k + ":" + X(k - i)(i))
        println("X" + (k + 1) + j + ": " + X(j - (k + 1))(k + 1))
        println("")*/
        val Xikj = producciones.toList.filter((p: Produccion) => X(k - i)(i).contains(p.cadena.head) && X(j - (k + 1))(k + 1).contains(p.cadena.tail.head)).map(_.variable)
        Xikj ::: recorrerIJ(i, j, X, producciones, k + 1)
      }
    }
    def casoInductivo(X: List[List[List[Char]]], producciones: Set[Produccion], j: Int, cadenaLen: Int, i: Int = 0): List[List[Char]] = {
      //println("I: " + i + " J:" + j)
      if (j == cadenaLen) {
        List()
      } else {
        val Xij = recorrerIJ(i, j, X, producciones, i)
        Xij :: casoInductivo(X, producciones, j + 1, cadenaLen, i + 1)

      }

    }

    def recursiva(X: List[List[List[Char]]], cadena: String, producciones: Set[Produccion], nivel: Int = 1): List[List[List[Char]]] = {
      if (nivel == cadena.size) {
        X
      } else {
        val fila = casoInductivo(X, producciones, nivel, cadena.size)
        /*println("")
        println("")
        println("valor fila actual: " + fila)*/
        recursiva(X ::: List(fila), cadena, producciones, nivel + 1)
      }

    }

    val paso0 = List(casoBase(cadena, g.producciones))
   /* println("Paso 0: " + paso0)
    println("Coordenada 0 0 :  " + paso0(0)(0))*/
    val p = g.producciones.filter(_.cadena.size > 1)
    /*println("producciones no terminales: " + p)
    println("Caso Inductivo: " + casoInductivo(paso0, p, 1, cadena.size))
    println("")
    println("")
    println("")
    println("-------------------------------------------------------------------------")
    println("")
    println("")
    println("Resultado: ")*/
    val resultado = recursiva(paso0, cadena, p)
    resultado.foreach { println }
    resultado

  }

  def perteneceA(cadena:String, g: Gramatica): Boolean = {
    if(cadena == ""){
      false
    }else{
    val matriz = generarMatriz(cadena, g)
    matriz.reverse.head.head.contains(g.inicial)}

  }

  def main(args: Array[String]): Unit = {
    val g = Importador.importarGramatica("Input/cyk")
    println("Sea G: ")
    println(g)
    print("La palabra abab pertenece a g? " + perteneceA("weqwe", g))
  }
}