package Codigo

import Modelo.Gramatica
import Modelo.Produccion

object Limpiador {

  def procesoRecursivo[A](anterior:Set[A],actual:Set[A], producciones:Set[Produccion], casoInductivo: (Set[A], Set[Produccion]) => Set[A] ):Set[A] = {
    if (anterior == actual) {
      return actual
    } else {
      val siguiente = casoInductivo(actual, producciones)
      procesoRecursivo(actual, siguiente, producciones,casoInductivo)
    }
  }

  def descubrirNulleables(producciones: Set[Produccion]): Set[Char] = {

    def esNulleable(p: Produccion, n: Set[Char]): Boolean = {
      p.cadena.map(n.contains(_)).fold(true)(_ && _)
    }
    def casoBase(producciones: Set[Produccion]): Set[Char] = {
      producciones.filter(_.esEpsilon).map(_.variable)
    }
    def casoInductivo(nulleables: Set[Char], producciones: Set[Produccion]): Set[Char] = {
      nulleables ++ producciones.filter(esNulleable(_, nulleables)).map(_.variable)
    }
    /*def recursiva(anterior: Set[Char], actual: Set[Char], producciones: Set[Produccion]): Set[Char] = {
      if (anterior == actual) {
        return actual
      } else {
        val siguiente = casoInductivo(actual, producciones)
        recursiva(actual, siguiente, producciones)
      }
    }*/
    val paso0 = casoBase(producciones)
    val paso1 = casoInductivo(paso0, producciones)
    //recursiva(paso0, paso1, producciones)
    procesoRecursivo[Char](paso0, paso1, producciones,casoInductivo)
  }

  def eliminarProduccionesEpsilon(g: Gramatica): Gramatica = {

    def encontrarPrimero(s: String, c: Char): Int = {
      s.indexOf(c)
    }
    def quitar(s: String, index: Int): String = {
      s.slice(0, index) + s.slice(index + 1, s.size)
    }
    def nulleablesEnProduccion(p: Produccion, c: List[Char]): List[Char] = {
      p.cadena.toList.filter(c.contains(_))
    }
    def recursiva(p: Produccion, nulleables: List[Char], index: Int = 0): Set[Produccion] = {
      if (nulleables.size == 0) {
        Set(p)
      } else {
        val n = nulleables.tail
        val i = index + encontrarPrimero(p.cadena.slice(index, p.cadena.size), nulleables.head)
        recursiva(p, n, i) ++ recursiva(new Produccion(p.variable, quitar(p.cadena, i)), n, i)
      }
    }

    val nulleables = descubrirNulleables(g.producciones)
    val resul = g.producciones.map((p:Produccion) => recursiva(p, nulleablesEnProduccion(p, nulleables.toList)))
    Gramatica(g.terminales, g.variables, g.inicial, resul.flatten.filter(_.cadena != "ε").filter(_.cadena != ""))
  }

  def crearParesUnitarios(g: Gramatica): Set[(Char, Char)] = {
    def casoBase(v: Set[Char]): Set[(Char, Char)] = {
      v.map((c: Char) => (c, c))
    }
    def casoInductivo(pares: Set[(Char, Char)], unitarias: Set[Produccion]): Set[(Char, Char)] = {
      if (unitarias.size == 0) {
        return pares
      } else {
        val p = unitarias.head
        val r = pares.filter(_._2 == p.variable)
        pares ++ r.map((c: (Char, Char)) => (c._1, p.cadena.charAt(0))) ++ casoInductivo(pares, unitarias.tail)
      }
    }
    /*def recursiva(anterior: Set[(Char, Char)], actual: Set[(Char, Char)], unitarias: Set[Produccion]): Set[(Char, Char)] = {
      if (anterior == actual) {
        actual
      } else {
        val siguiente = casoInductivo(actual, unitarias)
        recursiva(actual, siguiente, unitarias)
      }
    }*/
    val unitarias = g.producciones.filter(_.esUnitaria())
    val paso0 = casoBase(g.variables)
    val paso1 = casoInductivo(paso0, unitarias)
    //recursiva(paso0, paso1, unitarias)
    procesoRecursivo[(Char,Char)](paso0, paso1, unitarias,casoInductivo)

  }

  def eliminarProduccionesUnitarias(g: Gramatica): Gramatica = {
    def recursiva(paresUnitarios: Set[(Char, Char)], peoduccionesNoUnitarias: Set[Produccion]): Set[Produccion] = {
      if (paresUnitarios.size == 0) {
        peoduccionesNoUnitarias
      } else {
        val actual = paresUnitarios.head
        val prodParaAgregar = peoduccionesNoUnitarias.filter(_.variable == actual._2).map((pro: Produccion) => new Produccion(actual._1, pro.cadena))
        prodParaAgregar ++ recursiva(paresUnitarios.tail, peoduccionesNoUnitarias)
      }
    }
    val paresUnitarios = crearParesUnitarios(g)
    val noUnitarias = g.producciones.filter(!_.esUnitaria())
    Gramatica(g.terminales, g.variables, g.inicial, recursiva(paresUnitarios, noUnitarias))
  }

  def descubrirGeneradores(g: Gramatica): Set[Char] = {

    def casoBase(terminales: Set[Char]): Set[Char] = {
      terminales
    }

    def casoInductivo(generadores: Set[Char], producciones: Set[Produccion]): Set[Char] = {
      generadores ++ producciones.filter((p: Produccion) => (Set() ++ p.cadena).subsetOf(generadores)).map((_.variable))
    }

    /*def recursiva(anterior: Set[Char], actual: Set[Char], producciones: Set[Produccion]): Set[Char] = {
      if (anterior == actual) {
        return actual
      } else {
        val siguiente = casoInductivo(actual, producciones)
        recursiva(actual, siguiente, producciones)
      }
    }*/
    val paso0 = casoBase(g.terminales)
    val paso1 = casoInductivo(paso0, g.producciones)
    //recursiva(paso0, paso1, g.producciones)
    procesoRecursivo[Char](paso0, paso1, g.producciones,casoInductivo)


  }

  def eliminarNoGeneradores(g: Gramatica): Gramatica = {
    val generadores = descubrirGeneradores(g)
    val p = g.producciones.filter((p: Produccion) => ((Set() + p.variable) ++ p.cadena) -- generadores == Set())
    Gramatica(g.terminales, g.variables, g.inicial, p)
  }

  def descubrirAlcanzables(g: Gramatica): Set[Char] = {

    def casoBase(inicial: Char): Set[Char] = {
      Set() + inicial
    }

    def casoInductivo(alcanzables: Set[Char], producciones: Set[Produccion]): Set[Char] = {
      alcanzables ++ producciones.filter((p: Produccion) => alcanzables.contains(p.variable)).map(_.cadena.toList).flatten
    }

    /*def recursiva(anterior: Set[Char], actual: Set[Char], producciones: Set[Produccion]): Set[Char] = {
      if (anterior == actual) {
        return actual
      } else {
        val siguiente = casoInductivo(actual, producciones)
        return recursiva(actual, siguiente, producciones)
      }
    }*/

    val paso0 = casoBase(g.inicial)
    val paso1 = casoInductivo(paso0, g.producciones)
    //recursiva(paso0, paso1, g.producciones)
    procesoRecursivo[Char](paso0, paso1, g.producciones,casoInductivo)


  }

  def eliminarNoAlcanzables(g: Gramatica): Gramatica = {
    val alcanzables = descubrirAlcanzables(g)
    val p = g.producciones.filter((p: Produccion) => ((Set() + p.variable) ++ p.cadena) -- alcanzables == Set())
    new Gramatica(g.terminales, g.variables, g.inicial, p)
  }

  def limpiar(gra: Gramatica): Gramatica = {

    //val nulleables = descubrirNulleables(gra.producciones)
    val sinEpsilon = eliminarProduccionesEpsilon(gra)
    //val paresUnitarios = crearParesUnitarios(sinEpsilon)
    val sinUnitarias = eliminarProduccionesUnitarias(sinEpsilon)
    //val simbolosGeneradores = descubrirGeneradores(sinUnitarias)
    val sinNoGeneradores = eliminarNoGeneradores(sinUnitarias)
    //val simbolosAlcazables = descubrirAlcanzables(sinUnitarias)
    val sinNoAlcanzables = eliminarNoAlcanzables(sinNoGeneradores)
    val limpia = Gramatica.actualizarVariables(Gramatica.quitarTerminalesSinUsar(sinNoAlcanzables))

    println("Gramatica: ")
    println(gra)
    println()
    //println("Nulleables: " + nulleables)
    println()
    println("Sin epsilon: ")
    println(sinEpsilon)
    println()
    //println("pares unitarios:" + paresUnitarios)
    println()
    println("Sin Producciones unitarias:")
    println()
    println(sinUnitarias)
    println()
    println("Simbolos generadores:")
    println()
    //println(simbolosGeneradores)
    println()
    println("Sin no generadores:")
    println()
    println(sinNoGeneradores)
    println()
    println("Simbolos Alcanzables")
    println()
    //println(simbolosAlcazables)
    println()
    println("Sin no alcanzables")
    println()
    println("Limpia: " + limpia)
    println()

   

    limpia
  }

  def main(args: Array[String]): Unit = {
    val gra = Importador.importarGramatica("Input/input")
    limpiar(gra)

  }
}