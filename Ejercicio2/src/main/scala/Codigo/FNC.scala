package Codigo

import Modelo.Gramatica
import Modelo.Produccion

object FNC {
  def crearProduccionesTerminales(g:Gramatica): (Set[Produccion], List[Char]) = {

    def recursiva(g:Gramatica,terminales:Set[Char], sinUsar:List[Char],producciones:Set[Produccion] = Set()): (Set[Produccion], List[Char]) = {
      if (terminales.size == 0) {
        (producciones, sinUsar)
      } else {
        val p = g.producciones.filter(_.cadena == terminales.head + "").size match {
          case 0 => new Produccion(sinUsar.head, terminales.head + "")
          case _ => g.producciones.filter(_.cadena == terminales.head + "").head
        }
        recursiva(g,terminales.tail, sinUsar.diff(List(p.variable)), producciones + p)
      }
    }

    recursiva(g,g.terminales, (('A' to 'Z').toList  ::: List('Ñ')).diff(g.variables.toList))

  }



  def reemplazarTerminales(producciones: Set[Produccion], produccionesTerminales: Set[Produccion]): Set[Produccion] = {
    def quitarTerminalesDeProduccion(p: Produccion, produccionesTerminales: Set[Produccion]): Produccion = {
      if (p.cadena.size == 1) {
        p
      } else {
        new Produccion(p.variable, p.cadena.map((c: Char) => {
          val v = produccionesTerminales.filter(_.cadena.contains(c))
          if (v.size == 0) {
            c
          } else
            v.head.variable
        }))

      }
    }

    if (producciones.size == 0) {
      Set()
    } else {
      reemplazarTerminales(producciones.tail, produccionesTerminales) + quitarTerminalesDeProduccion(producciones.head, produccionesTerminales)
    }
  }

  def reducirProducciones(producciones: Set[Produccion], sinUsar: List[Char]): Set[Produccion] = {
    def reducir(p: Produccion, sinUsar: List[Char], producciones: Set[Produccion] = Set()): (Set[Produccion], List[Char]) = {

      if (p.cadena.size == 1) {
        (Set() + p, sinUsar)

      } else {
        if (p.cadena.size == 2) {
          (producciones + p, sinUsar)
        } else {
          val pref = new Produccion(p.variable, p.cadena.head + "" + sinUsar.head + "")
          val suf = new Produccion(sinUsar.head, p.cadena.tail)
          reducir(suf, sinUsar.tail, (producciones + pref))
        }
      }
    }
    if (producciones.size == 0) {
      Set()
    } else {

      val (p, s) = reducir(producciones.head, sinUsar)
      p ++ reducirProducciones(producciones.tail, s)

    }
  }

  def convertir(g: Gramatica): Gramatica = {

    println("sin usar:" +  (('A' to 'Z').toList ::: List() ::: List('Ñ')).diff(g.variables.toList))
    val (produccionesTerminales, sinUsar) = crearProduccionesTerminales(g)
    val produccionesSinTerminales = reemplazarTerminales(g.producciones, produccionesTerminales) ++ produccionesTerminales
    val produccionesFNC = reducirProducciones(produccionesSinTerminales, sinUsar)

    println("")
    println("produccionesTerminales: " + produccionesTerminales)
    println("producciones sin terminales. " + produccionesSinTerminales)
    println("producciones en FNC: " + produccionesFNC)
    Gramatica.actualizarVariables(new Gramatica(g.terminales, g.variables, g.inicial, produccionesFNC))

  }

  def main(args: Array[String]): Unit = {
    val c = convertir(Importador.importarGramatica("Input/fnc_input"))
    println("En fnc: " + c)
  }
}