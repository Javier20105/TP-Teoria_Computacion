package Modelo

class Gramatica(t: Set[Char], vars: Set[Char], ini: Char, prod: Set[Produccion]) {
  val terminales = t
  val variables = vars
  val inicial = ini
  val producciones = prod
  override def toString = s"Terminales:  $terminales \nVariables: $variables \nVariable inicial: $inicial \nProducciones: $producciones" 


}

object Gramatica {
  def quitarTerminalesSinUsar(g: Gramatica): Gramatica = {
    val terminalesUsados = Set() ++ g.producciones.map((p: Produccion) => p.cadena.toList.intersect(g.terminales.toList).toSet).flatten
    new Gramatica(terminalesUsados, g.variables, g.inicial, g.producciones)
  }

  def actualizarVariables(g: Gramatica): Gramatica = {
    val variablesUsadas = g.producciones.map((p: Produccion) => p.variable) + g.inicial
    new Gramatica(g.terminales, variablesUsadas, g.inicial, g.producciones)
  }
}