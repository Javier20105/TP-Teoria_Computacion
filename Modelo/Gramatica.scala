package Modelo

class Gramatica(t: Set[Char],  vars: Set[Char],ini: Char, prod: Set[Produccion]) {
  val terminales = t
  val variables = vars
  val inicial = ini
  val producciones = prod

}