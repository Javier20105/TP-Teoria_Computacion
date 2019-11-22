package Modelo

class Automata(t: Set[Char], v: Set[Estado], f: Set[Set[String]], tr: Set[Transicion]) {
  val alfabetoInput = t
  val estados = v
  val estadosFinales = f
  val transiciones = tr
  override def toString = s"Automata: \nAlfabeto input:  $t \nEstados: $v \nEstados finales: $f \nTransiciones: $tr"
  
  //def esFinal(e: Set[String]): Boolean = {estadosFinales.contains(e)}
}