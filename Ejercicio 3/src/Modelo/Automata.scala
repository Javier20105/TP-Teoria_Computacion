package Modelo

case class Automata(t: Set[Char], v: Set[Estado], f: Set[Set[String]], tr: Set[Transicion]) {
  val alfabetoInput = t
  val estados = v
  val estadosFinales = f
  val transiciones = tr
  override def toString = s"Automata: \nAlfabeto input:  $t \nEstados: $v \nEstados finales: $f \nTransiciones: $tr"
  
  //def esFinal(e: Set[String]): Boolean = {estadosFinales.contains(e)}
  
  def procesar(s: String): Boolean = {
    
    def procesando(s: String,ret: Boolean, index: Int, pos: Estado): Boolean = {
      if(s.length()<=index || !ret){
        ret
      } else {
        val charActual = s.charAt(index)
        if(hayTransicionPosible(pos,charActual)){
          val newpos = transicionesDeExacta(pos,charActual).head.estadoSalida
          procesando(s,ret,index+1,newpos)
        } else {
          val newret = false
          procesando(s,newret,index+1,pos)
        }
      }
    }
    
    procesando(s,true,0,estados.find(_.id.contains("1")).get)
  }
  
  def hayTransicionPosible(e: Estado, c: Char): Boolean = {
    !transicionesDeExacta(e,c).isEmpty
  }
  // solo debería devolver 1 Transicion
  def transicionesDeExacta(e: Estado, c: Char): Set[Transicion] = {
    transiciones.filter(t => t.estadoEntrada==e && t.variable == c && t.estadoSalida!=new Estado(Set(),false,false))
  }  
  
}