package Modelo

class Produccion(v:Char, c:String) {
  val variable = v
  val cadena = c
  override def toString = s"$variable -> $cadena"

  
}