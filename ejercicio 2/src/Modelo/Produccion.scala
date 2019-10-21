package Modelo

case class Produccion(v: Char, c: String) {
  val variable = v
  val cadena = c
  override def toString = { s"$variable -> $cadena" }

  def esEpsilon(): Boolean = { cadena == "ε" }

  def esUnitaria(): Boolean = { 
    cadena.size == 1 && cadena.charAt(0) >= 65 && cadena.charAt(0) <= 90 
    }

}