package Modelo

case class Produccion(v: Char, c: String) {
  val variable = v
  val cadena = c.filter(_ != ' ')
  override def toString = { s"$variable->$cadena" }

  def esEpsilon(): Boolean = { cadena == "ε" }

  def esUnitaria(): Boolean = { 
    cadena.size == 1 && cadena.charAt(0) >= 65 && cadena.charAt(0) <= 90 
    }

}

object Produccion{
  def apply(c:Char,s:String):Produccion = new Produccion(c,s)
}