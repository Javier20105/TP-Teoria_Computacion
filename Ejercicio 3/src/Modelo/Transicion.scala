package Modelo

case class Transicion(e: Estado, v: Char, s: Estado) {
  val estadoEntrada = e
  val variable = v
  val estadoSalida = s
  override def toString = { s"\n[$e, $v -> $s]" }

  def esEpsilon(): Boolean = { variable == 'E' }
  def esVar(c: Char): Boolean = {variable == c}
  def clausuraEpsi(e: Estado): Boolean = { this.esEpsilon() && !this.estadoEntrada.id.&(e.id).isEmpty }
}