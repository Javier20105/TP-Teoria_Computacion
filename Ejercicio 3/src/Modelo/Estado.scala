package Modelo
// Solo era una idea, no esta en uso
case class Estado(estado: Set[String], esF: Boolean, esI: Boolean/*, tr: Set[Transicion2]*/) {
  val id = estado
  val esFinal = esF
  val esInicial = esI
  //val transiciones = tr
  //override def toString = s"\n{Estado: $id, Inicial: $esI, Final: $esF}"
  override def toString = s"$id"
  def toStringFULL = s"\n{Estado: $id, Inicial: $esI, Final: $esF}"
  def equals(e: Estado) = this.id.equals(e.id)
}