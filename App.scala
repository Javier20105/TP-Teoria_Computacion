import Modelo.Gramatica
import Modelo.Produccion

object App {
  
  def main(args:Array[String]):Unit ={
    
    val v = 'A'
    val inicial = 'S'
    val p =  new Produccion(v,"S")
    val p1 = new Produccion(inicial, "A")
    val terminales = Set('a')
    val variables = Set(v)
    val producciones = Set(p,p1)
    
    val gramatica = new Gramatica(terminales,variables, inicial, producciones)
       
  }
}