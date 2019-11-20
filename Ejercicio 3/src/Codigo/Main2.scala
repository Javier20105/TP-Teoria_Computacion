package Codigo
import Modelo.Automata2
import Modelo.Transicion2
import Modelo.Estado
import scala.io.Source
import java.io._

object Main2 {

  def importarAutomata(ruta: String): Automata2 = {
   /**
   * Recibe transiciones en Strings con formato "1, a -> 2" y crea un set de Transiciones.
   */
    def crearTransiciones(s: Array[String], e: Set[Estado]): Set[Transicion2] = {
      if (s.size == 0) {
        Set()
      } else {
        val partesTransicion = s(s.size - 1).split(" -> ")
        val partes = partesTransicion(0).split(", ") 
        crearTransiciones(s.take(s.size - 1),e) + new Transicion2(getEstado(partes(0),e), partes(1).charAt(0), getEstado(partesTransicion(1),e))
        //crearTransiciones(s.take(s.size - 1),e) + new Transicion2(e.find(_.id.equals(partes(0))).get, partes(1).charAt(0),e.find(_.id.equals(partesTransicion(1))).get)
      }
    }
    def getEstado(s: String, e: Set[Estado]): Estado = {
      e.find(_.id.contains(s)).get
    }
    
    def crearEstadosOBJ(cant: Int, estadosF: Set[Set[String]], acum: Set[Estado]/*, tr: Set[Transicion2]*/): Set[Estado] = {
      if(cant<=0){
        Set()
      } else {
        val estadoNombre = Set() + cant.toString
        val estaEnFinales = esFinal(estadoNombre,estadosF)
        val estaComoInicial = esInicial(estadoNombre)
        val nuevoEstado = new Estado(estadoNombre,estaEnFinales,estaComoInicial/*,transicionesDe(estadoNombre,tr)*/)
        crearEstadosOBJ(cant-1, estadosF, acum/*,tr*/) + nuevoEstado
      }
    }
    
    val lineas = Source.fromFile(ruta).getLines()
    val alfabetoInput = lineas.next.split(", ").flatten.toSet
    val cantEstados = lineas.next.toInt
    val estadosFinales = lineas.next.split(", ").toSet
    val estadosFinalesSet = estadosFinales.map(Set()+_)
    val estados = crearEstadosOBJ(cantEstados, estadosFinalesSet,Set.empty[Estado]/*, transiciones*/)
    val transiciones = crearTransiciones(lineas.toArray, estados)
    new Automata2(alfabetoInput, estados, estadosFinalesSet, transiciones)
  }

// REVISAR ESTOOO!!!!!!!!!!!!!!!!!!!! EL INICIAL NO LO DETECTA
  // recibe 1 Estado.id y el Set[String] de estadosFinales
  def esFinal(uno: Set[String], eFin: Set[Set[String]]): Boolean = {
    !eFin.filter(!uno.&(_).isEmpty).isEmpty
  }
  // recibe Estado.id
  def esInicial(e: Set[String]): Boolean = {
    e.contains("1")
  }
  
  // EN DESARROLLO
  def transformer(a: Automata2): Automata2 = {
    val estadoInicial = Set() + clausuraEpsilonOBJ(a.estados.find(_.id.contains("1")).get,a.transiciones,a.estadosFinales)
    // variables necesarias para el new Automata
    val alfabetoInput = a.alfabetoInput
    val estados = estadoInicial ++ creadorGeneral(Set(),estadoInicial,a)
    val estadosFinales = establecerFinales(estados)//Set.empty[Set[String]]
    val transiciones = generadorDeTransicionesAFD(a.transiciones, estados)
    new Automata2(alfabetoInput, estados, estadosFinales, transiciones)
  }
  ////////////////////////////////////////////////////////////////////////////////////////////////////////
  def generadorDeTransicionesAFD(tr: Set[Transicion2], e: Set[Estado]): Set[Transicion2] = {
    e.map(transicionesDe(_,eliminarEpsilons(tr))).flatten
    //transicionesSinAgrupar.map(f)
  }
  def eliminarEpsilons(tr: Set[Transicion2]): Set[Transicion2] = {
    tr.filter(!_.esEpsilon())
  }/*
  def actualizarTransiciones(tr: Set[Transicion2]): Set[Transicion2] = {
    
  }*/
  
  ////////////////////////////////////////////////////////////////////////////////////////////
  def creadorGeneral(ret: Set[Estado],cont: Set[Estado], a: Automata2): Set[Estado] = {
    if(cont.isEmpty){
      ret
    } else {
      val acum = ret ++ creadorDeEstadosAFD(destinos(cont.head,a))
      creadorGeneral(acum,cont.tail,a)
    }
  }
  def creadorDeEstadosAFD(set: Set[Set[Estado]]): Set[Estado] = {
    set.map(unirEstados(_))
  }
  def unirEstados(a: Set[Estado]): Estado = {
    val estadoNombre = a.flatMap(_.id)
    val estaEnFinales = !a.filter(_.esFinal).isEmpty
    val estaComoInicial = !a.filter(_.esInicial).isEmpty
    new Estado(estadoNombre,estaEnFinales,estaComoInicial/*,transicionesDe(estadoNombre,tr)*/)
  }
  ////////////////////////////////////////////////////////////////////////////////////////////
  
  
  
  // A partir de un estado y un automata, devuelve Todos los destinos,
  // itera todos los inputs de abcedario y le calcula todos los destinos ya con clausuras
  def destinos(estado: Estado, a: Automata2): Set[Set[Estado]] = {
    // revisar, deberia acumular los de cada uno de los estados dentro del estado.
    a.alfabetoInput.map(i => destinosVar(i,transicionesDe(estado,a.transiciones),Set(),a))    
  }
  /**
   * Recibe 2 parametros.
   * <li>param1 - String = Estado del cual se desean las transiciones
   * <li>param2 - Set[Transicion] = Todas las transiciones del automata
   * 
   * Retorna un Set[Transicion] que contiene todas las transiciones en las cuales 'param1' es el estadoEntrada
   */
  def transicionesDe(e: Estado, tr: Set[Transicion2]): Set[Transicion2] = {
    tr.filter(!_.estadoEntrada.id.&(e.id).isEmpty)
  }
  /**
   * Recibe 4 parametros, definitivamente necesitaría reformatearse todo para que sea mas simple.
   * <li>param1 - Char = Es el input del abcedario a testear
   * <li>param2 - Set[Transicion] = Transiciones del estadoEntrada al cual se le queire testear el Char (param1)
   * <li>param3 - Set[String] = Se acumula el resultado (ingresarlo vacío o con valores para que se le agreguen nuevos)
   * <li>param4 - Automata = El automata en el cual existen los valores anteriores.
   * meterle como parametro la union de las transiciones de los estados
   */
  def destinosVar(v: Char, tr: Set[Transicion2], actual: Set[Estado], a:Automata2): Set[Estado] = {
    if(tr.size<=0){
      //println("base: "+ actual)
      actual
    } else {
      if(tr.head.esVar(v)){
        val acum = actual + clausuraEpsilonOBJ(tr.head.estadoSalida,transicionesDe(tr.head.estadoSalida,a.transiciones),a.estadosFinales)
        //println("acum: "+acum)
        destinosVar(v,tr.tail,acum,a)
      } else {
        //println("actual: "+ actual)
        destinosVar(v,tr.tail,actual,a)
      }
    }
  }
  
  
  
  ////////////////////////////////////////////////////////////////////////////////////////////
  // ClausuraEpsilon (tmb aplica el esFinal/esInicial) y devuelve el Estado
  def clausuraEpsilonOBJ(e: Estado, tr: Set[Transicion2], estadosF: Set[Set[String]]) : Estado = {
    val id = e.id ++ unionEstadosSalida(tr.filter(_.clausuraEpsi(e)))
    /*
    if(id.contains("1")){
      println("id: "+id)
      println("estados: "+estadosF)
      println("esFinal: "+esFinal(id,estadosF))
    }*/
    val eFinal = esFinal(id,estadosF)
    val eInicial = esInicial(id)
    
    //hacer vals y devolver tuplas Estado, Transicion2
    new Estado(id, eFinal, eInicial)
  }
  // auxiliar para clausuraEpsilonOBJ
  def unionEstadosSalida(tr: Set[Transicion2]): Set[String] = {
    if(tr.size == 0){
      Set()
    } else {
      tr.head.estadoSalida.id ++ unionEstadosSalida(tr.tail)
    }
  }
  ////////////////////////////////////////////////////////////////////////////////////////////
  
  
  // comentado la lista de finales porque deberían de estar ya definidos como finales o iniciales al crearse.
  def establecerFinales(e: Set[Estado]/*,ef: Set[Set[String]]*/): Set[Set[String]] = {
    e.filter(_.esFinal).map(_.id)
  }
  
  // innecesario, pero era simple de implementar por si hacia falta, como muchos otros metodos jajaja
  def establecerIniciales(e: Set[Estado]/*,ef: Set[String]*/): Set[Set[String]] = {
    e.filter(_.esInicial).map(_.id)
  }
  
  def main(args: Array[String]): Unit = {
    val gra = importarAutomata("Input/inputEj3")
    println("***************Automata AFND-Epsilon*************")
    println(gra)
    println("***************Automata AFD*************")
    println(transformer(gra))
    println("***************Tests individuales*************")/*
    println("Clausura del 1: " + clausuraEpsilon(true,transicionesDe("1",gra.transiciones)))
    //println("Crear clausuras: " + crearClausurasEpsilon(gra.estados, gra.transiciones))
    println("DestinosVar: "+destinosVar('a', transicionesDe("1",gra.transiciones),Set(),gra))
    println("Destinos: "+destinos("1",gra))
    println("Transiciones del 1: " + transicionesDe("1",gra.transiciones))*/
    //println("Ignorar: "+crearEstadosAFD(gra))
    /*
    val set1 = Set("3","5","4")
    val set2 = Set("4","5")
    val eFin = Set("1","2","3")
    println("Esta contenido? " + estaContenido(set1,set2))
    println("Contenedor: " + contenedor(set1,set2))
    println("Crear Estados: " + crearEstadosOBJ(10, eFin, Set()))*/
    val estado = gra.estados.find(_.id.contains("1")).get
    val estado2 = gra.estados.find(_.id.contains("2")).get
    val setEstados = Set() + estado + estado2
    println("Find Estado 1: "+estado)
    println("ClausuraEpsilon de 1: "+clausuraEpsilonOBJ(estado, gra.transiciones,gra.estadosFinales))
    
    println("destinos del estado de 1: \n"+destinos(estado,gra))
    
    println("destino de 'a': "+destinosVar('a', transicionesDe(estado,gra.transiciones), Set(), gra))
    
    println("Eliminar epsilons: "+eliminarEpsilons(gra.transiciones))

    
    //println("id resultado de la union de los Estados 1 y 2: "+setEstados.flatMap(_.id))
    
    // clausura del inicial
    // destinosvar, que me acumule en un solo estado los del mismo var(char)
    //clausura
    //heredarle las transiciones
    
  } 
  
  /* Obsoletos?
  def clausuraEpsilon(e: String, tr: Set[Transicion]): (Int, Set[String]) = {
    val eInt = e.toInt
    (eInt, tr.filter(_.clausuraEpsi(e)).map(_.estadoSalida) + e)
  }

  def crearClausurasEpsilon(est: Set[String], tr: Set[Transicion]): Set[(Int, Set[String])] = {
    est.map(clausuraEpsilon(_, tr)).toSet
  }
  
  /**
   * Solo funciona para estadoEntrada (True)
   * Se supone es para ambos, pero solo funciona bien con True, el False no me fijé bien, pero no funciona.
   */
  def clausuraEpsilon(deEstadoEntrada: Boolean, tr: Set[Transicion2]): Set[String] = {
    if(tr.isEmpty){
      Set()
    } else {
      if(deEstadoEntrada){
        val e = tr.head.estadoEntrada
        tr.filter(_.clausuraEpsi(e)).map(_.estadoSalida.id) + e
      } else {
        val e2 = tr.head.estadoSalida
        tr.filter(_.clausuraEpsi(e2)).map(_.estadoSalida.id) + e2
      }
    }
  }
  
  
  Usar este metodo para especificar las transiciones de qué Estado se quieren las clausuras (no se borró porque el otro no funciona bien)
   
  def clausuraEpsilon(e: Estado, tr: Set[Transicion2]): Set[String] = {
    tr.filter(_.clausuraEpsi(e)).map(_.estadoSalida.id) + e.id
  }
  */
  
  def estaContenido(a: Set[String], b: Set[String]): Boolean = {
    a.diff(b).isEmpty// || b.diff(a).isEmpty 
  }
  
  // Solo usar si estaContenido == true, si es false se maneja igual y devuelve vacío
  def contenedor(a: Set[String], b: Set[String]): Set[String] = {
    if(a.diff(b).isEmpty){
      b
    } else if(b.diff(a).isEmpty) {
      a
    } else {
      Set()
    }
  }
  
  /* revisar
  def limpiar(b: Set[String], a: Set[Set[String]]): Boolean = {
    a.map(estaContenido(_,b))
  }
  
  def limpiarContenidos(a: Set[Set[String]]): Set[Set[String]]={
    a.filter(limpiar(_,a))
  }
  
  
  // este es para testear, no hace falta tenerlo aca afuera del importarAutomata.
  def crearEstadosOBJ(cant: Int, estadosF: Set[String], acum: Set[Estado]/*, tr: Set[Transicion2]*/): Set[Estado] = {
    if(cant<=0){
      Set()
    } else {
      val estadoNombre = Set() + cant.toString
      val estaEnFinales = !(estadosF.&(estadoNombre).isEmpty)
      val estaComoInicial = estadoNombre.contains("1")
      val nuevoEstado = new Estado(estadoNombre,estaEnFinales,estaComoInicial/*,transicionesDe(estadoNombre,tr)*/)
      crearEstadosOBJ(cant-1, estadosF, acum/*,tr*/) + nuevoEstado
    }
  }
  */  
}