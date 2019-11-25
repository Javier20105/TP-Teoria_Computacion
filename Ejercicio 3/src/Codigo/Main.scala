package Codigo
import Modelo.Automata
import Modelo.Transicion
import Modelo.Estado
import scala.io.Source
import java.io._

object Main2 {

  def importarAutomata(ruta: String): Automata = {
   /**
   * Recibe transiciones en Strings con formato "1, a -> 2" y crea un set de Transiciones.
   */
    def crearTransiciones(s: Array[String], e: Set[Estado]): Set[Transicion] = {
      if (s.size == 0) {
        Set()
      } else {
        val partesTransicion = s(s.size - 1).split(" -> ")
        val partes = partesTransicion(0).split(", ") 
        crearTransiciones(s.take(s.size - 1),e) + new Transicion(getEstado(partes(0),e), partes(1).charAt(0), getEstado(partesTransicion(1),e))
        //crearTransiciones(s.take(s.size - 1),e) + new Transicion(e.find(_.id.equals(partes(0))).get, partes(1).charAt(0),e.find(_.id.equals(partesTransicion(1))).get)
      }
    }
    def getEstado(s: String, e: Set[Estado]): Estado = {
      e.find(_.id.contains(s)).get
    }
    
    def crearEstadosOBJ(cant: Int, estadosF: Set[Set[String]], acum: Set[Estado]/*, tr: Set[Transicion]*/): Set[Estado] = {
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
    new Automata(alfabetoInput, estados, estadosFinalesSet, transiciones)
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
  def transformer(a: Automata): Automata = {
    val estadoInicial = Set() + clausuraEpsilonOBJ(a.estados.find(_.id.contains("1")).get,a.transiciones,a.estadosFinales)
    // variables necesarias para el new Automata
    val alfabetoInput = a.alfabetoInput
    val transiciones = creadorGeneral(Set(),estadoInicial,a)
    val estados = estadoInicial ++ transiciones.map((t:Transicion) => t.estadoSalida)
    val estadosFinales = establecerFinales(estados)//Set.empty[Set[String]]
    //val transiciones = generadorDeTransicionesAFD(a.transiciones, estados)
    new Automata(alfabetoInput, estados, estadosFinales, transiciones)
  }
  ////////////////////////////////////////////////////////////////////////////////////////////////////////
  def generadorDeTransicionesAFD(tr: Set[Transicion], e: Set[Estado]): Set[Transicion] = {
    e.map(transicionesDe(_,eliminarEpsilons(tr))).flatten
    //transicionesSinAgrupar.map(f)
  }
  def eliminarEpsilons(tr: Set[Transicion]): Set[Transicion] = {
    tr.filter(!_.esEpsilon())
  }/*
  def actualizarTransiciones(tr: Set[Transicion]): Set[Transicion] = {
    
  }*/
  
  ////////////////////////////////////////////////////////////////////////////////////////////
  def creadorGeneral(ant: Set[Transicion],cont: Set[Estado], a: Automata): Set[Transicion] = {
    
    
    
    
    
    def recursiva(ant: Set[Transicion],sig:Set[Transicion],cont: Set[Estado], a: Automata): Set[Transicion] ={
     /* println("Anterior: " + ant)
      println("Siguiente: " + sig)
      println("guarda" + (ant == sig))
      println("Cont: " + cont)*/
      if(ant == sig){
      ant
      } else {
      val transicionesComoTupla = destinos(cont.head,a)
      val nuevosDatos = creadorDeEstadosAFD(transicionesComoTupla)
      val acum = sig ++ nuevosDatos
      recursiva(sig,acum, cont.tail ++ nuevosDatos.map(_.estadoSalida) ,a)
      }
    }
    
    val transicionesComoTupla = destinos(cont.head,a)
    val nuevosDatos = creadorDeEstadosAFD(transicionesComoTupla)
    val sig = ant ++ nuevosDatos
    recursiva(ant,sig,cont.tail ++ nuevosDatos.map(_.estadoSalida),a)
    
    
    
    
    
    
    
  }
  def creadorDeEstadosAFD(set: Set[(Set[Estado],Char,Estado)]): Set[Transicion] = {
    set.map(crearTransicion(_))
  }
  def crearTransicion(a: (Set[Estado],Char,Estado)): Transicion = {
    val estadoConjunto = a._1
    val estadoNombre = estadoConjunto.flatMap(_.id)
    val estaEnFinales = !estadoConjunto.filter(_.esFinal).isEmpty
    val estaComoInicial = !estadoConjunto.filter(_.esInicial).isEmpty
    val nuevoEstado = new Estado(estadoNombre,estaEnFinales,estaComoInicial/*,transicionesDe(estadoNombre,tr)*/)
    val nuevaTransicion = new Transicion(a._3,a._2,nuevoEstado)
    nuevaTransicion
  }
  ////////////////////////////////////////////////////////////////////////////////////////////
  
  
  
  // A partir de un estado y un automata, devuelve Todos los destinos,
  // itera todos los inputs de abcedario y le calcula todos los destinos ya con clausuras
  def destinos(estado: Estado, a: Automata): Set[(Set[Estado],Char,Estado)] = {
    // revisar, deberia acumular los de cada uno de los estados dentro del estado.
    //println("trabajando en el estado: "+ estado)
    a.alfabetoInput.map(i => destinosVar(i,transicionesDe(estado,a.transiciones),Set(),a,estado))    
  }
  /**
   * Recibe 2 parametros.
   * <li>param1 - String = Estado del cual se desean las transiciones
   * <li>param2 - Set[Transicion] = Todas las transiciones del automata
   * 
   * Retorna un Set[Transicion] que contiene todas las transiciones en las cuales 'param1' es el estadoEntrada
   */
  def transicionesDe(e: Estado, tr: Set[Transicion]): Set[Transicion] = {
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
  def destinosVar(v: Char, tr: Set[Transicion], actual: Set[Estado], a:Automata,e:Estado): (Set[Estado],Char,Estado) = {
    if(tr.size<=0){
      //println("base: "+ actual)
      (actual,v,e)
    } else {
      if(tr.head.esVar(v)){
        val clausura = clausuraEpsilonOBJ(tr.head.estadoSalida,transicionesDe(tr.head.estadoSalida,a.transiciones),a.estadosFinales)
        val acum = actual + clausura
        /*println()
        println("letra: " + v)
        println("actual: " + actual)
        println("clausura: " + clausura)
        println("acum: "+acum)
        println()*/
        destinosVar(v,tr.tail,acum,a,e)
      } else {
        //println("actual: "+ actual)
        destinosVar(v,tr.tail,actual,a,e)
      }
    }
  }
  
  
  
  ////////////////////////////////////////////////////////////////////////////////////////////
  // ClausuraEpsilon (tmb aplica el esFinal/esInicial) y devuelve el Estado
  def clausuraEpsilonOBJ(e: Estado, tr: Set[Transicion], estadosF: Set[Set[String]]) : Estado = {
    val id = e.id ++ unionEstadosSalida(tr.filter(_.clausuraEpsi(e)))
    /*
    if(id.contains("1")){
      println("id: "+id)
      println("estados: "+estadosF)
      println("esFinal: "+esFinal(id,estadosF))
    }*/
    val eFinal = esFinal(id,estadosF)
    val eInicial = esInicial(id)
    
    //hacer vals y devolver tuplas Estado, Transicion
    new Estado(id, eFinal, eInicial)
  }
  // auxiliar para clausuraEpsilonOBJ
  def unionEstadosSalida(tr: Set[Transicion]): Set[String] = {
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
    val gra = importarAutomata("Input/TestInputs/TestImportar")
    println("***************Automata AFND-Epsilon*************")
    println(gra)
    println("***************Automata AFD*************")
    val transformado = transformer(gra)
    println(transformado)
    println("***************Tests individuales*************")
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
  def clausuraEpsilon(deEstadoEntrada: Boolean, tr: Set[Transicion]): Set[String] = {
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
   
  def clausuraEpsilon(e: Estado, tr: Set[Transicion]): Set[String] = {
    tr.filter(_.clausuraEpsi(e)).map(_.estadoSalida.id) + e.id
  }
  */
  /*
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
  */
  
  // este es para testear, no hace falta tenerlo aca afuera del importarAutomata.
  def crearEstadosOBJ(cant: Int, estadosF: Set[String], acum: Set[Estado]/*, tr: Set[Transicion]*/): Set[Estado] = {
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