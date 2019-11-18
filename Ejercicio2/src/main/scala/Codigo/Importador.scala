package Codigo

import Modelo.Gramatica
import Modelo.Produccion

import scala.io.Source
import java.io._

import scala.util.matching.Regex

object Importador {
  def importarGramatica(ruta: String): Gramatica = {

    def verificarFormato(lineas:Iterator[String],pattern:Regex):Set[Char] = {
      if(lineas.hasNext){

        val linea = lineas.next()
        println("linea leida: " + linea)
        if(linea.size == 0){
          Set()
        }
        else{
           pattern.findFirstIn(linea) match{
            case Some(linea) => linea.toSet
            case _=> null
          }

        }
      }else{
        null
      }
    }

    def verificarProducciones(lineas:Iterator[String],terminales:Set[Char],variables:Set[Char]):Set[Produccion] = {
      if(lineas.hasNext){
        crearProducciones(lineas.next().split(","),terminales,variables)
      }else{
        Set()
      }
    }
    def crearProducciones(s: Array[String],terminales:Set[Char],variables:Set[Char]): Set[Produccion] = {
      if (s.size == 0) {
        Set()
      } else {
        val pattern = "->".r

        val partesProd = pattern.findFirstIn(s(s.size - 1)) match {
          case Some("->") => s(s.size - 1).split("->")
          case _=> Array(" "," ")
        }
        if(partesProd.size == 2){
          val v = partesProd(0)
          val c = partesProd(1)

          val variable = "[A-Z]+".r.findFirstIn(v) match{
            case Some(v) => v
            case _=> null
          }

          val cadena = "[a-zA-Zε]+".r.findFirstIn(c) match{
            case Some(c) => c
            case _=> null
          }

          if(variable == null || cadena == null || variable.length != 1 || (variable.toSet -- variables) == variable || (cadena.toSet -- (variables ++ terminales ++ Set('ε'))).size != 0){
            crearProducciones(s.take(s.size - 1),terminales,variables) + null
          }else{
            crearProducciones(s.take(s.size - 1),terminales,variables) + new Produccion(variable.head, cadena)
          }
        }
        else{
          Set(null)
        }


        }

      }


    val lineas = Source.fromFile(ruta).getLines()
    val terminales = verificarFormato(lineas,"[a-z]+".r)
    val variables = verificarFormato(lineas, "[A-Z]+".r)
    val inicial = verificarFormato(lineas, "[A-Z]+".r)
    val producciones = verificarProducciones(lineas,terminales,variables)

    println("terminales: " + terminales + " variables: " + variables + " inicial: " + inicial + " producciones: " + producciones)
    if((terminales == null || variables == null || inicial == null || inicial.size != 1 || (inicial -- variables) == inicial || producciones.contains(null))){
      null
    }else {
     new Gramatica(terminales, variables, inicial.head, producciones)
    }

  }

  def main(args:Array[String]):Unit = {
    print("\n".size)
  }


}