import triest.{Triest, TriestBase, TriestFD, TriestImpr}
import utils.{Edge, EdgeChange}

import scala.io.Source

/**
  * Created by Jacob on 17-Nov-16.
  */
object Main {

  def main(args: Array[String]): Unit = {

    var arglist = args.toList
    type OptionMap = Map[Symbol, Any]

    def nextOption(map: OptionMap, list: List[String]): OptionMap = {
      list match {
        case Nil => map
        case "-fp" :: value :: tail => nextOption(map ++ Map('file_path -> value.toString), tail)
        case "-m" :: value :: tail => nextOption(map ++ Map('M -> value.toInt), tail)
        case "-alg" :: value :: tail => nextOption(map ++ Map('alg -> value.toString), tail)
        case option :: tail => println("unknown parameter")
                              sys.exit(1)
      }
    }

    val options = nextOption(Map(), arglist)
    val filePath: String = options.getOrElse('file_path, "").toString
    val file = Source.fromFile(filePath).getLines()
    val graphData = file.map(l => {
      val nodes = l.split(" ")
      EdgeChange(1, Edge(nodes(0).toInt, nodes(1).toInt))
    })

    val M = Integer.parseInt(options.getOrElse('M, 8000).toString)

    val alg = options.getOrElse('alg, "base")

    alg match{
      case "base" => runTriestBase(M, graphData)
      case "impr" => runTriestImpr(M, graphData)
      case "fd" => runTriestFD(M, graphData)
      case _ => println("Algorithm not found")
    }

  }

  def runTriestImpr(M: Int, graphData: Iterator[EdgeChange]) = {
    val triestImpr = new TriestImpr(M)
    graphData.foreach(ec => triestImpr.runEdge(ec.e))
    println(triestImpr.estimateGlobalCount())
  }

  def runTriestBase(M: Int, graphData: Iterator[EdgeChange]): Unit ={

    val triestBase = new TriestBase(M)
    graphData.foreach(ec => triestBase.runEdge(ec.e))
    println(triestBase.estimateGlobalCount())
  }

  def runTriestFD(M: Int, graphData: Iterator[EdgeChange]) = {
    val triestFD = new TriestFD(M)
    graphData.foreach(triestFD.runEdge)
    println(triestFD.estimateGlobalCount())

  }

}

