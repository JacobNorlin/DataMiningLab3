import triest.{Triest, TriestBase, TriestFD, TriestImpr}
import utils.{Edge, EdgeChange}

import scala.io.Source

/**
  * Created by Jacob on 17-Nov-16.
  */
object Main {

  def main(args: Array[String]): Unit = {
//    System.setProperty("hadoop.home.dir", "C:\\Users\\Jacob\\Programmering\\lib\\hadoop-2.7.1")
//    val conf = new SparkConf()
//      .setAppName("Simple Application")
//      .setMaster(("local"))
//    val sc = new SparkContext(conf)
//    val file = sc.textFile("./data")
    val file = Source.fromFile("./data/out.moreno_names_names").getLines()
    val graphData = file.map(l => {
      val nodes = l.split(" ")
      EdgeChange(1, Edge(nodes(0).toInt, nodes(1).toInt))
    })

    val M = 3000

    //runTriestBase(M, graphData)
    //runTriestFD(M, graphData)
    runTriestImpr(M, graphData)


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

