package utils

import scala.util.Random

/**
  * Created by Jacob on 17-Nov-16.
  */
case class  Edge(u: Int, v: Int)
case class EdgeChange(v: Int, e: Edge)

class EdgeSample(
                  var edges: List[Edge] = List(),
                  val rand: Random = new Random()
                ){
  def addEdge(e: Edge) = {
    edges = e :: edges
  }

  def removeEdge(e: Edge) = {
    edges = edges diff List(e)
  }

  def getRandomEdge(): Edge = {
    edges(rand.nextInt(edges.size))
  }

  def containsEdge(e: Edge): Boolean = {
    edges.contains(e)
  }

  def neighbourHood(n: Int) = {
    edges.filter((e: Edge) => e.u == n || e.v == n)
      .map((e: Edge) => if (e.v == n) e.u else e.v)
  }

}