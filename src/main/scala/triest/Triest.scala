package triest

import utils.{EdgeChange, EdgeSample}

import scala.util.Random

/**
  * Created by Jacob on 18-Nov-16.
  */
abstract class Triest(val   M: Int,
                      val edgeSample: EdgeSample = new EdgeSample(),
                      var globalCounter: Int = 0,
                      var localCounters: Map[Int, Int] = Map(),
                      var t: Int = 0
                      ) {


  def flipBiasedCoin(p: Float): Boolean = {
    val rand = new Random()
    p <= rand.nextFloat()
  }

  def updateCounters(edgeChange: EdgeChange) = {
    val u = edgeChange.e.u
    val v = edgeChange.e.v
    val change = edgeChange.v
    val Nu_v = edgeSample.neighbourHood(u) intersect edgeSample.neighbourHood(v)
    Nu_v.foreach(n => {
      globalCounter += change
      localCounters = localCounters.updated(n, localCounters.getOrElse(n, 0) + change)
      localCounters = localCounters.updated(u, localCounters.getOrElse(u, 0) + change)
      localCounters = localCounters.updated(v, localCounters.getOrElse(v, 0) + change)

    })

  }

  def printCounters(): Unit ={
    println("Global triangle estimate: " + globalCounter)
  }
}
