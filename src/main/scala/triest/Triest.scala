package triest

import utils.{EdgeChange, EdgeSample}

import scala.util.Random

/**
  * Created by Jacob on 18-Nov-16.
  */
abstract class Triest(val   M: Int,
                      val edgeSample: EdgeSample = new EdgeSample(),
                      var globalCounter: Double = 0,
                      var localCounters: Map[Int, Double] = Map(),
                      var t: Int = 0
                      ) {


  def flipBiasedCoin(p: Float): Boolean = {
    val rand = new Random()
    p <= rand.nextFloat()
  }

  def updateCounters(edgeChange: EdgeChange) = {
    val u = edgeChange.e.u
    val v = edgeChange.e.v
    val change = edgeChange.v.toDouble
    val Nu_v = edgeSample.neighbourHood(u) intersect edgeSample.neighbourHood(v)
    Nu_v.foreach(n => {
      globalCounter += change
      val cCounter: Double = localCounters.getOrElse(n, 0)
      val uCounter: Double = localCounters.getOrElse(u, 0)
      val vCounter: Double = localCounters.getOrElse(v, 0)
      localCounters = localCounters.updated(n, cCounter + change)
      localCounters = localCounters.updated(u, uCounter + change)
      localCounters = localCounters.updated(v, vCounter + change)

    })

  }

  def printCounters(): Unit ={
    println("Global triangle estimate: " + globalCounter)
  }
}
