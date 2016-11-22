package triest

import utils.{Edge, EdgeChange}

/**
  * Created by Jacob on 22-Nov-16.
  */
class TriestImpr(M: Int) extends Triest(M){

  def runEdge(e: Edge) = {
    t += 1
    updateCounters(EdgeChange(1, e))
    if(sampleEdge(e)){
      edgeSample.addEdge(e)
    }
  }

  def estimateGlobalCount(): Double ={
    globalCounter
  }

  override def updateCounters(edgeChange: EdgeChange) = {
    val u = edgeChange.e.u
    val v = edgeChange.e.v
    val change = edgeChange.v
    val Nu_v = edgeSample.neighbourHood(u) intersect edgeSample.neighbourHood(v)
    val n: Double = Math.max(1, (t-1)*(t-2).toFloat/(M*(M-1).toFloat))
    Nu_v.foreach(c => {
      globalCounter += n
      val cCounter: Double = localCounters.getOrElse(c, 0)
      val uCounter: Double = localCounters.getOrElse(u, 0)
      val vCounter: Double = localCounters.getOrElse(v, 0)
      localCounters = localCounters.updated(c, cCounter + n)
      localCounters = localCounters.updated(u, uCounter + n)
      localCounters = localCounters.updated(v, vCounter + n)
    })

  }

  def sampleEdge(e: Edge): Boolean = {
    if(t <= M){
      return true
    }else if(flipBiasedCoin(M.toFloat / t.toFloat)){
      val randomEdge = edgeSample.getRandomEdge()
      edgeSample.removeEdge(randomEdge)
      return true
    }
    return false
  }

}
