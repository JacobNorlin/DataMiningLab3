package triest
import utils._

import scala.collection.immutable.HashMap
import scala.util.Random
/**
  * Created by Jacob on 17-Nov-16.
  */
class TriestBase(M: Int) extends Triest(M){

  def runEdge(e: Edge) = {
      t += 1
      if(sampleEdge(e)){
        edgeSample.addEdge(e)
        updateCounters(new EdgeChange(1, e))
      }
  }

  def estimateGlobalCount(): Double ={
    globalCounter*epsilon()
  }

  def epsilon(): Double = {
    val sampleSize = edgeSample.edges.size
    Math.max(1, (t*t-1*t-2)/(sampleSize*sampleSize-1*sampleSize-2))
  }

  def sampleEdge(e: Edge): Boolean = {
    if(t <= M){
      true
    }else if(flipBiasedCoin(M.toFloat / t.toFloat)){
      val randomEdge = edgeSample.getRandomEdge()
      edgeSample.removeEdge(randomEdge)
      updateCounters(new EdgeChange(-1, randomEdge))
      true
    }else{
      false
    }
  }

}

