package triest

import utils._

/**
  * Created by Jacob on 18-Nov-16.
  */
class TriestFD(
                M: Int,
                var dI: Int = 0,
                var dO: Int = 0,
                var s: Int = 0
              ) extends Triest(M) {

  def runEdge(edgeChange: EdgeChange) = {
    t += 1

    val v = edgeChange.v
    val e = edgeChange.e

    s += v

    if(v > 0){
      if(sampleEdge(e)){
        updateCounters(EdgeChange(1, e))
      }
    }else if(edgeSample.containsEdge(e)){
      updateCounters(EdgeChange(-1, e))
      edgeSample.removeEdge(e)
      dI = Math.max(0, dI-1)
    }else{
      dO += Math.max(0, dO - 1)
    }
  }

  def estimateGlobalCount(): Float = {
    println("Starting estimate...")
    val kT = k()
    val p = phi(3,edgeSample.edges.size,s)

    (globalCounter.toFloat/kT)*p
  }

  def phi(a: Int, b: Int, c:Int) = {
    (0 until a-1).fold(1){
      case (acc, i) => {
        acc*(c-i)/(b-i)
      }
    }
  }

  def binomialCoeff(n: Int, k: Int): BigInt = {
    if(n == 0) return 0
    val C = Array.fill[BigInt](k+1)(0)
    C(0) = 1

    (1 to n+1).foreach(i => {
      var j: Int = Math.min(i, k)
      while(j > 0){
        C(j) = C(j) + C(j-1)
        j -= 1
      }
    })

    val a = C(k)
    a
  }


  def k(): Float = {

    def f(j: Int): Float = {
      val w = Math.min(edgeSample.edges.size, dI+dO+s)
      ((binomialCoeff(s, j)*binomialCoeff(dI+dO, w-j)).toFloat /binomialCoeff(s+dI+dO, w).toFloat)
    }

    1 - (f(0)+f(1)+f(2))
  }

  def sampleEdge(e: Edge): Boolean = {
    if (dI + dO == 0) {
      if (edgeSample.edges.size < M) {
        edgeSample.addEdge(e)
        return true
      } else if (flipBiasedCoin(M / t.toFloat)) {
        val randomEdge = edgeSample.getRandomEdge()
        updateCounters(EdgeChange(-1, randomEdge))
        edgeSample.removeEdge(randomEdge)
        edgeSample.addEdge(e)
        return true
      }
    }
    else if (flipBiasedCoin((dI.toFloat / (dI.toFloat + dO.toFloat)))) {
      edgeSample.addEdge(e)
      dI = Math.max(0, dI - 1)
      return true
    }else{
      dO = Math.max(0, dO -1)
      return false
    }

    false
  }


}
