package utils
import org.scalatest.FlatSpec
import org.scalatest._
/**
  * Created by Jacob on 20-Nov-16.
  */
class EdgeTests extends FlatSpec with Matchers{
  "Neighbourhoods of (1,2) (2,3) (1,3) " should "be" in {
    val edges = List(Edge(1,2),Edge(2,3),Edge(1,3))
    val sample = new EdgeSample(edges)
    val n = sample.neighbourHood(1).sorted
    val n2 = sample.neighbourHood(2).sorted
    val n3 = sample.neighbourHood(3).sorted
    assert(n == List(2,3))
    assert(n2 == List(1,3))
    assert(n3 == List(1,2))
  }

  "Neighbourhood of (1,2) for 1" should "be" in {
    val edges = List(Edge(1,2))
    val sample = new EdgeSample(edges)
    val n = sample.neighbourHood(1)
    val b = sample.neighbourHood(2)
    assert(n == List(2))
    assert(b == List(1))
  }
}
