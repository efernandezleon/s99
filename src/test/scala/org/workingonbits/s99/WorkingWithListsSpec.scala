package org.workingonbits.s99

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class TestWorkingWithSpec extends FlatSpec with Matchers {
  
  /*
   * Specs for last method
   */
  
  it should "throw error if an empty list is used with last method" in {
    val emptyList = List()
    intercept[RuntimeException] {
      WorkingWithLists.last(emptyList)
    }
  }
  
  "A one-element-list" should "return the element in last method" in {
    val element = 3
    assert(WorkingWithLists.last(List(element)) === 3)
  }
  
  "More-than-one-element-list" should "return the last one in last method" in {
    assert(WorkingWithLists.last(List(1,2,3,4,5,6,7)) === 7)
  }

}