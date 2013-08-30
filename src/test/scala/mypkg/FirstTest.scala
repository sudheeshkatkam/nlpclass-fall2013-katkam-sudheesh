package mypkg

import org.junit.Assert._
import org.junit.Test

class FirstTests {

  @Test
  def testSomething {
    assertEquals("this is a string", First.something)
  }

}