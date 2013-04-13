package monadblocks

import language.{reflectiveCalls, postfixOps}

import MonadBlocks.{wrap, unwrap}
import scalaz._
import Scalaz._
import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.concurrent._
import scala.concurrent.duration._
import ExecutionContext.Implicits.global

@RunWith(classOf[JUnit4])
class SimpleTests {
  
  @Test
  def `simple test`() {
    assertEquals(List("foo"), wrap(unwrap(List("foo"))))
  }
  
  // val blurb = wrap{(); ()}
  
  // val blurb = wrap {
  //   val f1 = wrap[Option, Boolean] { sleep(500); false }
  //   val f2 = wrap[Option, Int] { sleep(4000); 42 }
  //   if (unwrap(f1)) unwrap(f2)
  //   println("harro")
  // }

  // val blurb:List[Boolean] = wrap {
  //   println("hi"); true
  // }

  // val blurb = wrap {
  //   ()
  //   ()
  //   ()
  //   ()
  // }

  // val blurb = wrap {
  //   ()
  //   val f1 = List(false, true)
  //   val f2 = List(4,5)
  //   if (unwrap(f1)) unwrap(f2)
  // }
  
  // val blurb = wrap {
  //   val x = wrap(1)
  //   unwrap(x)
  // }
  
  // val blurb = wrap {
  //   val f1 = List(false)
  //   unwrap(f1)
  //   unwrap(f1)
  //   unwrap(f1)
  //   unwrap(f1)
  // }
  
  // val blurb = wrap {
  //   import scala.concurrent._
  //   import scala.concurrent.duration._
  //   import ExecutionContext.Implicits.global
  //   val f1 = List { sleep(500); false }
  //   val f2 = List { sleep(4000); 42 }
  //   val res = if (unwrap(f1))
  //       unwrap(f2)
  //     else
  //       4
  //   println("harro")
  //   res
  // }
  
  // def bar = wrap {
  //   val xs = List(5,1,12)
  //   if (unwrap(xs) > unwrap(List(9,2,6)))
  //     unwrap(xs)
  // }
}