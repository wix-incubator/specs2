package examples

import org.specs2.{ScalaCheck, Specification}
import org.specs2.matcher.ThrownExpectations
import org.scalacheck._
import Prop._

class TestSpec extends Specification with ScalaCheck with ThrownExpectations { def is = s2"""
  test $e1
"""
  def e1 = prop { i: Int =>
    //-1.5135041232239E9 must beCloseTo(-1.513504123223903E9 +/- 1.0E-7)
//    -1513504123.2239 must beCloseTo(-1513504123.223903 +/- 1.0E-5)
    (i >= 1) ==> Prop.collect(i) {
      i must be_<(0)
    }
  }

}
