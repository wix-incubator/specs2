package org.specs2.matcher

import MatcherMacros._
import MatchResultLogicalCombinators._
import org.specs2.execute.ResultLogicalCombinators._

class MTest extends MustExpectations {
  def test {
    matchA[Cat].name("Kitty")
  }

  case class Cat(name: String = "", age: Int = 0, kitten: Seq[Cat] = Seq())
}
