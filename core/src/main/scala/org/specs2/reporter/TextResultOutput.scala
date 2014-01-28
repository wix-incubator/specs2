package org.specs2
package reporter

import io.ConsoleOutput

/**
 * Implementation of the ResultOutput trait as Text
 */
class TextResultOutput extends LineLoggerOutput with ConsoleOutput {

  def infoLog(msg: String)    = println(msg)
  def failureLog(msg: String) = println(msg)
  def errorLog(msg: String)   = println(msg)

}

