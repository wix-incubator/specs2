package org.specs2
package json

import util.matching.Regex
import text.Regexes._

/**
 * This trait provides utility functions for Json objects
 */
private[specs2]
trait Json {

  /**
   * The string has to be parsed by a brand new parser each time
   * otherwise it will not be thread (see issue #70)
   * @return Some(json) if the string is parsable as a JSON document
   */
  def parse(s: String): Option[JSONType] = {
    val parser = new Parser {
      def parseRaw(input : String) : Option[JSONType] =
        phrase(root)(new lexical.Scanner(input)) match {
          case Success(result, _) => Some(result)
          case _ => None
      }
    }
    // give the parser a chance to parse singly-quoted json
    parser.parseRaw(s).orElse(if (s.contains("'")) parser.parseRaw(s.replace("'", "\"")) else None)
  }

  /**
   * @return the list of pairs in the json document where the value is a terminal type
   */
  def terminalPairs(json: JSONType): Seq[(Any, Any)] = collect(json)(
    values = vs,
    objects = os,
    keyedValues = {
      case (key, value) => Seq((key,value))
      case other        => Nil
    },
    keyedObjects = kvs)

  /**
   * @return the list of values in the json document where the value is a terminal type
   */
  def terminalValues(json: JSONType): Seq[Any] = collect(json)(
    values = vs,
    objects = os,
    keyedValues = {
      case (key, value) => Seq(value)
      case other        => Seq(other)
    },
    keyedObjects = kvs)

  /**
   * @return all the JSON objects or values that are addressed by a key in the document.
   *         if there is only one it is returned directly, otherwise the elements are put in a JSONArray
   */
  def findDeep(key: String, json: JSONType): Option[JSONType] = findDeep((k: Any) => k == key, json)
  /**
   * @return findDeep with a Regex
   */
  def findDeep(key: Regex, json: JSONType): Option[JSONType] = findDeep((k: Any) => key matches k.toString, json)

  /**
   * @return findDeep with a key equality function
   */
  def findDeep(condition: Any => Boolean, json: JSONType): Option[JSONType] = {
    val seq = collect(json)(
      objects = os,
      values = vs,
      keyedValues = {
        case (k, v) if condition(k) => Seq(v)
        case other                  => Nil
      },
      keyedObjects = {
        case (k, o) if condition(k) => Seq(o)
        case other                  => Nil
      })
    seq match {
      case Nil                   => None
      case (o: JSONType) :: Nil  => Some(o)
      case other                 => Some(JSONArray(seq.toList))
    }
  }

  /**
   * @return the JSON object that's addressed by a key if the document is a Map
   */
  def find(key: String, json: JSONType): Option[JSONType] = find((k: String) => k == key, json)

  /**
   * @return the JSON object that's addressed by a key if the document is a Map, and the key matches a regular expression
   */
  def find(keyRegex: Regex, json: JSONType): Option[JSONType] = find((key: String) => keyRegex matches key, json)

  /**
   * @return the JSON object that's addressed by a key if the document is a Map, and the key matches a condition on the key
   */
  def find(condition: String => Boolean, json: JSONType): Option[JSONType] = json match {
    case JSONObject(map) => map.iterator.toSeq.find { case (k, v) => condition(k) } map { case (_, o: JSONType) => o }
    case other           => None
  }

  /**
   * generic collect operation to iterate through the JSON tree and get values and objects
   */
  private def collect[T](json: JSONType)(values: Any => Seq[T],
                                         objects: JSONType => Seq[T],
                                         keyedValues: (Any, Any)  => Seq[T],
                                         keyedObjects: (Any, Any) => Seq[T]): Seq[T] = json match {
    case JSONObject(map) => map.toList.flatMap {
      case (k, (o: JSONType)) => objects(o) ++ keyedObjects(k, o) ++ collect(o)(values, objects, keyedValues, keyedObjects)
      case (k, v)             => values(v)  ++ keyedValues(k, v)
    }
    case JSONArray((o: JSONType) :: rest) => objects(o) ++
                                             collect(o)(values, objects, keyedValues, keyedObjects) ++
                                             collect(JSONArray(rest))(values, objects, keyedValues, keyedObjects)
    case JSONArray(other :: rest)         => values(other) ++
                                             collect(JSONArray(rest))(values, objects, keyedValues, keyedObjects)
    case JSONArray(Nil)                   => Nil
  }

  private def vs[T]  = (a: Any) => Nil: Seq[T]
  private def os[T]  = (a: JSONType) => Nil: Seq[T]
  private def kvs[T] = (k: Any, v: Any) => Nil: Seq[T]
}

private[specs2]
object Json extends Json


import scala.util.parsing.combinator._
import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.input.CharArrayReader._

/**
 *  A marker class for the JSON result types.
 *
 *  @author Derek Chen-Becker <"java"+@+"chen-becker"+"."+"org">
 */
private[specs2]
sealed abstract class JSONType {
  /**
   * This version of toString allows you to provide your own value
   * formatter.
   */
  def toString (formatter : JSONFormat.ValueFormatter) : String

  /**
   * Returns a String representation of this JSON value
   * using the JSONFormat.defaultFormatter.
   */
  override def toString = toString(JSONFormat.defaultFormatter)
}

/**
 * This object defines functions that are used when converting JSONType
 * values into String representations. Mostly this is concerned with
 * proper quoting of strings.
 *
 * @author Derek Chen-Becker <"java"+@+"chen-becker"+"."+"org">
 */
private[specs2]
object JSONFormat {
  /**
   * This type defines a function that can be used to
   * format values into JSON format.
   */
  type ValueFormatter = Any => String

  /**
   * The default formatter used by the library. You can
   * provide your own with the toString calls on
   * JSONObject and JSONArray instances.
   */
  val defaultFormatter : ValueFormatter = (x : Any) => x match {
    case s : String => "\"" + quoteString(s) + "\""
    case jo : JSONObject => jo.toString(defaultFormatter)
    case ja : JSONArray => ja.toString(defaultFormatter)
    case other => other.toString
  }

  /**
   * This function can be used to properly quote Strings
   * for JSON output.
   */
  def quoteString (s : String) : String =
    s.map {
      case '"'  => "\\\""
      case '\\' => "\\\\"
      case '/'  => "\\/"
      case '\b' => "\\b"
      case '\f' => "\\f"
      case '\n' => "\\n"
      case '\r' => "\\r"
      case '\t' => "\\t"
      /* We'll unicode escape any control characters. These include:
       * 0x0 -> 0x1f  : ASCII Control (C0 Control Codes)
       * 0x7f         : ASCII DELETE
       * 0x80 -> 0x9f : C1 Control Codes
       *
       * Per RFC4627, section 2.5, we're not technically required to
       * encode the C1 codes, but we do to be safe.
       */
      case c if ((c >= '\u0000' && c <= '\u001f') || (c >= '\u007f' && c <= '\u009f')) => "\\u%04x".format(c: Int)
      case c => c
    }.mkString
}

/**
 *  Represents a JSON Object (map).
 *
 *  @author Derek Chen-Becker <"java"+@+"chen-becker"+"."+"org">
 */
private[specs2]
case class JSONObject (obj : Map[String,Any]) extends JSONType {
  def toString (formatter : JSONFormat.ValueFormatter) =
    "{" + obj.map({ case (k,v) => formatter(k.toString) + " : " + formatter(v) }).mkString(", ") + "}"
}

/**
 *  Represents a JSON Array (list).
 *  @author Derek Chen-Becker <"java"+@+"chen-becker"+"."+"org">
 */
private[specs2]
case class JSONArray (list : List[Any]) extends JSONType {
  def toString (formatter : JSONFormat.ValueFormatter) =
    "[" + list.map(formatter).mkString(", ") + "]"
}

/**
 *  The main JSON Parser.
 *
 *  @author Derek Chen-Becker <"java"+@+"chen-becker"+"."+"org">
 */
private[specs2]
class Parser extends StdTokenParsers with ImplicitConversions {
  // Fill in abstract defs
  type Tokens = Lexer
  val lexical = new Tokens

  // Configure lexical parsing
  lexical.reserved ++= List("true", "false", "null")
  lexical.delimiters ++= List("{", "}", "[", "]", ":", ",")

  /** Type signature for functions that can parse numeric literals */
  type NumericParser = String => Any

  // Global default number parsing function
  protected var defaultNumberParser : NumericParser = {_.toDouble}

  // Per-thread default number parsing function
  protected val numberParser = new ThreadLocal[NumericParser]() {
    override def initialValue() = defaultNumberParser
  }

  // Define the grammar
  def root       = jsonObj | jsonArray
  def jsonObj    = "{" ~> repsep(objEntry, ",") <~ "}" ^^ { case vals : List[_] => JSONObject(Map(vals : _*)) }
  def jsonArray  = "[" ~> repsep(value, ",") <~ "]" ^^ { case vals : List[_] => JSONArray(vals) }
  def objEntry   = stringVal ~ (":" ~> value) ^^ { case x ~ y => (x, y) }
  def value: Parser[Any] = (jsonObj | jsonArray | number | "true" ^^^ true | "false" ^^^ false | "null" ^^^ null | stringVal)
  def stringVal  = accept("string", { case lexical.StringLit(n) => n} )
  def number     = accept("number", { case lexical.NumericLit(n) => numberParser.get.apply(n)} )
}


/**
 *  @author Derek Chen-Becker <"java"+@+"chen-becker"+"."+"org">
 */
private[specs2]
class Lexer extends StdLexical with ImplicitConversions {

  override def token: Parser[Token] =
  //( '\"' ~ rep(charSeq | letter) ~ '\"' ^^ lift(StringLit)
    ( string ^^ StringLit
      | number ~ letter ^^ { case n ~ l => ErrorToken("Invalid number format : " + n + l) }
      | '-' ~> whitespace ~ number ~ letter ^^ { case ws ~ num ~ l => ErrorToken("Invalid number format : -" + num + l) }
      | '-' ~> whitespace ~ number ^^ { case ws ~ num => NumericLit("-" + num) }
      | number ^^ NumericLit
      | EofCh ^^^ EOF
      | delim
      | '\"' ~> failure("Unterminated string")
      | rep(letter) ^^ checkKeyword
      | failure("Illegal character")
      )

  def checkKeyword(xs : List[Any]) = {
    val strRep = xs mkString ""
    if (reserved contains strRep) Keyword(strRep) else ErrorToken("Not a keyword: " + strRep)
  }

  /** A string is a collection of zero or more Unicode characters, wrapped in
   *  double quotes, using backslash escapes (cf. http://www.json.org/).
   */
  def string = '\"' ~> rep(charSeq | chrExcept('\"', '\n', EofCh)) <~ '\"' ^^ { _ mkString "" }

  override def whitespace = rep(whitespaceChar)

  def number = intPart ~ opt(fracPart) ~ opt(expPart) ^^ { case i ~ f ~ e =>
    i + optString(".", f) + optString("", e)
  }
  def intPart = zero | intList
  def intList = nonzero ~ rep(digit) ^^ {case x ~ y => (x :: y) mkString ""}
  def fracPart = '.' ~> rep(digit) ^^ { _ mkString "" }
  def expPart = exponent ~ opt(sign) ~ rep1(digit) ^^ { case e ~ s ~ d =>
    e + optString("", s) + d.mkString("")
  }

  private def optString[A](pre: String, a: Option[A]) = a match {
    case Some(x) => pre + x.toString
    case None => ""
  }

  def zero: Parser[String] = '0' ^^^ "0"
  def nonzero = elem("nonzero digit", d => d.isDigit && d != '0')
  def exponent = elem("exponent character", d => d == 'e' || d == 'E')
  def sign = elem("sign character", d => d == '-' || d == '+')

  def charSeq: Parser[String] =
    ('\\' ~ '\"' ^^^ "\""
      |'\\' ~ '\\' ^^^ "\\"
      |'\\' ~ '/'  ^^^ "/"
      |'\\' ~ 'b'  ^^^ "\b"
      |'\\' ~ 'f'  ^^^ "\f"
      |'\\' ~ 'n'  ^^^ "\n"
      |'\\' ~ 'r'  ^^^ "\r"
      |'\\' ~ 't'  ^^^ "\t"
      |'\\' ~> 'u' ~> unicodeBlock)

  val hexDigits = Set[Char]() ++ "0123456789abcdefABCDEF".toArray
  def hexDigit = elem("hex digit", hexDigits.contains(_))

  private def unicodeBlock = hexDigit ~ hexDigit ~ hexDigit ~ hexDigit ^^ {
    case a ~ b ~ c ~ d =>
      new String(Array(Integer.parseInt(List(a, b, c, d) mkString "", 16)), 0, 1)
  }

  //private def lift[T](f: String => T)(xs: List[Any]): T = f(xs mkString "")
}
