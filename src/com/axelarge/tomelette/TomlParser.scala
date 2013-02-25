package com.axelarge.tomelette

import java.io.FileReader
import java.util.Date
import reflect.ClassTag
import util.parsing.combinator.JavaTokenParsers
import util.parsing.input.Reader
import util.Try


sealed trait TValue { def toUnderlying: Any }
case class TString(value: String)       extends TValue { def toUnderlying = value }
case class TLong(value: Long)           extends TValue { def toUnderlying = value }
case class TDouble(value: Double)       extends TValue { def toUnderlying = value }
case class TBoolean(value: Boolean)     extends TValue { def toUnderlying = value }
case class TDateTime(value: Date)       extends TValue { def toUnderlying = value }
case class TArray(value: List[TValue])  extends TValue { def toUnderlying = value.map(_.toUnderlying) }
case class TDict(value: Map[String, TValue]) extends TValue {
  override def toUnderlying = value.mapValues(_.toUnderlying)

  def get(keyPath: String): Option[TValue] = get(keyPath.split('.').toList)
  def get(keys: List[String]): Option[TValue] = keys match {
    case Nil => Some(this)
    case key :: tail =>
      value.get(key).flatMap {
        case tDict: TDict => tDict.get(tail)
        case other => if (tail.isEmpty) Some(other) else None
      }
  }

  def getAs[T : ClassTag] (keyPath: String): Option[T] = getAs[T](keyPath.split('.').toList)
  def getAs[T : ClassTag] (keys: List[String]): Option[T] = get(keys).map(_.toUnderlying).flatMap {
    case t: T => Some(t)
    case _ => None
  }
}

object TDict {
  val empty = TDict(Map())
}


protected sealed trait Statement
case class KeyGroup(keys: List[String]) extends Statement
case class Assignment(key: String, value: TValue) extends Statement


class TomlParser extends JavaTokenParsers {
  private def safely[A,B](f: A => B): PartialFunction[A,B] = Function.unlift(a => Try(f(a)).toOption)

  override protected val whiteSpace = """(\s|#.*)+""".r

  val integer = """-?(?:0|[1-9]\d*)""".r ^? (safely(str => TLong(str.toLong)), "Cannot parse integer " + _)
  val float = """-?(?:0|[1-9][0-9]*)\.\d+""".r ^? (safely(str => TDouble(str.toDouble)), "Cannot parse floating point number " + _)
  val boolean = ("true" | "false") ^^ (str => TBoolean(str == "true"))
  val string = stringLiteral ^^ (s => TString(s.replace("\\n", "\n").replace("\\r", "\r") .replace("\\\\", "\\").replace("\\t", "\t").replace("\\\"", "\"").replace("\\0", "\0")))
  val datetime = """\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}Z""".r ^? (
    safely(str => TDateTime(javax.xml.bind.DatatypeConverter.parseDateTime(str).getTime)),
    _ + " is not a valid date"
    )
  val array = "[" ~> repsep(primitive, ",") <~ ",".? ~ "]" ^? ({
    case Nil => TArray(Nil)
    case ls@head :: tail  if (tail.forall(_.getClass == head.getClass)) => TArray(ls)
  }, "Array is not homogenous: " + _)

  val primitive: Parser[TValue] = datetime | float | integer | boolean | string | array

  val keyPart = """[_\p{L}][_\p{L}\p{Nd}]*""".r ^^ (_.toString)
  val keyGroup = "[" ~> rep1sep(keyPart, ".") <~ "]" ^^ KeyGroup.apply
  val assignment = (keyPart <~ "=") ~ primitive ^^ {case key ~ value => Assignment(key, value)}

  val statement = keyGroup | assignment
  val statements = statement.*


  private def buildDict(statements: List[Statement]) =
    statements.foldLeft((TDict.empty, List[String]())) {
      case ((dict, keyPath), stmt) => stmt match {
        case KeyGroup(keys) => (dict, keys)
        case Assignment(key, value) => (updateKeyPath(dict, keyPath :+ key, value), keyPath)
      }
    }._1

  private case class DuplicateKeyError(message: String) extends RuntimeException(message)

  private def updateKeyPath(map: TDict, keyPath: List[String], value: TValue): TDict = keyPath match {
    case Nil => map
    case key :: tail =>
      map.value.get(key).getOrElse(TDict.empty) match {
        case tDict: TDict =>
          TDict(map.value.updated(key, if (tail.isEmpty) value else updateKeyPath(tDict, tail, value)))
        case _ =>
          throw new DuplicateKeyError(s"Key $key has already been set!")
      }
  }

  def toEither(res: ParseResult[List[Statement]]): Either[String, TDict] = res match {
    case Success(data, _) =>
      try Right(buildDict(data))
      catch {case DuplicateKeyError(msg) => Left(msg)}
    case NoSuccess(msg, _) => Left(msg)
  }

  def parse(in: Reader[Char]): Either[String, TDict] = toEither(parseAll(statements, in))
  def parse(in: java.io.Reader): Either[String, TDict] = toEither(parseAll(statements, in))
  def parse(in: java.lang.CharSequence): Either[String, TDict] = toEither(parseAll(statements, in))
}


object TomlParserTest {
  def main(args: Array[String]) {
    val parser = new TomlParser

    val result = parser.parse(new FileReader(if (args.length > 0) args(0) else "testdata/test.toml"))
    val out = result.fold (
      error => "Error while parsing: " + error,
      data =>
        if (args.length > 1) data.get(args(1)).map(_.toUnderlying)
        else data.toUnderlying
    )

    println(out)
  }
}