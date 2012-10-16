package net.indoorlabs.neo4j
package record
package field

import net.liftweb.util.ControlHelpers._
import net.liftweb.json.JsonAST.{JNull, JValue, JNothing, JDouble}
import net.liftweb.record._
import field.{DoubleTypedField, NumericTypedField}
import net.liftweb.common._

trait FloatTypedField extends NumericTypedField[Float] {
  def setFromAny(in: Any): Box[Float] = setNumericFromAny(in, _.floatValue)

  def setFromString(s: String): Box[Float] = setBox(tryo(ParseDouble(s).toFloat))

  def defaultValue = 0.0f

  def asJValue = valueBox.map(v => JDouble(v.toFloat)) openOr (JNothing: JValue)
  def setFromJValue(jvalue: JValue) = jvalue match {
    case JNothing|JNull if optional_? => setBox(Empty)
    case JDouble(d)                   => setBox(Full(d.toFloat))
    case other                        => setBox(FieldHelpers.expectedA("JDouble", other))
  }
}

class Neo4jFloatField[OwnerType <: Neo4jRecord[OwnerType]](rec: OwnerType)
  extends Neo4jField[Float, OwnerType] with MandatoryTypedField[Float] with FloatTypedField {

  def this(rec: OwnerType, value: Float) = {
    this(rec)
    set(value)
  }

  def owner = rec

  protected def propToBoxMyType(p: Any): Box[Float] = p match {
    case f: Float => Full(f)
    case _ => Empty
  }
}

class Neo4jOptionalFloatField[OwnerType <: Neo4jRecord[OwnerType]](rec: OwnerType)
  extends Neo4jField[Float, OwnerType] with OptionalTypedField[Float] with FloatTypedField {

  def this(rec: OwnerType, value: Box[Float]) = {
    this(rec)
    setBox(value)
  }

  def owner = rec

  protected def propToBoxMyType(p: Any): Box[Float] = p match {
    case f: Float => Full(f)
    case _ => Empty
  }
}