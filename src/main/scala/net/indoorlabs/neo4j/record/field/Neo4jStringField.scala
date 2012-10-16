package net.indoorlabs.neo4j
package record
package field

import net.liftweb.common.{Empty, Full, Box}
import net.liftweb.record.{OptionalTypedField, MandatoryTypedField}
import net.liftweb.record.field.{StringTypedField}
import net.liftweb.util.StringHelpers._

class Neo4jStringField[OwnerType <: Neo4jRecord[OwnerType]](rec: OwnerType, val maxLength: Int)
  extends Neo4jField[String, OwnerType] with MandatoryTypedField[String] with StringTypedField {

  def this(rec: OwnerType, maxLength: Int, value: String) = {
    this(rec, maxLength)
    set(value)
  }

  def this(rec: OwnerType, value: String) = {
    this(rec, 100)
    set(value)
  }

  def owner = rec

  protected def valueTypeToBoxString(in: ValueType): Box[String] = toBoxMyType(in)
  protected def boxStrToValType(in: Box[String]): ValueType = toValueType(in)

  protected def propToBoxMyType(p: Any): Box[String] = p match {
    case s: String => Full(s)
    case _ => Empty
  }
}

abstract class Neo4jUniqueIdField[OwnerType <: Neo4jRecord[OwnerType]](rec: OwnerType, override val maxLength: Int)
  extends Neo4jStringField[OwnerType](rec, maxLength) {

  override lazy val defaultValue = randomString(maxLen)

  def reset(): OwnerType = this(randomString(maxLen))
}

class Neo4jOptionalStringField[OwnerType <: Neo4jRecord[OwnerType]](rec: OwnerType, val maxLength: Int)
  extends Neo4jField[String, OwnerType] with OptionalTypedField[String] with StringTypedField {

  def this(rec: OwnerType, maxLength: Int, value: Box[String]) = {
    this(rec, maxLength)
    setBox(value)
  }

  def this(rec: OwnerType, value: Box[String]) = {
    this(rec, 100)
    setBox(value)
  }

  def owner = rec

  protected def valueTypeToBoxString(in: ValueType): Box[String] = toBoxMyType(in)
  protected def boxStrToValType(in: Box[String]): ValueType = toValueType(in)

  protected def propToBoxMyType(p: Any): Box[String] = p match {
    case s: String => Full(s)
    case _ => Empty
  }
}