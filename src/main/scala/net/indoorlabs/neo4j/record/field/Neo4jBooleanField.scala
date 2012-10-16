package net.indoorlabs.neo4j
package record
package field

import net.liftweb.record.{OptionalTypedField, MandatoryTypedField}
import net.liftweb.record.field.BooleanTypedField
import net.liftweb.common.{Full, Empty, Box}

class Neo4jBooleanField[OwnerType <: Neo4jRecord[OwnerType]](rec: OwnerType)
  extends Neo4jField[Boolean, OwnerType] with MandatoryTypedField[Boolean] with BooleanTypedField {

  def this(rec: OwnerType, value: Boolean) = {
    this(rec)
    set(value)
  }

  def owner = rec

  def defaultValue = false

  protected def propToBoxMyType(p: Any): Box[Boolean] = p match {
    case b: Boolean => Full(b)
    case _ => Empty
  }
}

class Neo4jOptionalBooleanField[OwnerType <: Neo4jRecord[OwnerType]](rec: OwnerType)
  extends Neo4jField[Boolean, OwnerType] with OptionalTypedField[Boolean] with BooleanTypedField {

  def this(rec: OwnerType, value: Box[Boolean]) = {
    this(rec)
    setBox(value)
  }

  def owner = rec

  protected def propToBoxMyType(p: Any): Box[Boolean] = p match {
    case b: Boolean => Full(b)
    case _ => Empty
  }
}