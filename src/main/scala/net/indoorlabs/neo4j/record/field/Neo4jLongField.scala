package net.indoorlabs.neo4j
package record
package field

import net.liftweb.record.{OptionalTypedField, MandatoryTypedField}
import net.liftweb.record.field.{LongTypedField}
import net.liftweb.common.{Empty, Full, Box}

class Neo4jLongField[OwnerType <: Neo4jRecord[OwnerType]](rec: OwnerType)
  extends Neo4jField[Long, OwnerType] with MandatoryTypedField[Long] with LongTypedField {

  def this(rec: OwnerType, value: Long) = {
    this(rec)
    set(value)
  }

  def owner = rec

  protected def propToBoxMyType(p: Any): Box[Long] = p match {
    case l: Long => Full(l)
    case _ => Empty
  }
}

class Neo4jOptionalLongField[OwnerType <: Neo4jRecord[OwnerType]](rec: OwnerType)
  extends Neo4jField[Long, OwnerType] with OptionalTypedField[Long] with LongTypedField {

  def this(rec: OwnerType, value: Box[Long]) = {
    this(rec)
    setBox(value)
  }

  def owner = rec

  protected def propToBoxMyType(p: Any): Box[Long] = p match {
    case l: Long => Full(l)
    case _ => Empty
  }
}