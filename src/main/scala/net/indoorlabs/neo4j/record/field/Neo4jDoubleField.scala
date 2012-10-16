package net.indoorlabs.neo4j
package record
package field

import net.liftweb.record.field.DoubleTypedField
import net.liftweb.record.{OptionalTypedField, MandatoryTypedField}
import net.liftweb.common.{Empty, Box, Full}

class Neo4jDoubleField[OwnerType <: Neo4jRecord[OwnerType]](rec: OwnerType)
  extends Neo4jField[Double, OwnerType] with MandatoryTypedField[Double] with DoubleTypedField {

  def this(rec: OwnerType, value: Double) = {
    this(rec)
    set(value)
  }

  def owner = rec

  protected def propToBoxMyType(p: Any): Box[Double] = p match {
    case d: Double => Full(d)
    case _ => Empty
  }
}

class Neo4jOptionalDoubleField[OwnerType <: Neo4jRecord[OwnerType]](rec: OwnerType)
  extends Neo4jField[Double, OwnerType] with OptionalTypedField[Double] with DoubleTypedField {

  def this(rec: OwnerType, value: Box[Double]) = {
    this(rec)
    setBox(value)
  }

  def owner = rec

  protected def propToBoxMyType(p: Any): Box[Double] = p match {
    case d: Double => Full(d)
    case _ => Empty
  }
}