package net.indoorlabs.neo4j
package record
package field

import net.liftweb.record.field.IntTypedField
import net.liftweb.record.{OptionalTypedField, MandatoryTypedField}
import net.liftweb.common.{Empty, Full, Box}

class Neo4jIntField[OwnerType <: Neo4jRecord[OwnerType]](rec: OwnerType)
  extends Neo4jField[Int, OwnerType] with MandatoryTypedField[Int] with IntTypedField {

  def owner = rec

  def this(rec: OwnerType, value: Int) = {
    this(rec)
    set(value)
  }

  protected def propToBoxMyType(p: Any): Box[Int] = p match {
    case i: Int => Full(i)
    case _ => Empty
  }
}

class Neo4jOptionalIntField[OwnerType <: Neo4jRecord[OwnerType]](rec: OwnerType)
  extends Neo4jField[Int, OwnerType] with OptionalTypedField[Int] with IntTypedField {

  def owner = rec

  def this(rec: OwnerType, value: Box[Int]) = {
    this(rec)
    setBox(value)
  }

  protected def propToBoxMyType(p: Any): Box[Int] = p match {
    case i: Int => Full(i)
    case _ => Empty
  }
}