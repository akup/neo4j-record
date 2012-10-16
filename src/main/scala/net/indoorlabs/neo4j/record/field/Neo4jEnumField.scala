package net.indoorlabs.neo4j
package record
package field

import net.liftweb.record._
import field.EnumTypedField
import net.liftweb.common.{Empty, Full, Box}

class Neo4jEnumField[OwnerType <: Neo4jRecord[OwnerType], EnumType <: Enumeration](rec: OwnerType, protected val enum: EnumType)(implicit m: Manifest[EnumType#Value])
  extends Neo4jField[EnumType#Value, OwnerType] with MandatoryTypedField[EnumType#Value] with EnumTypedField[EnumType]
{
  def this(rec: OwnerType, enum: EnumType, value: EnumType#Value)(implicit m: Manifest[EnumType#Value]) = {
    this(rec, enum)
    set(value)
  }

  def owner = rec
  protected val valueManifest = m

  protected def propToBoxMyType(p: Any): Box[EnumType#Value] = p match {
    case value: Int => fromInt(value)
    case _ => Empty
  }
}

class Neo4jOptionalEnumField[OwnerType <: Neo4jRecord[OwnerType], EnumType <: Enumeration](rec: OwnerType, protected val enum: EnumType)(implicit m: Manifest[EnumType#Value])
  extends Neo4jField[EnumType#Value, OwnerType] with OptionalTypedField[EnumType#Value] with EnumTypedField[EnumType]
{
  def this(rec: OwnerType, enum: EnumType, value: Box[EnumType#Value])(implicit m: Manifest[EnumType#Value]) = {
    this(rec, enum)
    setBox(value)
  }

  def owner = rec
  protected val valueManifest = m

  protected def propToBoxMyType(p: Any): Box[EnumType#Value] = p match {
    case value: Int => fromInt(value)
    case _ => Empty
  }
}