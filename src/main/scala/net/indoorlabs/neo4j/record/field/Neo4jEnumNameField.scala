package net.indoorlabs.neo4j
package record
package field

import net.liftweb.record.field.EnumNameTypedField
import net.liftweb.record.{OptionalTypedField, MandatoryTypedField}
import net.liftweb.common.{Full, Empty, Box}

class Neo4jEnumNameField[OwnerType <: Neo4jRecord[OwnerType], EnumType <: Enumeration](rec: OwnerType, protected val enum: EnumType)(implicit m: Manifest[EnumType#Value])
  extends Neo4jField[EnumType#Value, OwnerType] with MandatoryTypedField[EnumType#Value] with EnumNameTypedField[EnumType]
{
  def this(rec: OwnerType, enum: EnumType, value: EnumType#Value)(implicit m: Manifest[EnumType#Value]) = {
    this(rec, enum)
    set(value)
  }

  def owner = rec
  protected val valueManifest = m

  protected def propToBoxMyType(p: Any): Box[EnumType#Value] = p match {
    case s: String => s match {
      case "" if optional_? => Empty
      case _                => enum.values.find(_.toString == s) match {
        case Some(et) => Full(et)
        case _ => Empty
      }
    }
    case _ => Empty
  }
}

class Neo4jOptionalEnumNameField[OwnerType <: Neo4jRecord[OwnerType], EnumType <: Enumeration](rec: OwnerType, protected val enum: EnumType)(implicit m: Manifest[EnumType#Value])
  extends Neo4jField[EnumType#Value, OwnerType] with OptionalTypedField[EnumType#Value] with EnumNameTypedField[EnumType]
{
  def this(rec: OwnerType, enum: EnumType, value: Box[EnumType#Value])(implicit m: Manifest[EnumType#Value]) = {
    this(rec, enum)
    setBox(value)
  }

  def owner = rec
  protected val valueManifest = m

  protected def propToBoxMyType(p: Any): Box[EnumType#Value] = p match {
    case s: String => s match {
      case "" if optional_? => Empty
      case _                => enum.values.find(_.toString == s) match {
        case Some(et) => Full(et)
        case _ => Empty
      }
    }
    case _ => Empty
  }
}

