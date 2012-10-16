package net.indoorlabs.neo4j
package record
package field

import java.util.TimeZone
import net.liftweb.record.field.{TimeZoneTypedField}

class Neo4jTimeZoneField[OwnerType <: Neo4jRecord[OwnerType]](rec: OwnerType)
  extends Neo4jStringField(rec, 32) with TimeZoneTypedField {

  override def defaultValue = TimeZone.getDefault.getID

  def isAsTimeZone: TimeZone = TimeZone.getTimeZone(value) match {
    case null => TimeZone.getDefault
    case x => x
  }
}

class Neo4jOptionalTimeZoneField[OwnerType <: Neo4jRecord[OwnerType]](rec: OwnerType)
  extends Neo4jOptionalStringField(rec, 32) with TimeZoneTypedField