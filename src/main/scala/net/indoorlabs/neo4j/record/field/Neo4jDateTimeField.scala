package net.indoorlabs.neo4j
package record
package field

import net.liftweb.record.field.DateTimeTypedField
import net.liftweb.record.{OptionalTypedField, MandatoryTypedField}
import net.liftweb.common.{Full, Box}
import java.util.{Date, Calendar}
import net.liftweb.util.TimeHelpers._

class Neo4jDateTimeField[OwnerType <: Neo4jRecord[OwnerType]](rec: OwnerType)
  extends Neo4jField[Calendar, OwnerType] with MandatoryTypedField[Calendar] with DateTimeTypedField {

  def owner = rec

  def this(rec: OwnerType, value: Calendar) = {
    this(rec)
    setBox(Full(value))
  }

  def defaultValue = Calendar.getInstance

  protected def propToBoxMyType(p: Any): Box[Calendar] = toDate(p).flatMap(d => Full(dateToCal(d)))

  private final def dateToCal(d: Date): Calendar = {
    val cal = Calendar.getInstance()
    cal.setTime(d)
    cal
  }
}

class Neo4jOptionalDateTimeField[OwnerType <: Neo4jRecord[OwnerType]](rec: OwnerType)
  extends Neo4jField[Calendar, OwnerType] with OptionalTypedField[Calendar] with DateTimeTypedField {

  def owner = rec

  def this(rec: OwnerType, value: Box[Calendar]) = {
    this(rec)
    setBox(value)
  }

  protected def propToBoxMyType(p: Any): Box[Calendar] = toDate(p).flatMap(d => Full(dateToCal(d)))

  private final def dateToCal(d: Date): Calendar = {
    val cal = Calendar.getInstance()
    cal.setTime(d)
    cal
  }
}