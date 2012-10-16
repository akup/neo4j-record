package net.indoorlabs.neo4j
package record
package field

import net.liftweb.util.FieldError
import java.util.regex._
import java.util.regex.{Pattern => RegexPattern}
import net.liftweb.http.S
import net.liftweb.common.{Full, Empty}
import xml.Text
import net.liftweb.record.field.{Countries, StringTypedField}

trait Neo4jPostalCodeTypedField extends StringTypedField {
  protected val country: Neo4jCountryField[_]

  override def setFilter = toUpper _ :: trim _ :: super.setFilter

  override def validations = validatePostalCode _ :: Nil

  def validatePostalCode(in: ValueType): List[FieldError] = country.value match {
    case Countries.USA       => valRegex(RegexPattern.compile("[0-9]{5}(\\-[0-9]{4})?"), S.??("invalid.zip.code"))(in)
    case Countries.Sweden    => valRegex(RegexPattern.compile("[0-9]{3}[ ]?[0-9]{2}"), S.??("invalid.postal.code"))(in)
    case Countries.Australia => valRegex(RegexPattern.compile("(0?|[1-9])[0-9]{3}"), S.??("invalid.postal.code"))(in)
    case Countries.Canada    => valRegex(RegexPattern.compile("[A-Z][0-9][A-Z][ ][0-9][A-Z][0-9]"), S.??("invalid.postal.code"))(in)
    case _ => genericCheck(in)
  }

  private def genericCheck(zip: ValueType): List[FieldError] = {
    toBoxMyType(zip) flatMap {
      case null => Full(Text(S.??("invalid.postal.code")))
      case s if s.length < 3 => Full(Text(S.??("invalid.postal.code")))
      case _ => Empty
    }
  }
}

class Neo4jPostalCodeField[OwnerType <: Neo4jRecord[OwnerType]](rec: OwnerType, val country: Neo4jCountryField[OwnerType])
extends Neo4jStringField(rec, 32) with Neo4jPostalCodeTypedField

class Neo4jOptionalPostalCodeField[OwnerType <: Neo4jRecord[OwnerType]](rec: OwnerType, val country: Neo4jCountryField[OwnerType])
extends Neo4jOptionalStringField(rec, 32) with Neo4jPostalCodeTypedField