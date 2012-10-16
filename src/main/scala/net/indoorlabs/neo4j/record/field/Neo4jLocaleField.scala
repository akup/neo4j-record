package net.indoorlabs.neo4j
package record
package field

import java.util.Locale
import net.liftweb.record.field.{LocaleTypedField, LocaleField}

class Neo4jLocaleField[OwnerType <: Neo4jRecord[OwnerType]](rec: OwnerType)
  extends Neo4jStringField(rec, 16) with LocaleTypedField {

  override def defaultValue = Locale.getDefault.toString

  def isAsLocale: Locale = Locale.getAvailableLocales.filter(_.toString == value).toList match {
    case Nil => Locale.getDefault
    case x :: xs => x
  }

  def buildDisplayList: List[(String, String)] = LocaleField.localeList

}

class Neo4jOptionalLocaleField[OwnerType <: Neo4jRecord[OwnerType]](rec: OwnerType)
  extends Neo4jOptionalStringField(rec, 16) with LocaleTypedField {

  /** Label for the selection item representing Empty, show when this field is optional. Defaults to the empty string. */
  def emptyOptionLabel: String = ""

  def buildDisplayList: List[(String, String)] = ("", emptyOptionLabel)::LocaleField.localeList
}