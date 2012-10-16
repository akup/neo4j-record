package net.indoorlabs.neo4j
package record
package field

import net.liftweb.common._
import net.liftweb.util._
import net.liftweb.http._
import java.util.regex.Pattern
import net.liftweb.record.TypedField
import scala.xml.Text

object EmailField {
  def emailPattern = Pattern.compile("^[_A-Za-z0-9-]+(\\.[_A-Za-z0-9-]+)*@[A-Za-z0-9-]+(\\.[A-Za-z0-9-]+)*(\\.[A-Za-z]{2,4})$")

  def validEmailAddr_?(email: String): Boolean = emailPattern.matcher(email).matches
}

trait EmailTypedField extends TypedField[String] {
  private def validateEmail(emailValue: ValueType): List[FieldError] =
    toBoxMyType(emailValue) match {
      case Full(email) if EmailField.validEmailAddr_?(email) => Nil
      case _ => Text(S.??("invalid.email.address"))
    }

  override def validations = validateEmail _ :: Nil
}

class Neo4jEmailField[OwnerType <: Neo4jRecord[OwnerType]](rec: OwnerType, maxLength: Int)
  extends Neo4jStringField[OwnerType](rec, maxLength) with EmailTypedField

class Neo4jOptionalEmailField[OwnerType <: Neo4jRecord[OwnerType]](rec: OwnerType, maxLength: Int)
  extends Neo4jOptionalStringField[OwnerType](rec, maxLength) with EmailTypedField