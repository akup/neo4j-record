/*
 *                                The MIT License

Copyright (c) 2012 Alexander Kuprin 


Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
 */

/*
 * Original based on lift-mongodb-record
 * And lift-record
 */

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