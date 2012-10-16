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

import net.liftweb.util.ControlHelpers._
import net.liftweb.json.JsonAST.{JNull, JValue, JNothing, JDouble}
import net.liftweb.record._
import field.{DoubleTypedField, NumericTypedField}
import net.liftweb.common._

trait FloatTypedField extends NumericTypedField[Float] {
  def setFromAny(in: Any): Box[Float] = setNumericFromAny(in, _.floatValue)

  def setFromString(s: String): Box[Float] = setBox(tryo(ParseDouble(s).toFloat))

  def defaultValue = 0.0f

  def asJValue = valueBox.map(v => JDouble(v.toFloat)) openOr (JNothing: JValue)
  def setFromJValue(jvalue: JValue) = jvalue match {
    case JNothing|JNull if optional_? => setBox(Empty)
    case JDouble(d)                   => setBox(Full(d.toFloat))
    case other                        => setBox(FieldHelpers.expectedA("JDouble", other))
  }
}

class Neo4jFloatField[OwnerType <: Neo4jRecord[OwnerType]](rec: OwnerType)
  extends Neo4jField[Float, OwnerType] with MandatoryTypedField[Float] with FloatTypedField {

  def this(rec: OwnerType, value: Float) = {
    this(rec)
    set(value)
  }

  def owner = rec

  protected def propToBoxMyType(p: Any): Box[Float] = p match {
    case f: Float => Full(f)
    case _ => Empty
  }
}

class Neo4jOptionalFloatField[OwnerType <: Neo4jRecord[OwnerType]](rec: OwnerType)
  extends Neo4jField[Float, OwnerType] with OptionalTypedField[Float] with FloatTypedField {

  def this(rec: OwnerType, value: Box[Float]) = {
    this(rec)
    setBox(value)
  }

  def owner = rec

  protected def propToBoxMyType(p: Any): Box[Float] = p match {
    case f: Float => Full(f)
    case _ => Empty
  }
}