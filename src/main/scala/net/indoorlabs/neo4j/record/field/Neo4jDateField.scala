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

import net.liftweb.common.{Box, Empty, Failure, Full}
import net.liftweb.http.S
import net.liftweb.json.JsonAST._
import net.liftweb.util.Helpers._
import java.util.Date
import net.liftweb.http.S._
import xml.NodeSeq
import net.liftweb.record._
import net.liftweb.http.js.JE.{Str, JsNull}

trait DateTypedField extends TypedField[Date] {

  def setFromAny(in : Any): Box[Date] = toDate(in).flatMap(d => setBox(Full(d))) or genericSetFromAny(in)

  def setFromString(s: String): Box[Date] = s match {
    case "" if optional_? => setBox(Empty)
    case other            => setBox(tryo(parseInternetDate(s)))
  }

  private def elem =
    S.fmapFunc(SFuncHolder(this.setFromAny(_))){funcName =>
      <input type="text"
        name={funcName}
        value={valueBox.map(s => toInternetDate(s.getTime)) openOr ""}
        tabindex={tabIndex toString}/>
    }

  def toForm: Box[NodeSeq] =
    uniqueFieldId match {
      case Full(id) => Full(elem % ("id" -> id))
      case _        => Full(elem)
    }

  def asJs = valueBox.map(v => Str(toInternetDate(v.getTime))) openOr JsNull

  def asJValue = asJString(v => toInternetDate(v.getTime))

  def setFromJValue(jvalue: JValue) = setFromJString(jvalue) {
    v => boxParseInternetDate(v)
  }
}

class Neo4jDateField[OwnerType <: Neo4jRecord[OwnerType]](rec: OwnerType)
  extends Neo4jField[Date, OwnerType] with MandatoryTypedField[Date] with DateTypedField {

  def owner = rec

  def this(rec: OwnerType, value: Date) = {
    this(rec)
    setBox(Full(value))
  }

  def defaultValue = new Date

  protected def propToBoxMyType(p: Any): Box[Date] = toDate(p)
}

class Neo4jOptionalDateField[OwnerType <: Neo4jRecord[OwnerType]](rec: OwnerType)
  extends Neo4jField[Date, OwnerType] with OptionalTypedField[Date] with DateTypedField {

  def owner = rec

  def this(rec: OwnerType, value: Box[Date]) = {
    this(rec)
    setBox(value)
  }

  protected def propToBoxMyType(p: Any): Box[Date] = toDate(p)
}