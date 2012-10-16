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