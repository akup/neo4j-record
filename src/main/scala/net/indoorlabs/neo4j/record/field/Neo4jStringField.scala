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

import net.liftweb.common.{Empty, Full, Box}
import net.liftweb.record.{OptionalTypedField, MandatoryTypedField}
import net.liftweb.record.field.{StringTypedField}
import net.liftweb.util.StringHelpers._

class Neo4jStringField[OwnerType <: Neo4jRecord[OwnerType]](rec: OwnerType, val maxLength: Int)
  extends Neo4jField[String, OwnerType] with MandatoryTypedField[String] with StringTypedField {

  def this(rec: OwnerType, maxLength: Int, value: String) = {
    this(rec, maxLength)
    set(value)
  }

  def this(rec: OwnerType, value: String) = {
    this(rec, 100)
    set(value)
  }

  def owner = rec

  protected def valueTypeToBoxString(in: ValueType): Box[String] = toBoxMyType(in)
  protected def boxStrToValType(in: Box[String]): ValueType = toValueType(in)

  protected def propToBoxMyType(p: Any): Box[String] = p match {
    case s: String => Full(s)
    case _ => Empty
  }
}

abstract class Neo4jUniqueIdField[OwnerType <: Neo4jRecord[OwnerType]](rec: OwnerType, override val maxLength: Int)
  extends Neo4jStringField[OwnerType](rec, maxLength) {

  override lazy val defaultValue = randomString(maxLen)

  def reset(): OwnerType = this(randomString(maxLen))
}

class Neo4jOptionalStringField[OwnerType <: Neo4jRecord[OwnerType]](rec: OwnerType, val maxLength: Int)
  extends Neo4jField[String, OwnerType] with OptionalTypedField[String] with StringTypedField {

  def this(rec: OwnerType, maxLength: Int, value: Box[String]) = {
    this(rec, maxLength)
    setBox(value)
  }

  def this(rec: OwnerType, value: Box[String]) = {
    this(rec, 100)
    setBox(value)
  }

  def owner = rec

  protected def valueTypeToBoxString(in: ValueType): Box[String] = toBoxMyType(in)
  protected def boxStrToValType(in: Box[String]): ValueType = toValueType(in)

  protected def propToBoxMyType(p: Any): Box[String] = p match {
    case s: String => Full(s)
    case _ => Empty
  }
}