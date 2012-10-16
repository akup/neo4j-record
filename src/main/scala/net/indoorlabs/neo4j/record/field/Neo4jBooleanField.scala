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

import net.liftweb.record.{OptionalTypedField, MandatoryTypedField}
import net.liftweb.record.field.BooleanTypedField
import net.liftweb.common.{Full, Empty, Box}

class Neo4jBooleanField[OwnerType <: Neo4jRecord[OwnerType]](rec: OwnerType)
  extends Neo4jField[Boolean, OwnerType] with MandatoryTypedField[Boolean] with BooleanTypedField {

  def this(rec: OwnerType, value: Boolean) = {
    this(rec)
    set(value)
  }

  def owner = rec

  def defaultValue = false

  protected def propToBoxMyType(p: Any): Box[Boolean] = p match {
    case b: Boolean => Full(b)
    case _ => Empty
  }
}

class Neo4jOptionalBooleanField[OwnerType <: Neo4jRecord[OwnerType]](rec: OwnerType)
  extends Neo4jField[Boolean, OwnerType] with OptionalTypedField[Boolean] with BooleanTypedField {

  def this(rec: OwnerType, value: Box[Boolean]) = {
    this(rec)
    setBox(value)
  }

  def owner = rec

  protected def propToBoxMyType(p: Any): Box[Boolean] = p match {
    case b: Boolean => Full(b)
    case _ => Empty
  }
}