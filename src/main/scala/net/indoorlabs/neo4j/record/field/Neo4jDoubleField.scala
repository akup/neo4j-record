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

import net.liftweb.record.field.DoubleTypedField
import net.liftweb.record.{OptionalTypedField, MandatoryTypedField}
import net.liftweb.common.{Empty, Box, Full}

class Neo4jDoubleField[OwnerType <: Neo4jRecord[OwnerType]](rec: OwnerType)
  extends Neo4jField[Double, OwnerType] with MandatoryTypedField[Double] with DoubleTypedField {

  def this(rec: OwnerType, value: Double) = {
    this(rec)
    set(value)
  }

  def owner = rec

  protected def propToBoxMyType(p: Any): Box[Double] = p match {
    case d: Double => Full(d)
    case _ => Empty
  }
}

class Neo4jOptionalDoubleField[OwnerType <: Neo4jRecord[OwnerType]](rec: OwnerType)
  extends Neo4jField[Double, OwnerType] with OptionalTypedField[Double] with DoubleTypedField {

  def this(rec: OwnerType, value: Box[Double]) = {
    this(rec)
    setBox(value)
  }

  def owner = rec

  protected def propToBoxMyType(p: Any): Box[Double] = p match {
    case d: Double => Full(d)
    case _ => Empty
  }
}