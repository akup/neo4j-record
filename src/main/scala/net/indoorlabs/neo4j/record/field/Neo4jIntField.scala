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

import net.liftweb.record.field.IntTypedField
import net.liftweb.record.{OptionalTypedField, MandatoryTypedField}
import net.liftweb.common.{Empty, Full, Box}

class Neo4jIntField[OwnerType <: Neo4jRecord[OwnerType]](rec: OwnerType)
  extends Neo4jField[Int, OwnerType] with MandatoryTypedField[Int] with IntTypedField {

  def owner = rec

  def this(rec: OwnerType, value: Int) = {
    this(rec)
    set(value)
  }

  protected def propToBoxMyType(p: Any): Box[Int] = p match {
    case i: Int => Full(i)
    case _ => Empty
  }
}

class Neo4jOptionalIntField[OwnerType <: Neo4jRecord[OwnerType]](rec: OwnerType)
  extends Neo4jField[Int, OwnerType] with OptionalTypedField[Int] with IntTypedField {

  def owner = rec

  def this(rec: OwnerType, value: Box[Int]) = {
    this(rec)
    setBox(value)
  }

  protected def propToBoxMyType(p: Any): Box[Int] = p match {
    case i: Int => Full(i)
    case _ => Empty
  }
}