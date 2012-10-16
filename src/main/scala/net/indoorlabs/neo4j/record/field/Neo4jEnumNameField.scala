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

import net.liftweb.record.field.EnumNameTypedField
import net.liftweb.record.{OptionalTypedField, MandatoryTypedField}
import net.liftweb.common.{Full, Empty, Box}

class Neo4jEnumNameField[OwnerType <: Neo4jRecord[OwnerType], EnumType <: Enumeration](rec: OwnerType, protected val enum: EnumType)(implicit m: Manifest[EnumType#Value])
  extends Neo4jField[EnumType#Value, OwnerType] with MandatoryTypedField[EnumType#Value] with EnumNameTypedField[EnumType]
{
  def this(rec: OwnerType, enum: EnumType, value: EnumType#Value)(implicit m: Manifest[EnumType#Value]) = {
    this(rec, enum)
    set(value)
  }

  def owner = rec
  protected val valueManifest = m

  protected def propToBoxMyType(p: Any): Box[EnumType#Value] = p match {
    case s: String => s match {
      case "" if optional_? => Empty
      case _                => enum.values.find(_.toString == s) match {
        case Some(et) => Full(et)
        case _ => Empty
      }
    }
    case _ => Empty
  }
}

class Neo4jOptionalEnumNameField[OwnerType <: Neo4jRecord[OwnerType], EnumType <: Enumeration](rec: OwnerType, protected val enum: EnumType)(implicit m: Manifest[EnumType#Value])
  extends Neo4jField[EnumType#Value, OwnerType] with OptionalTypedField[EnumType#Value] with EnumNameTypedField[EnumType]
{
  def this(rec: OwnerType, enum: EnumType, value: Box[EnumType#Value])(implicit m: Manifest[EnumType#Value]) = {
    this(rec, enum)
    setBox(value)
  }

  def owner = rec
  protected val valueManifest = m

  protected def propToBoxMyType(p: Any): Box[EnumType#Value] = p match {
    case s: String => s match {
      case "" if optional_? => Empty
      case _                => enum.values.find(_.toString == s) match {
        case Some(et) => Full(et)
        case _ => Empty
      }
    }
    case _ => Empty
  }
}

