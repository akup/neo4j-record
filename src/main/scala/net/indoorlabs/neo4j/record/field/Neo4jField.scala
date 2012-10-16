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


import java.lang.Boolean
import net.liftweb.record.Field
import org.neo4j.scala.Neo4jImplicits._
import net.liftweb.common.{Empty, Full, Box}
import tools.nsc.doc.model.ProtectedInInstance

trait Neo4jField[MyType, OwnerType <: Neo4jRecord[OwnerType]] extends Field[MyType, OwnerType] {
  def dbIndexed_? : Boolean = false

  override def uniqueFieldId: Box[String] = Full(owner.meta.getNodeGroupName + "_" + name+"_id")

  override def valueBox: Box[MyType] = synchronized {
    if (owner.node.isEmpty || dirty_?) super.valueBox
    else if (canRead_?) getNodeValueBox
    else getNodeValueBox.flatMap(obscure)
  }

  protected def getNodeValueBox: Box[MyType] = owner.node match {
    case Full(n) => n(name) match {
      case Some(prop) => propToBoxMyType(prop)
      case _ => Empty
    }
    case _ => Empty
  }

  protected def propToBoxMyType(p: Any): Box[MyType]
  
  override def checkCanRead_? = owner.canRead_?

  override def checkCanWrite_? = owner.canWrite_?
}