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