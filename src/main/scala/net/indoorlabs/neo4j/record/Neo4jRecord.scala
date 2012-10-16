package net.indoorlabs.neo4j
package record

import field._
import org.neo4j.graphdb._
import org.neo4j.scala.Neo4jImplicits._
import net.liftweb.common.{Box, Full, Empty}
import net.liftweb.record.{LifecycleCallbacks, Record}
import java.lang.Boolean
import net.liftweb.json.JsonAST._
import net.liftweb.json.JsonDSL._

class Neo4jWriteException(msg: String) extends Exception(msg)

/**
 * If neo4jFields are used to present data, save command will not be needed,
 * as values will be setted directly to undeliyng node, if it is not Empty
 */
import org.neo4j.scala.Neo4jImplicits._
trait Neo4jRecord[MyType <: Neo4jRecord[MyType]] extends Record[MyType] {
  self: MyType =>

  /**
   * Neo4jRecord has an underliyng node if it was already created
   */
  private[record] var _node: Box[Node] = Empty

  def node = _node

  /**
  * The meta record (the object that contains the meta result for this type)
  */
  def meta: Neo4jMetaRecord[MyType]

  /**
  * Save the instance and return the instance
  */
  def saveMe(): MyType = {
    this.save()
    this
  }

  def save(): Boolean = {
    runSafe {
      meta.save(this)
    } 
  }

  def save(create_rel: Boolean): Boolean = {
    runSafe {
      meta.save(this)(create_rel)
    }
  }
  
  override def saveTheRecord(): Box[MyType] = if (save()) Full(this) else Empty

  /**
  * Delete the instance from backing store
  */
  def delete_! : Boolean = {
    runSafe {
      meta.delete_!(this)
    }
  }
  
  /**
   * Can the value of this field be read without obscuring the result?
   */
  def canRead_? : Boolean = safe_? || checkCanRead_?

  /**
   * If the owner is not in "safe" mode, check the current environment to see if
   * the field can be read
   */
  def checkCanRead_? = true

  /**
   * Can the value of this field be written?
   */
  def canWrite_? : Boolean = safe_? || checkCanWrite_?

  /**
   * If the owner is not in "safe" mode, check the current environment to see if
   * the field can be written
   */
  def checkCanWrite_? = true

  /**
  * Set the fields of this record from the given Node
  */
  def setFieldsFromNode(node: Node): Unit = meta.setFieldsFromNode(this, node)

  override def toString = {
    val fieldList = this.fields.map(f => "%s=%s" format (f.name,
        f.valueBox match {
          case Full(c: java.util.Calendar) => c.getTime().toString()
          case Full(null) => ""
          case Full(v) => v.toString
          case _ => ""
        }))
    import scala.collection.JavaConversions._
    val relList = _node match {
      case Full(n) => n.rels.map(r => "Relates to "+r.getOtherNode(n))
      case _ => List[String]()
    }

    "%s={%s}{%s}\n%s" format (this.getClass.toString, fieldList.mkString(", "), node, relList.mkString(";\n"))
  }

  override def equals( obj: Any ) = obj match {
    case rec: Neo4jRecord[MyType] => _node.equals( rec._node )
    case _ => false
  }

  override def hashCode() = _node.hashCode()

  def diff(that: MyType, filter: List[Any] = Nil, linkNames: List[String] = meta.includedLinks): JObject = {
    //if (_node.isEmpty || that.node.isEmpty) throw new Exception("Save before diff")
    fillId
    that.fillId
    val links = linkNames.map(meta.linkByName(_, this)).filter(!_.isEmpty).map(_.get)

    val json1 = meta.asJValueNoLinks(this)
    val json2 = meta.asJValueNoLinks(that)
    val d = json1.diff(json2)

    //val ch = d.changed
    val changed = if(d.changed == JNothing) List[JField]()
    else {
      val ch_fields: List[JField] = d.changed.children.map(_.asInstanceOf[JField])
      ch_fields.map(f => JField(f.name,
          JObject(JField("old", json1\f.name) :: JField("new", f.value) :: Nil)))
    }

    val links_diff = links.map(l => {
      val base_l = meta.linkByName(l.name, that).get
      JField(l.name, l.diff(base_l, filter) )
    }).filter(!_.value.children.isEmpty)
        
    val full_diff = changed ::: links_diff
    if (full_diff.size == 0) Nil
    else JField("update", full_diff) :: Nil
  }
  
  def asJValueNoLinks = meta.asJValueNoLinks(this)
  
  def asJValueLocaled(locale: String): JObject = meta.asJValue(this, locale)
  
  def applyDiff(diff: JObject) = {
    val updated_fields = diff\"update"
    val jfields = updated_fields.asInstanceOf[JObject].children.filter(_.isInstanceOf[JField]).map(_.asInstanceOf[JField]).map(jf =>
    	(jf.name, jf)
    ) toMap
    
    fields.foreach(f => jfields.get(f.name).foreach(jf => {
      //println("SET FROM JVALUE 1: " + f.get)
      f.setFromJValue(jf.value\"new")
      //println("SET FROM JVALUE 1: " + f.get + " " + jf.value\"new")
    }))

    //val updated_links = diff\"links"
    //val links_jf = updated_links.children.filter(_.isInstanceOf[JField]).map(_.asInstanceOf[JField]).map(jf => (jf.name, jf)) toMap

    meta.links(this).foreach(l => {
      l.save_cached
      jfields.get(l.name).foreach(_.value match {
        case v: JObject => l.applyDiff(v)
        case _ =>
      })
    })

    save
  }

  def writeException: Nothing = throw new Neo4jWriteException("No write permissions")
  
  
  def add_index(indName: String, value: Any) = node match {
    case Full(n) => meta._node_index.add(n, meta.getNodeGroupName + "_" + indName, value); true
    case _ => false
  }
  
  private[record] def fillId: Option[Long] = None
}

/**
* Mix this into a Record to add an Id autogenerated field
*/
trait Neo4jId[OwnerType <: Neo4jRecord[OwnerType]] extends Neo4jRecord[OwnerType] {
  self: OwnerType =>

  import net.liftweb.record.field.LongField

  //id with generator
  lazy val _id = new MyId(this/*.asInstanceOf[OwnerType]*/, -1L)
  class MyId(rec: OwnerType, value: Long) extends Neo4jLongField(rec, value) with LifecycleCallbacks {
    override def beforeSave = if (get < 0L ) {
      set(meta.getNextId)
    }

    override def defaultValue = -1L
  }

  // convenience method that returns the value of _id
  def id = _id.value
  
  def setNextId = _id.set(meta.getNextId)
  override private[record] def fillId = if (_id.get < 0) Some(setNextId) else Some(id)
}

/**
* Mix this into a Record to add an Id autogenerated indexed field
*/
trait Neo4jIndId[OwnerType <: Neo4jRecord[OwnerType]] extends Neo4jId[OwnerType] {
  self: OwnerType =>

  //id with generator
  override lazy val _id = new MyIndId(this, -1L)
  class MyIndId(rec: OwnerType, value: Long) extends MyId(rec, value) {
    override def dbIndexed_? = true
  }
}

object Neo4jRecordImplicits {
  implicit def record2Node(rec: Neo4jRecord[_]): Option[Node] = rec.node
  
  implicit def record2relationshipBuilder(rec: Neo4jRecord[_]) = new OptionalNodeRelationshipMethods(rec.node)
}