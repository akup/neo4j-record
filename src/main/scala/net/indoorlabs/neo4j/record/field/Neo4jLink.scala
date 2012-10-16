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

import org.neo4j.scala.Neo4jImplicits._
import scala.collection.JavaConversions._
import org.neo4j.graphdb._
import index.IndexManager
import net.indoorlabs.neo4j.Meta.Reflection._
import org.apache.lucene.search.TermQuery
import net.liftweb.util.ElemSelector
import net.liftweb.record.BaseField
import net.liftweb.json.JsonAST.{JObject, JNothing}
import net.liftweb.json.JValue
import net.liftweb.common.{Failure, Empty, Box, Full}
import net.liftweb.util._
import scala.collection.immutable.Queue
import net.liftweb.json.JsonAST._
import net.liftweb.json.JsonDSL._

class GuardedException(msg: String) extends Exception(msg)

trait BaseLink[OwnerType <: Neo4jRecord[OwnerType], LinkedType <: Neo4jRecord[LinkedType]] extends FieldIdentifier {
  private[record] var fieldName: String = _
  
  def owner: OwnerType
  
  def doDiff_? = false
  
  def name: String = fieldName  
  def direction = Direction.OUTGOING  
  def r_type: RelationshipType
  
  def anti_direction = if (direction == Direction.OUTGOING) Direction.INCOMING else Direction.OUTGOING
  
  private[record] final def setName_!(newName : String) : String = {
    fieldName = newName
    fieldName
  }
  
  override def uniqueFieldId: Box[String] = Full(name+"_id")
  
  def node_index: Seq[(String, Any)] = Nil //for multilang strings

  /** Validate this links settings, returning any errors found */
  def validate: List[FieldError] = Nil
  
  def save_cached: Unit
  
  
  def asJValue: JObject
  def asJValueLocaled(locale: String): JObject
  def setFromJValue(jv: JValue): List[LinkedType]
  def jval_to_add(jv: JValue): JObject
  def diff(that: BaseLink[OwnerType, _], filter: List[Any]): JObject
  def applyDiff(diff: JObject): Unit
  
  protected def metaFromRel(rel: Relationship): Option[LinkedType]
}

class Neo4jLink[OwnerType <: Neo4jRecord[OwnerType], LinkedType <: Neo4jRecord[LinkedType]]
    (rec: OwnerType, rel_type: RelationshipType, meta: Neo4jMetaRecord[LinkedType]) extends BaseLink[OwnerType, LinkedType] {
  protected def single_params: Map[String, List[Any]] = Map()
  
  private def check_single_params(params: Map[String, Any]) = {
    var ret = false
    if (single_params.size == 0) ret = true
    else {
      val params_query = single_params.filter(entry => {
        params.get(entry._1) match {
          case Some(p_val) => {
            if (entry._2.size > 0) entry._2.contains(p_val)
            else true
          }
          case _ => false
        }
      }).keySet.map(p_name => p_name -> params.get(p_name).get) toMap
      
      //println("P QUERY: " + params_query)
      //println("P QUERY: " + single_params.size)

      if (params_query.size != single_params.size) ret = false
      else {
        ret = (owner.node match {
          case Full(n) => (rels( params_query ).headOption.isEmpty || !get_cached_deleted( params_query ).isEmpty )
          case _ => true
        }) && get_cached_linked( params_query ).isEmpty
      }
      //println("RET: " + ret)
    }
    ret
  }
  
  protected def guard(from_node: Node, to_node: Node, params: Map[String, Any]) = {
    check_single_params(params)
  }
  protected def guard(to_rec: LinkedType, params: Map[String, Any]) = {
    check_single_params(params)
  }
  
  
  override def r_type = rel_type

  protected def index_name : String = ""
    
  private var cached_linked = Queue[(LinkedType, Map[String, Any])]()
  private var cached_deleted = Queue[(Map[String, Any], Boolean)]()
  
  def get_cached_linked() = cached_linked
  def get_cached_linked(param_map: Map[String, Any]) = cached_linked.filter(l => {
    param_map.find(p => l._2.get(p._1) != Some(p._2)).isEmpty
  })
  def get_cached_deleted() = cached_deleted
  def get_cached_deleted(param_map: Map[String, Any]) = cached_deleted.filter(d => {
    d._1.isEmpty || param_map.find(p => d._1.get(p._1) != Some(p._2)).isEmpty
  })
  
  protected def link_node(rec_node: LinkedType, param_map: Map[String, Any], do_guard: Boolean): Unit = {
    if (rec_node.node isEmpty) rec_node.save
    for (n <- owner.node;
         f_n <- rec_node.node) {
      if (do_guard && !guard(n, f_n, param_map)) throw new GuardedException("guarded")
      val new_rel = if (direction == Direction.OUTGOING) (n --> rel_type --> f_n).rel.get
      else (n <-- rel_type <-- f_n).rel.get

      for (param <- param_map) {
        new_rel(param._1) = any2neo4j(param._2.asInstanceOf[AnyRef])
        if (ind_props.contains(param._1)) {
          _rel_index.foreach(_.add(new_rel, param._1, param._2))
        }
      }
      return
    }
    throw new Exception("No n4j nodes to link")
  }
  
  protected def unlink_node(rel: Relationship, deleteNodes: Boolean) = {
    metaFromRel(rel).flatMap(m => {
      _rel_index.foreach(_.remove(rel))
      rel.delete
      if (deleteNodes) {
        m.delete_!
        None
      } else Some(m)
    })
  }
  
  private def unlink_nodes(param_map: Map[String, Any], deleteNodes: Boolean) = {
    internalGetRels(param_map).toList.map(unlink_node(_, deleteNodes)).filter(!_.isEmpty).map(_.get)
  }
  
  protected def save_cached_deleted = {
    cached_deleted.distinct.foreach(dltd => unlink_nodes(dltd._1, dltd._2))
    cached_deleted = Queue[(Map[String, Any], Boolean)]()
  }
  
  protected def save_cached_linked = {
    cached_linked.foreach(lnkd => link_node(lnkd._1, lnkd._2, false))
    cached_linked = Queue[(LinkedType, Map[String, Any])]()
  }
  
  override def save_cached = {
    save_cached_deleted
    save_cached_linked
  }

  protected lazy val _rel_index = if (index_name != "")
    Full(Neo4jDB.use( db => db.index().forRelationships("ft_"+index_name,
      Map[String, String](
        IndexManager.PROVIDER -> "lucene",
        "type" -> "fulltext")) )
    )
    else Empty

  protected lazy val ind_props: List[String] = Nil

  def owner = rec
  
  
  def get2(params: Seq[String] = Nil) = rels().toSeq.filter(rel => {
    !cached_deleted.map(_._1).exists(d => d.filter(p => rel(p._1) != Some(p._2)).isEmpty)
  }).map(rel => meta.fromNode(
      if (direction == Direction.OUTGOING) rel.getEndNode
      else rel.getStartNode
    ) -> params.map(p => p -> rel(p)).filter(!_._2.isEmpty).map(p => p._1 -> p._2.get)) ++ 
    get_cached_linked.map(x => x._1 -> params.map(p => p -> x._2.get(p)).filter(!_._2.isEmpty).map(p => p._1 -> p._2.get))
    
  def get2(params: Seq[String], param_map: Map[String, Any]) = rels(param_map).toSeq.filter(rel => {
    !cached_deleted.map(_._1).exists(d => d.filter(p => rel(p._1) != Some(p._2)).isEmpty)
  }).map(rel => meta.fromNode(
      if (direction == Direction.OUTGOING) rel.getEndNode
      else rel.getStartNode
    ) -> params.map(p => p -> rel(p)).filter(!_._2.isEmpty).map(p => p._1 -> p._2.get)) ++
    get_cached_linked(param_map).map(
        x => x._1 -> params.map(p => p -> x._2.get(p)).filter(!_._2.isEmpty).map(p => p._1 -> p._2.get))
    
  def get1 = rels().toSeq.filter(rel => {
    !cached_deleted.map(_._1).exists(d => d.filter(p => rel(p._1) != Some(p._2)).isEmpty)
  }).map(rel => meta.fromNode(
      if (direction == Direction.OUTGOING) rel.getEndNode
      else rel.getStartNode
    )) ++ get_cached_linked.map(_._1)
    
  def get1(param_map: Map[String, Any] = Map.empty) = rels(param_map).toSeq.filter(rel => {
    !cached_deleted.map(_._1).exists(d => d.filter(p => rel(p._1) != Some(p._2)).isEmpty)
  }).map(rel => meta.fromNode(
      if (direction == Direction.OUTGOING) rel.getEndNode
      else rel.getStartNode
    )) ++ get_cached_linked(param_map).map(_._1)

  def get_saved(): Iterable[LinkedType] = rels.map(rel => meta.fromNode(
      if (direction == Direction.OUTGOING) rel.getEndNode
      else rel.getStartNode
    ))

  def rels(): Iterable[Relationship] =
    if (owner.canRead_?) internalGetRels()
    else Iterable[Relationship]()

  protected def internalGetRels(): Iterable[Relationship] = owner.node match {
    case Full(n) => n.rels(rel_type, direction)
    case _ => Nil//throw new Exception("No n4j from node")
  }

  def get_saved(param_map: Map[String, Any]): Iterable[LinkedType] =
    rels(param_map).map(rel => meta.fromNode(
      if (direction == Direction.OUTGOING) rel.getEndNode
      else rel.getStartNode))

  def rels(param_map: Map[String, Any]): Iterable[Relationship] =
    if (owner.canRead_?) internalGetRels(param_map)
    else Iterable[Relationship]()

  protected def internalGetRels(param_map: Map[String, Any]): Iterable[Relationship] = owner.node match {
    case Full(n) => {
      var query: List[String] = List()

      val p_m = if (index_name != "") param_map.filter(p => {
        if (ind_props.contains(p._1)) {
          query ::= ( """%s:"%s"""" format (p._1, p._2.toString) )
          false
        }
        true
      })
      else param_map

      val rs = query match {
        case head :: tail => {
          _rel_index.get.query( tail.foldLeft(head)(_ + " AND " + _), owner.node.get, null )
        }
        case _ => n.rels(rel_type, direction)
      }

      if (p_m.size > 0) rs.toList.filter(r => {
        p_m.map(p => r(p._1) == Some(p._2)).find(!_).isEmpty
      })
      else rs
    }
    case _ => Nil//throw new Exception("No n4j from node")
  }

  def += (rec_node: LinkedType, param_map: Map[String, Any] = Map(), immediately: Boolean = false) : Unit = if (owner.canWrite_?) {
    if (immediately) {
      save_cached
      link_node(rec_node, param_map, true)
    } else {
      if (!guard(rec_node, param_map)) throw new GuardedException("guarded")
      cached_linked :+= (rec_node, param_map)
    }
  } else owner.writeException
  
  override protected def metaFromRel(rel: Relationship): Option[LinkedType] = Some(meta.fromNode(rel.getOtherNode(owner.node.get)))
  
  def deleteLinks(param_map: Map[String, Any], deleteNodes: Boolean = true, immediately: Boolean = false): List[Neo4jRecord[_]] = if (owner.canWrite_?) {
    var ret = List[LinkedType]()
    if (immediately) {
      save_cached
      ret = unlink_nodes(param_map, deleteNodes)
    }
    
    val spanned = cached_linked.partition(l => {
      param_map.find(p => l._2.get(p._1) != Some(p._2)).isEmpty
    })
    cached_linked = spanned._2
    if (!immediately) cached_deleted :+= (param_map, deleteNodes)
    ret
  } else owner.writeException
  
  //def delete_all_links(deleteNodes: Boolean = true) = internalGetLinks().foreach(unlink_node(_, deleteNodes))
  
  def asJValue: JObject = {
    val all_linked = get2()
    if (all_linked.isEmpty) Nil
    else JObject(JField("links", JArray( all_linked.map(lnkd => lnkd._1.asJValue).toList )) :: Nil)
  }
  def asJValueLocaled(locale: String): JObject = {
    val all_linked = get2()
    if (all_linked.isEmpty) Nil
    else JObject(JField("links", JArray( all_linked.map(lnkd => lnkd._1.asJValueLocaled(locale)).toList )) :: Nil)
  }
  
  def diff(that: BaseLink[OwnerType, _], filter: List[Any] = Nil): JObject = that match {
    case that: Neo4jLink[OwnerType, LinkedType] => internalDiff(that, filter)
    case _ => throw new Exception("Not the same LinkedType")
  }

  protected def internalDiff(that: Neo4jLink[OwnerType, LinkedType], filter: List[Any] = Nil): JObject = if (doDiff_?) {
    val initial = asJValue
    val d = initial.diff(that.asJValue)
    val ch_lang_fields: List[JField] = d.changed.children.map(_.asInstanceOf[JField])
    val changed = ch_lang_fields.map(f => JField(f.name, JField("update",
        JObject( f.value.children.map(_.asInstanceOf[JField]).map(val_f =>
          JField(val_f.name,
            JObject(JField("old", initial\f.name\val_f.name) :: JField("new", val_f.value) :: Nil)
          )
        ))
    ) :: Nil))
    val del_lang_fields: List[JField] = d.deleted.children.map(_.asInstanceOf[JField])
    val deleted = del_lang_fields.map(f => JField(f.name, JField("delete", f.value) :: Nil))
    val add_lang_fields: List[JField] = d.added.children.map(_.asInstanceOf[JField])
    val added = add_lang_fields.map(f => JField(f.name, JField("add", f.value) :: Nil))
    changed ::: deleted ::: added
  } else JObject(Nil)

  
  def applyDiff(diff: JObject): Unit = if (doDiff_?) {
    val links_fields = diff.children.filter(_.isInstanceOf[JField]).map(_.asInstanceOf[JField])
    
    links_fields.foreach(loc_f => {
      val id = loc_f.name
      val act_field = loc_f.value.asInstanceOf[JObject].children(0).asInstanceOf[JField]
      act_field.name match {
        case "update" => getObjById(id) match {
          case Some(obj) => {
            obj.applyDiff(JObject(act_field :: Nil))
            obj.save
          }
          case _ => throw new Exception("Meta object link does not exist: "+loc_f.name)
        }
        case "delete" => getObjById(id) match {
          case Some(obj) => {
            if (!obj.delete_!) throw new Exception("Deletion error: "+loc_f.name)
          }
          case _ => throw new Exception("Meta object link does not exist: "+loc_f.name)
        }
        case "add" => getObjById(id) match {
          case Some(_) => throw new Exception("Meta object link already exists: "+loc_f.name)
          case _ => {
            val rec = meta.createRecord
            rec.save
            rec.setFieldsFromJValue(act_field.value)
            rec.save
            this.+=(rec)
          }
        }
        case x => throw new Exception("Incorrect action field: "+x)
      }
    })
  }
  
  def jval_to_add(jv: JValue): JObject = jv match {
    case x: JObject => x.children.map(_.asInstanceOf[JField]).map(f => JField(f.name, JField("add", f.value) :: Nil))
    case _ => throw new Exception ("Input is not a JObject")
  }
  
  def setFromJValue(jv: JValue): List[LinkedType] = if (doDiff_?) {
    setFromJValueID(jv)
  } else {
    if (!jv.children.isEmpty) {
      owner.save
      deleteLinks(Map(), immediately = true)
      
      val linksArr = (jv\"links").asInstanceOf[JArray]
      linksArr.arr.map(jobj => {
        val obj = meta.createRecord
        obj runSafe{ obj.save; obj.setFieldsFromJValue(jobj); obj.save; }
        +=(obj, immediately = true)
        obj
      })
    } else Nil
  }
  
  protected def setFromJValueID(jv: JValue): List[LinkedType] = {
    val fields = jv.asInstanceOf[JObject].children.map(_.asInstanceOf[JField])
    save_cached
    var ids = Seq[String]()
    val ret = fields.map(f => {
      val id = f.name
      ids = ids :+ id
      getRelById(id) match {
        case Some(link) => {
          val rec = meta.fromNode(link.getEndNode())
          rec.setFieldsFromJValue(f.value)
          rec.save
          rec
        }
        case _ => {
          val rec = meta.createRecord
          rec.setFieldsFromJValue(f.value)
          setRecId(rec, id)
          rec.save
          owner.save
          this.+=(rec)
          rec
        }
      }
    })
    
    deleteLinksIds(ids)
    
    ret
  }
  
  protected def getObjById(id: String): Option[Neo4jRecord[LinkedType]] = None
  protected def setRecId(rec: LinkedType, id: String): Unit = {}
  protected def getRelById(id: String): Option[Relationship] = None
  protected def deleteLinksIds(ids: Seq[String]): Unit = {}
}

class Neo4jSingleLink[OwnerType <: Neo4jRecord[OwnerType], LinkedType <: Neo4jRecord[LinkedType]]
    (rec: OwnerType, rel_type: RelationshipType, meta: Neo4jMetaRecord[LinkedType])
    (implicit guardNode : (Node, Node) =>
        Boolean = (r_n: Node , n: Node) => true,
      guardLinked : (OwnerType, (LinkedType, Map[String, Any])) =>
        Boolean = (r_n: OwnerType , l: (LinkedType, Map[String, Any])) => true) extends BaseLink[OwnerType, LinkedType] {
  def owner = rec
  
  override def r_type = rel_type
  
  private var cached_linked : Option[(LinkedType, Map[String, Any])] = None
  private var cached_deleted : Option[(Boolean)] = None
  
  def get_cached_linked() = cached_linked
  def get_cached_deleted() = cached_deleted
  
  protected def link_node(rec_node: LinkedType, param_map: Map[String, Any], do_guard: Boolean): Unit = {
    if (rec_node.node isEmpty) rec_node.save
    for (n <- owner.node;
         f_n <- rec_node.node) {
      if (n.rels(rel_type, Direction.OUTGOING).size > 0) throw new Exception("Already has relationship")
      if (do_guard && !guardNode(n, f_n)) throw new GuardedException("guarded")
      val new_rel = (n --> rel_type --> f_n).rel.get

      for (param <- param_map) {
        new_rel(param._1) = any2neo4j(param._2.asInstanceOf[AnyRef])
      }
      return
    }
    throw new Exception("No n4j node to link")
  }
  
  private def unlink_node(deleteNode: Boolean): Option[Neo4jRecord[_]] = rel.flatMap(rel => {
    metaFromRel(rel).flatMap(m => {
      rel.delete()
      if (deleteNode) {
        m.delete_!
        None
      } else Some(m)
    })
  })
  
  private def save_cached_deleted = {
    cached_deleted.foreach(unlink_node(_))
    cached_deleted = None
  }
  
  private def save_cached_linked = {
    cached_linked.foreach(lnkd => link_node(lnkd._1, lnkd._2, false))
    cached_linked = None
  }
  
  override def save_cached = {
    save_cached_deleted
    save_cached_linked
  }

  def get_saved: Box[LinkedType] = rel.map(rel => meta.fromNode(rel.getEndNode))
  def get1: Box[LinkedType] = {
    cached_linked.map(_._1) orElse
    {if (!cached_deleted.getOrElse(false)) rel.map(rel => meta.fromNode(rel.getEndNode)) else None}
  }
  def get2(params: Seq[String] = Nil) = {
    cached_linked.map(
        x => x._1 -> params.map(p => p -> x._2.get(p)).filter(!_._2.isEmpty).map(p => p._1 -> p._2.get)) orElse
    {if (!cached_deleted.getOrElse(false))
      rel.map(rel => meta.fromNode(rel.getEndNode) -> params.map(p =>
        p -> rel(p)).filter(!_._2.isEmpty).map(p => p._1 -> p._2.get))
    else None
    }
  }

  def rel: Box[Relationship] = if (owner.canRead_?) owner.node match {
    case Full(n) => n.rels(rel_type, Direction.OUTGOING).headOption match {
      case Some(rel) => Full(rel)
      case _ => Empty
    }
    case _ => Empty
  } else Failure("Current user has no read permissions")

  def --> (rec_node: LinkedType, param_map: Map[String, Any] = Map(), immediately: Boolean = false) = if (owner.canWrite_?) {
    if (immediately) {
      cached_linked = None
      save_cached
      link_node(rec_node, param_map, true)
    } else {
      val l = (rec_node, param_map)
      if (!guardLinked(owner, l)) throw new GuardedException("guarded")
      cached_deleted = None
      cached_linked = Some(l)
    }
  } else owner.writeException
  
  protected def metaFromRel(rel: Relationship): Option[LinkedType] = Some(meta.fromNode(rel.getOtherNode(owner.node.get)))
  
  def deleteLink(deleteNodes: Boolean = true, immediately: Boolean = false): Option[Neo4jRecord[_]] = if (owner.canWrite_?) {
    var ret: Option[Neo4jRecord[_]] = None
    if (immediately) {
      save_cached
      ret = unlink_node(deleteNodes)
    }
    else cached_linked match {
      case Some(lnkd) => {
        cached_linked = None
        cached_deleted = None
      }
      case _ => cached_deleted = Some(deleteNodes)
    }

    ret
  } else owner.writeException
  
  def allLinked: Option[LinkedType] = {
    val l = get_cached_linked.map(_._1)
    if (l.isEmpty) get_saved else l
  }
  
  def asJValue: JObject = {
    val all_linked = allLinked
    all_linked.map(lnkd => lnkd.asJValue) getOrElse Nil
  }
  def asJValueLocaled(locale: String): JObject = {
    val all_linked = allLinked
    all_linked.map(lnkd => lnkd.asJValueLocaled(locale)) getOrElse Nil
  }
  
  def diff(that: BaseLink[OwnerType, _], filter: List[Any] = Nil): JObject = that match {
    case that: Neo4jSingleLink[OwnerType, LinkedType] => internalDiff(that/*, ignore*/)
    case _ => throw new Exception("Not the same LinkedType")
  }

  protected def internalDiff(that: Neo4jSingleLink[OwnerType, LinkedType]): JObject = if (doDiff_?) {
    val pre_ini = asJValue
    val initial: JObject = if (pre_ini.children.isEmpty) pre_ini else JField("link", pre_ini) :: Nil 
    val pre_that = that.asJValue
    val that_val: JObject = if (pre_that.children.isEmpty) pre_that else JField("link", pre_that) :: Nil
    val d = initial.diff(that_val)
    
    val ch_lang_fields: List[JField] = d.changed.children.map(_.asInstanceOf[JField])
    val changed = ch_lang_fields.map(f => JField("update",
        JObject( f.value.children.map(_.asInstanceOf[JField]).map(val_f =>
          JField(val_f.name,
            JObject(JField("old", initial\f.name\val_f.name) :: JField("new", val_f.value) :: Nil)
          )
        ))
    ))
    val del_lang_fields: List[JField] = d.deleted.children.map(_.asInstanceOf[JField])
    val deleted = del_lang_fields.map(f => JField("delete", f.value))
    val add_lang_fields: List[JField] = d.added.children.map(_.asInstanceOf[JField])
    val added = add_lang_fields.map(f => JField("add", f.value))
    changed ::: deleted ::: added
  } else JObject(Nil)
  
  def applyDiff(diff: JObject) = if (doDiff_?) {
    val act_field = diff.children(0).asInstanceOf[JField]
    act_field.name match {
      case "update" => allLinked match {
        case Some(linked) => {
          linked.applyDiff(JObject(act_field :: Nil))
          linked.save
        }
        case _ => throw new Exception("Meta object link not exists")
      }
      case "delete" => allLinked match {
        case Some(linked) => {
          if (!linked.delete_!) throw new Exception("File deletion error")
        }
        case _ => throw new Exception("Meta object link not exists")
      }
      case "add" => allLinked match {
        case Some(_) => throw new Exception("Meta object link already exists")
        case _ => {
          val rec = meta.createRecord
          rec.save
          rec.setFieldsFromJValue(act_field.value)
          rec.save
          this.-->(rec)
        }
      }
      case x => throw new Exception("Incorrect action field: "+x)
    }
  }
  
  def jval_to_add(jv: JValue): JObject = jv match {
    case x: JObject => x.children.map(_.asInstanceOf[JField]).map(f => JField(f.name, JField("add", f.value) :: Nil))
    case _ => throw new Exception ("Input is not a JObject")
  }
  
  def setFromJValue(jv: JValue): List[LinkedType] = {
    if (!jv.children.isEmpty) {
      owner.save
      deleteLink()
      
      //val jobj = (jv\"link").asInstanceOf[JObject]
      val obj = meta.createRecord
      obj runSafe{ obj.save; obj.setFieldsFromJValue(jv); obj.save; }
      -->(obj, immediately = true)
      obj :: Nil
    } else Nil
  }
}

object Neo4jLink {
  implicit def n4jLink_to_iterable[OwnerType <: Neo4jRecord[OwnerType], LinkedType <: Neo4jRecord[LinkedType]](n4jlink: Neo4jLink[OwnerType, LinkedType]): Iterable[LinkedType] = n4jlink.get_saved

  implicit def n4jLink_to_box[OwnerType <: Neo4jRecord[OwnerType], LinkedType <: Neo4jRecord[LinkedType]](n4jlink: Neo4jSingleLink[OwnerType, LinkedType]): Box[LinkedType] = n4jlink.get_saved
}

class Neo4jOne2ManyLink[T <: Neo4jRecord[T], L <: Neo4jRecord[L]](f_n: T, r_t: RelationshipType, meta: Neo4jMetaRecord[L])
    extends Neo4jLink(f_n, r_t, meta) {
  override def r_type = r_t
  
  override protected def guard(from_node: Node, to_node: Node, params: Map[String, Any]) = {
    var ret = false
    //println("ONE 2 MANY")
    //println(ret)
	  if (to_node.rels(r_t, anti_direction).find(r => {
	    if (direction == Direction.OUTGOING) r.getStartNode == from_node
	    else r.getEndNode == to_node
	  }).isEmpty) {
	    //println("NO LINK")
	    ret = super.guard(from_node, to_node, params)
	  }
    //println("LAST RET: " + ret)
	  ret
  }
  
  override protected def guard(to_rec: L, params: Map[String, Any]) = {
    var ret = false
    if (!get_cached_linked.exists(_._1 eq to_rec) || (!to_rec.node.isEmpty && (owner.node match {
      case Full(n) => rels().toSeq.filter(rel => {
          !get_cached_deleted.map(_._1).exists(d => d.filter(p => rel(p._1) != Some(p._2)).isEmpty)
        }).exists(_.getOtherNode(n) == to_rec.node.get)
      case _ => false
    }))) {
      ret = super.guard(to_rec, params)
    }
    ret
  }
}

trait LinkLifecycleCallbacks {
  this: BaseLink[_, _] =>

  def beforeSave {}
  def afterSave {}

  def beforeDelete {}
  def afterDelete {}
  
  def beforeValidation {}
  def afterValidation {}
}
