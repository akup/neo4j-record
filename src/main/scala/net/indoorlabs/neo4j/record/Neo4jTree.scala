package net.indoorlabs.neo4j
package record

import field.{Neo4jIntField, Neo4jStringField, Neo4jLink}
import org.neo4j.graphdb.Traverser.Order
import net.liftweb.common._
import org.neo4j.scala.Neo4jImplicits._
import org.neo4j.graphdb._
import scala.collection.JavaConversions._
import org.neo4j.graphdb.{RelationshipType, Direction, TraversalPosition}
import net.liftweb.record.LifecycleCallbacks
import net.liftweb.util.FieldError
import scala.xml.Text
import net.liftweb.json.JsonAST._
import net.liftweb.json.JsonDSL._

trait Neo4jChildHolder[MyType <: Neo4jChildHolder[MyType]] extends Neo4jRecord[MyType] with Neo4jId[MyType] {
  self: MyType =>

  lazy val name: Neo4jStringField[MyType] = new TreeNodeStringField(this, 256)
  protected class TreeNodeStringField(obj: MyType, size: Int) extends Neo4jStringField(obj, size) {
    override def dbIndexed_? = true
  }

  object counter extends Neo4jIntField(this) with LifecycleCallbacks {
    override def beforeSave = if (autoCounter) set(getChildrenCount)
  }

  /*
  def getChildrenCount = node match {
    case Full(n) => {
      n.traverse(Order.BREADTH_FIRST,
          (pos: TraversalPosition) => pos.depth == 1, //one step
          (pos: TraversalPosition) => true, // return all nodes
          req_out: _*).size - 1
    }
    case _ => 0
  }
  */
  
  def getChildrenCount = {
    var i = 0
    meta.links(this).foreach(l => {
      i += (l match {
        case l: Neo4jLink[MyType, Neo4jTree[_]] if (r_types.contains(l.r_type)) =>
          l.get_cached_linked().size + l.rels().size
        case _ => 0
      })
    })
    i
  }

  protected def autoCounter = true

  val r_types: List[RelationshipType]

  private def r_req(dir: Direction) = {for (r_t <- r_types) yield List(r_t, dir)}.flatten
  lazy val req_inc = r_req(Direction.INCOMING)
  lazy val req_out = r_req(Direction.OUTGOING)
}

class TreeLink[T <: Neo4jTree[T], L <: Neo4jTree[L]](f_n: T, r_t: RelationshipType, meta: Neo4jMetaRecord[L], postfix: String)
      extends Neo4jLink(f_n, r_t, meta) {
  protected def _postfix = (postfix + owner.id)
  
  //override protected def index_name = "tree"+_postfix
  //override protected lazy val ind_props = ("id"+_postfix) :: Nil
  
  override def save_cached = {
    super.save_cached
    get_saved.foreach(_.save)
  }
  
  override protected def single_params: Map[String, List[Any]] = Map(("id" + _postfix) -> Nil)
  
  override protected def guard(from_node: Node, to_node: Node, params: Map[String, Any]) = {
    val ch_name = (to_node("name") getOrElse "").toString
    ch_name.length > 0 && !owner.hasChild(ch_name) && super.guard(from_node, to_node, params)
  }
  
  override protected def guard(to_rec: L, params: Map[String, Any]) = {
    val ch_name = to_rec.name.get
    ch_name.length > 0 && !owner.hasChild(ch_name) && super.guard(to_rec, params)
  }
  
  override def += (rec_node: L, param_map: Map[String, Any] = Map(), imidiate: Boolean = false) : Unit = {
    val id = rec_node.runSafe{rec_node.fillId.get}
    
    val p_map = param_map + (("id" + _postfix) -> id)
    super.+= (rec_node, p_map, imidiate)
  }

  override def asJValue: JObject = get2().map(lnkd => JField(lnkd._1.id.toString, lnkd._1.asJValue)).toList
  override def asJValueLocaled(locale: String): JObject = get2().map(lnkd => JField(lnkd._1.id.toString, lnkd._1.asJValueLocaled(locale))).toList
  
  override def getRelById(id: String): Option[Relationship] = super.rels( Map( ("id" + _postfix) -> id.toLong) ).headOption
  
  override def jval_to_add(jv: JValue): JObject = throw new Exception ("Unsupported operation for tree")
  
  override protected def internalDiff(that: Neo4jLink[T, L], filter: List[Any] = Nil/*, ignore: Boolean = false*/): JObject = {
    var linksMap = Map[Long, L]()
    rels.foreach(rel => {
      val node = rel.getEndNode
      val ch = owner.from_node(node).map(_.asInstanceOf[L])
      ch.foreach(c => linksMap += (c.id -> c))
    })
    
    var thatLinksMap = Map[Long, L]()
    that.rels.foreach(rel => {
      val node = rel.getEndNode
      val ch = owner.from_node(node).map(_.asInstanceOf[L])
      ch.foreach(c => thatLinksMap += (c.id -> c))
    })
    
    var already_added = List[String]()
    var changed = List[JField]()
    thatLinksMap.foreach(kv => {
      linksMap.get(kv._1) match {
        case Some(v) => { //here we add changes
          val d = v.diff(kv._2)\"update"
          if (!d.children.isEmpty) {
            changed ::= JField(kv._1.toString, (JField("update", d) :: Nil))
          }
          already_added ::= kv._1.toString
        }
        case _ =>
      }
    })
    
    val initial = asJValue
    val d = initial.diff(that.asInstanceOf[TreeLink[_, _]].asJValue)

    val del_fields: List[JField] = d.deleted.children.map(_.asInstanceOf[JField])
    val deleted = del_fields.map(f => JField(f.name, JField("delete", f.value) :: Nil))
    val add_fields: List[JField] = d.added.children.map(_.asInstanceOf[JField])
    val added = add_fields.filter(f => !already_added.contains(f.name)).map(f => JField(f.name, JField("add", f.value) :: Nil))
    changed ::: deleted ::: added
  }

  override def applyDiff(diff: JObject) = {
    val links_fields = diff.children.filter(_.isInstanceOf[JField]).map(_.asInstanceOf[JField])
    
    links_fields.foreach(loc_f => {
      val id = loc_f.name
      val act_field = loc_f.value.asInstanceOf[JObject].children(0).asInstanceOf[JField]
      act_field.name match {
        case "update" => getRelById(id) match {
          case Some(link) => {
            val rec = meta.fromNode(link.getEndNode())
            rec.applyDiff(JObject(act_field :: Nil))
            rec.save
          }
          case _ => throw new Exception("Tree link does not exist: "+loc_f.name)
        }
        case "delete" => getRelById(id) match {
          case Some(link) => {
            val rec = meta.fromNode(link.getEndNode())
            if (!rec.delete_!) throw new Exception("Deletion error: "+loc_f.name)
          }
          case _ => throw new Exception("Tree link does not exist: "+loc_f.name)
        }
        case "add" => getRelById(id) match {
          case Some(_) => throw new Exception("Locale link already exists: "+loc_f.name)
          case _ => {
            val rec = meta.createRecord
            rec.setFieldsFromJValue(act_field.value)
            rec._id.set(id.toLong)
            rec.save
            this.+=(rec)
          }
        }
        case x => throw new Exception("Incorrect action field: "+x)
      }
    })
  }
  
  override def setFromJValue(jv: JValue): List[L] = setFromJValueID(jv)
  
  override protected def setRecId(rec: L, id: String): Unit = {
    rec._id.set(id.toLong)
  }

  override protected def deleteLinksIds(ids_s: Seq[String]): Unit = {
    val ids = ids_s.map(_.toLong)
    val del_ids = get1().toList.map(_.id).filter(!ids.contains(_))

    del_ids.foreach(id => deleteLinks(Map("id" + _postfix -> id)))
  }
}

trait Neo4jTree[MyType <: Neo4jTree[MyType]] extends Neo4jChildHolder[MyType] {
  self: MyType =>

  def parents: List[Neo4jTree[_]]
  
  def hasChild(ch_name: String) = {
    meta.links(this).exists(_ match {
      case l: Neo4jLink[MyType, Neo4jTree[_]] if (r_types.contains(l.r_type)) =>
        l.get_cached_linked().exists(_._1.name.get == ch_name) ||
        l.rels().exists(_.getEndNode()("name") == Some(ch_name))
      case _ => false
    })
  }

  //TODO: get many pathes
  def getPath(): String = node match {
    case Full(n) => {
      val traverser = n.traverse(Order.DEPTH_FIRST,
          (pos: TraversalPosition) => { //stop evaluator
            val n = pos.currentNode()
            r_types.find(n.hasRelationship(_, Direction.INCOMING)).map(_ => false) getOrElse true
          },
          (pos: TraversalPosition) => true, // return every node
          req_inc: _*)
      traverser.foldRight("")((node, path) => path + "/" + (node("name") getOrElse ("")).toString)
    }
    case _ => ""
  }

  def getTreeNode(path: Option[String]): Box[Neo4jTree[_]] = path match {
    case Some(p) => getTreeNode(p)
    case _ => Empty
  }

  def getTreeNode(path: String): Box[Neo4jTree[_]] = node match {
    case Full(n) => {
      val path_list = path.split("/").filter(_.length >0)
      if (path_list.size == 0) return Full(this)
      var path_len = 0
      val tr = n.traverse(Order.DEPTH_FIRST,
          (pos: TraversalPosition) => {
            pos.returnedNodesCount != pos.depth || (path_list.size) == pos.depth
          },
          (pos: TraversalPosition) => {
            val ret = pos.returnedNodesCount == pos.depth &&
              (pos.depth == 0 || path_list(pos.depth - 1) == (pos.currentNode()("name") getOrElse ""))
            if (ret) path_len += 1
            ret
          },
          req_out: _*)

      val last = tr.lastOption

      if (path_len - 1 == path_list.size) for (last_node <- last) return from_node(last_node)
      Empty
    }
    case _ => Empty
  }

  def from_node(n: Node): Box[Neo4jTree[_]]

  override def delete_! = {
    node.foreach( n =>
      r_types.flatMap(n.rels(_, Direction.OUTGOING)).foreach(r =>
        from_node(r.getEndNode).foreach(rec => rec.delete_!) ) )

    super.delete_!
  }
}