package net.indoorlabs.neo4j
package record

import field._
import net.liftweb.record.MetaRecord
import org.neo4j.scala.Neo4jImplicits._
import org.neo4j.graphdb.{Direction, Node}
import org.neo4j.graphdb.index._
import scala.collection.JavaConversions._
import java.lang.Exception
import net.liftweb.common._
import java.lang.reflect.Method
import scala.collection.mutable.ListBuffer
import net.liftweb.json.JsonAST._
import sun.security.action.GetLongAction
import org.neo4j.graphdb.Relationship
import net.liftweb.record.FieldHelpers.expectedA
import org.apache.lucene.search.spans._
import se.snigel.lucene.SpanFuzzyQuery
import org.neo4j.index.lucene.QueryContext
import org.apache.lucene.index.Term
import net.liftweb.util.FieldError
import org.apache.lucene.search.Sort

trait Neo4jMetaRecord[BaseRecord <: Neo4jRecord[BaseRecord]]
  extends MetaRecord[BaseRecord] with Neo4jMeta[BaseRecord] {

  self: BaseRecord =>

  private var linkLifecycleCallbacks: List[(String, Method)] = Nil
  private def isLinkLifecycle(m: Method) = classOf[LinkLifecycleCallbacks].isAssignableFrom(m.getReturnType)
  def foreachLinkCallback(inst: BaseRecord, f: LinkLifecycleCallbacks => Any) {
    linkLifecycleCallbacks.foreach(m => f(m._2.invoke(inst).asInstanceOf[LinkLifecycleCallbacks]))
  }
  
  override def validate(inst: BaseRecord): List[FieldError] = {
    super.validate(inst) ::: {
      foreachLinkCallback(inst, _.beforeValidation)
	    try{
		    linksList.flatMap(_.link(inst).validate)
	    } finally {
	      foreachLinkCallback(inst, _.afterValidation)
	    }
    }
  }
  
    
  protected def nodeGroupName: String
  
  def getNodeGroupName = nodeGroupName

  val id_name = "_id"
  val upd_ind_postfix = "_upd"
  val create_ind_postfix = "_cr"

  val created_at = "created_at"
  val updated_at = "updated_at"

  lazy val _node_index = usedb( db => db.index().
    forNodes(nodeGroupName,
    Map[String, String](
      IndexManager.PROVIDER -> "lucene",
      "type" -> "fulltext"))
  )
  //TODO: add index to created at
  //TODO: add index to updated at


  /**
   * Create default node referenced by relation named with nodeNameGroup from neo4j reference node
   */
  private val _mainNode: Node = usedb( db =>
    execInNeo4j(d => {
      val _rel = d.getReferenceNode.getSingleRelationship(nodeGroupName, Direction.OUTGOING)
      val _mainnode = if (_rel == null) {
        val _n = d.createNode
        d.getReferenceNode --> nodeGroupName --> _n
        _n(id_name) = 0L
        _n
      } else {
        _rel.getEndNode
      }
      _mainnode
    })(db)
  )

  def mainNode = _mainNode

  /**
   * Generates a next id and saves next value
   */
  def getNextId = {
    _mainNode(id_name) match {
      case Some(currentId) => currentId match {
        case cId: Long => {
          _mainNode(id_name) = cId + 1L
          cId
        }
        case _ => throw new Exception("Id is not Long")
      }
      case _ => throw new Exception("No id property")
    }
  }

  def dbIndNames: List[String] = Nil

  /**
  * Delete the instance from backing store
  */
  def delete_!(inst: BaseRecord): Boolean = {
    inst.node match {
      case Full(n) => {
        foreachCallback(inst, _.beforeDelete)
        foreachLinkCallback(inst, _.beforeDelete)
        val linked_recs = links(inst).filter(_.isInstanceOf[Neo4jLink[_, _]]).map(_.asInstanceOf[Neo4jLink[_, _]]).
            flatMap(_.deleteLinks(Map(), false, true)).filter(_.node.map(n => {
          n.rels.iterator.hasNext
        }) getOrElse false)
        
        val b = List.newBuilder[Neo4jRecord[_]]
		    val seen = collection.mutable.HashSet[Node]()
		    for (x <- linked_recs) {
		      val n = x.node.get
		      if (!seen(n)) {
		        b += x
		        seen += n
		      }
		    }
        val excluded_records = b.result

        val exclude_nodes = excluded_records.map(_.node.get)

        delete_!(n, Seq(_node_index), exclude_nodes ::: mainNode :: Nil)
        
        //from List distinct method
		    excluded_records.foreach(rec => rec runSafe {rec.delete_!})

        foreachCallback(inst, _.afterDelete)
        foreachLinkCallback(inst, _.afterDelete)
        true
      }
      case _ => false
    }
  }

  //TODO: bulk delete
  /*
  def bulkDelete_!!(qry: DBObject): Unit = {
    useColl(coll =>
      coll.remove(qry)
    )
  }

  def bulkDelete_!!(k: String, o: Any): Unit = bulkDelete_!!(new BasicDBObject(k, o))
  */

  //TODO: AnyRef for queries is bad
  /**
  * Find a single node by a query
  */
  def findByQuery(queryOrQueryObject: AnyRef): Box[BaseRecord] = {
    _node_index.query(queryOrQueryObject).headOption.map(fromNode(_))
    //Full(fromNode(_node_index.query(queryOrQueryObject).headOption))
  }
  
  
  private def filterHits(hits: IndexHits[Node], filter: Node => Boolean = _ => true,
      limit: Option[Int] = None, skip: Option[Int] = None, calc_total: Boolean = false) = {
    var total = 0
    skip.foreach(sk => {
      var skipped = 0
      while (hits.hasNext() && skipped < sk) {
        val n = hits.next()
        if (filter(n)) skipped += 1
        total += 1
      }
    })
    
    var yeilded = List[(Node, Float)]()
    val lim = limit match {
      case Some(lim) if (lim > -1) => (added: Int) => added < lim
      case _ => (added: Int) => true
    }
    
    var added = 0
    while (hits.hasNext() && lim(added)) {
      val n = hits.next()
      if (filter(n)) {
        yeilded :+= (n, hits.currentScore())
        added += 1
      }
      total += 1
    }
    if (calc_total) while (hits.hasNext()) {hits.next; total += 1}
    
    yeilded -> total
  }
  
  private def filterRelHits(hits: Iterator[Relationship], filter: Node => Boolean = _ => true,
      limit: Option[Int] = None, skip: Option[Int] = None, calc_total: Boolean = false) = {
    var total = 0
    skip.foreach(sk => {
      var skipped = 0
      while (hits.hasNext() && skipped < sk) {
        val n = hits.next().getOtherNode(_mainNode)
        if (filter(n)) skipped += 1
        total += 1
      }
    })
    
    var yeilded = List[Node]()
    val lim = limit match {
      case Some(lim) if (lim > -1) => (added: Int) => added < lim
      case _ => (added: Int) => true
    }
    
    var added = 0
    while (hits.hasNext() && lim(added)) {
      val n = hits.next().getOtherNode(_mainNode)
      if (filter(n)) {
        yeilded :+= n
        added += 1
      }
      total += 1
    }
    if (calc_total) while (hits.hasNext()) {hits.next; total += 1}
    
    yeilded -> total
  }

  /**
  * Find all nodes by a query
  */
  def findAllByQuery(queryOrQueryObject: AnyRef, filter: Node => Boolean = _ => true,
      limit: Option[Int] = None, skip: Option[Int] = None, calc_total: Boolean = false): Pair[List[(BaseRecord, Float)], Int] = {
    val hits = _node_index.query(queryOrQueryObject)
    val filtered = filterHits(hits, filter, limit, skip, calc_total)
    filtered._1.map(item => (fromNode(item._1), item._2)) -> filtered._2
  }
  
  def findAllByFuzzyName(index_name: String, name: String, span_near: Int = 0, in_order: Boolean = true,
      filter: Node => Boolean = _ => true,
      limit: Option[Int] = None, skip: Option[Int] = None,
      sort: Option[Sort] = None, calc_total: Boolean = false): (List[(BaseRecord, Float)], Int) = {
    val index_field_name = getNodeGroupName + "_" + index_name
    val words: Array[SpanQuery] = name.split(" ").map(_.trim).filter(_.size > 0).map(word => {
      val size = word.length
      val min_letters: Float = if (size - 2 < 0) 0 else size - 2
      val similarity = min_letters / size
      new SpanFuzzyQuery(new Term(index_field_name, word.toLowerCase), similarity, 0)
    })
    val fuzzy_phrase = new SpanNearQuery(words, span_near, in_order)
    
    val q = new QueryContext( fuzzy_phrase )
    findAllByQuery(sort match {
          case Some(s) => q.sort(s)
          case _ => q.sortByScore()
        }, filter, limit, skip, calc_total)
  }

  /**
  * Find a single node by a query for a specific field
  */
  def find(key: String, queryOrQueryObject: AnyRef): Box[BaseRecord] = fieldByName(key) match {
    case Full(field) => field match {
      case f: Neo4jField[_, BaseRecord] => {
        if (f.dbIndexed_? || dbIndNames.contains(f))
          _node_index.query(nodeGroupName + "_" + key, queryOrQueryObject).headOption.map(fromNode(_))
          //Full(fromNode(_node_index.query(nodeGroupName + "_" + key, queryOrQueryObject).getSingle()))
        else
          Failure("Not indexed field")
      }
      case _ => Failure("Not indexed field")
    }
    case _ => Failure("Not indexed field")
  }

  /**
  * Find a single node by a query for a specific field
  */
  def findAllByKeyQuery(key: String, queryOrQueryObject: AnyRef, filter: Node => Boolean = _ => true,
      limit: Option[Int] = None, skip: Option[Int] = None, calc_total: Boolean = false): Pair[List[(BaseRecord, Float)], Int] = fieldByName(key) match {
    case Full(field) => field match {
      case f: Neo4jField[_, BaseRecord] => {
        if (f.dbIndexed_? || dbIndNames.contains(f)) {
          val hits = _node_index.query(nodeGroupName + "_" + key, queryOrQueryObject)
          val filtered = filterHits(hits, filter, limit, skip, calc_total)
          filtered._1.map(item => (fromNode(item._1), item._2)) -> filtered._2
        }
        else
          Nil -> 0
      }
      case _ => Nil -> 0
    }
    case _ => Nil -> 0
  }

  /**
  * Find a single node by id
  */
  def find(oid: Any): Box[BaseRecord] = findByKey(id_name -> (try { oid.toString.toInt } catch {case _ => -1}))
  
  def findNode(oid: Any): Box[Node] = findByKeyNode(id_name -> (try { oid.toString.toInt } catch {case _ => -1}))

  def find(key_val: Pair[Neo4jField[_, BaseRecord], Any]): Box[BaseRecord] = findByKey(key_val._1.name -> key_val._2)
  /**
  * Find a single node by key -> value
  */
  def findByKey(key_val: Pair[String, Any]): Box[BaseRecord] = {
    val ret: Box[Node] = findByKeyNode(key_val)
    ret.map(fromNode(_))
  }
  
  def findByKeyNode(key_val: Pair[String, Any]): Box[Node] = fieldByName(key_val._1) match {
    case Full(field) => field match {
      case f: Neo4jField[_, BaseRecord] => {
        if (f.dbIndexed_? || dbIndNames.contains(f.name)) {
          _node_index.get(nodeGroupName + "_" + key_val._1, key_val._2).headOption
          /*
          val ind_node = _node_index.get(nodeGroupName + "_" + key_val._1, key_val._2).getSingle()
          if (ind_node == null) Empty else Full(fromNode(ind_node))
          */
        }
        else
          findByRelSingle(key_val)
      }
      case _ => findByRelSingle(key_val)
    }
    case _ => Failure("No _id field")
  }

  private def findByRelSingle(key_val: Pair[String, Any]): Box[Node] = {
    _mainNode.rels(nodeGroupName, Direction.OUTGOING).find(
      _.getOtherNode(_mainNode)(key_val._1) == Some(key_val._2)) match {
        case Some(rel) => Full(rel.getOtherNode(_mainNode))
        case _ => Empty
      }
  }
  
  def findByIndex(key_val: Pair[String, Any]) = {
    _node_index.get(nodeGroupName + "_" + key_val._1, key_val._2).headOption.map(fromNode(_))
    /*
    val ind_node = _node_index.get(nodeGroupName + "_" + key_val._1, key_val._2).getSingle()
    if (ind_node == null) Empty else Full(fromNode(ind_node))
    */
  }
  
  def findAllByIndex(key_val: Pair[String, Any], filter: Node => Boolean = _ => true,
      limit: Option[Int] = None, skip: Option[Int] = None, calc_total: Boolean = false) = {
    val hits = _node_index.get(nodeGroupName + "_" + key_val._1, key_val._2)
    val filtered = filterHits(hits, filter, limit, skip, calc_total)
    filtered._1.map(item => fromNode(item._1)) -> filtered._2
  }
  
  def findAllByIndexQuery(key: String, queryOrQueryObject: AnyRef, filter: Node => Boolean = _ => true,
      limit: Option[Int] = None, skip: Option[Int] = None, calc_total: Boolean = false) = {
    val hits = _node_index.query(nodeGroupName + "_" + key, queryOrQueryObject)
    val filtered = filterHits(hits, filter, limit, skip, calc_total)
    filtered._1.map(item => (fromNode(item._1), item._2)) -> filtered._2
  }

  /**
  * Find all nodes by key -> value
  */
  def findAllByKey(key_val: Pair[String, Any], filter: Node => Boolean = _ => true,
      limit: Option[Int] = None, skip: Option[Int] = None, calc_total: Boolean = false): Pair[List[BaseRecord], Int] = fieldByName(key_val._1) match {
    case Full(field) => field match {
      case f: Neo4jField[_, BaseRecord] => {
        if (f.dbIndexed_? || dbIndNames.contains(f.name)) {
          val hits = _node_index.get(nodeGroupName + "_" + key_val._1, key_val._2)
          val filtered = filterHits(hits, filter, limit, skip, calc_total)
          filtered._1.map(item => fromNode(item._1)) -> filtered._2
        }
        else
          findByRel(key_val, filter, limit, skip, calc_total)
      }
      case _ => findByRel(key_val, filter, limit, skip, calc_total)
    }
    case _ => Nil -> 0
  }
  
  def findAll(filter: Node => Boolean = _ => true,
      limit: Option[Int] = None, skip: Option[Int] = None, calc_total: Boolean = false, no_read: Boolean = false): Pair[List[BaseRecord], Int] = {
    val hits = _mainNode.rels(nodeGroupName, Direction.OUTGOING)
    val filtered = filterRelHits(hits.iterator, filter, limit, skip, calc_total)
    ( if (no_read) {val rec = createRecord; filtered._1.map(v => rec)} else filtered._1.map(fromNode(_)) ) -> filtered._2
  }

  private def findByRel(key_val: Pair[String, Any], filter: Node => Boolean = _ => true,
      limit: Option[Int] = None, skip: Option[Int] = None, calc_total: Boolean = false): Pair[List[BaseRecord], Int] = {
    val filteredRelations = _mainNode.rels(nodeGroupName, Direction.OUTGOING).filter(
      _.getOtherNode(_mainNode)(key_val._1).map(_ == key_val._2) getOrElse false)
    val filtered = filterRelHits(filteredRelations.iterator, filter, limit, skip, calc_total)
    filtered._1.map(fromNode(_)) -> filtered._2
    //filterRelHits(filteredRelations.iterator, filter, limit, skip).map(fromNode(_))
  }

  protected def saveOp(inst: BaseRecord)(f: => Unit): Boolean = {
    foreachCallback(inst, _.beforeSave)
    foreachLinkCallback(inst, _.beforeSave)
    val dirty_fields = inst.node.map(n => {
      fields(inst).map( f => f match {
        case n4jf: Neo4jField[_, BaseRecord] if (n4jf.dirty_?) => Some(n4jf)
        case _ => None
      }).filter(!_.isEmpty).map(_.get)
    })
    f
    inst.node.foreach(n => {
      val dirty = dirty_fields getOrElse fields(inst)
      dirty.foreach( f => if (!n(f.name).isEmpty) f match {
        case n4jf: Neo4jField[_, BaseRecord] => {
          if (n4jf.dbIndexed_? || dbIndNames.contains(f.name)) {
            _node_index.remove(n, nodeGroupName + "_" + f.name)
            _node_index.add(n, nodeGroupName + "_" + f.name, f.get)
          }
        }
        case _ =>
      })
      
      links(inst).foreach( l => {
        l.save_cached
        l.node_index.foreach(ind => {
	        _node_index.remove(n, nodeGroupName + "_" + ind._1)
	        _node_index.add(n, nodeGroupName + "_" + ind._1, ind._2)
	      })
      })
    })
    foreachCallback(inst, _.afterSave)
    foreachLinkCallback(inst, _.afterSave)
    true
  }

  /**
  * Save the instance in the appropriate backing store.
  * New node will be referenced from _mainNode if createRelation_? or create_rel are true
  */
  def save(inst: BaseRecord)(implicit create_rel: Boolean = false): Boolean = saveOp(inst) {
    val node = inst.node match {
      case Full(n) => {
        n.rels(nodeGroupName, Direction.INCOMING).foreach(_(updated_at) = System.currentTimeMillis())
        n
      }
      case _ => {
        val n = usedb(gdb => gdb.createNode())
        if (createRelation_? || create_rel)
          _mainNode --> nodeGroupName --> n
        val cur_time = System.currentTimeMillis()
        n.rels(nodeGroupName, Direction.INCOMING).foreach(rel => {
          rel(created_at) = cur_time
          rel(updated_at) = cur_time
        })
        inst._node = Full(n)
        n
      }
    }
    fillNodeByRecordFields(inst, node)
    inst.fields.foreach(_.resetDirty)
  }

  def createRelation_? = true

  //TODO: multiple nodes save
  /*
  /**
  * Update multiple records with a DBObject query
  */
  def saveMulti(query: DBObject, update: DBObject): Unit = {
    useColl( coll =>
      coll.updateMulti(query, update)
    )
  }
  */

  //TODO: Insert multiple nodes to db
  /*
  /**
   * Insert multiple records
   */
  def insertAll(insts: List[BaseRecord]): Unit = {
    insts.foreach(inst => foreachCallback(inst, _.beforeSave))
    useColl( coll =>
      coll.insert(insts.map(_.asDBObject).toArray:_*)
    )
    insts.foreach(inst => foreachCallback(inst, _.afterSave))
  }
  */

  /**
  * Fill a Node from the field names and values.
  */
  def fillNodeByRecordFields(inst: BaseRecord, node: Node): Unit = {

    import Meta.Reflection._

    for (f <- fields(inst) if (f.dirty_?)) {
      setNodeProperty(f, node)
    }

    node
  }

  /**
  * Creates a new record, then sets the fields with the given DBObject.
  *
  * @param dbo - the DBObject
  * @return Box[BaseRecord]
  */
  def fromNode(node: Node, cr_rec: () => BaseRecord = () => createRecord): BaseRecord = {
    val inst: BaseRecord = cr_rec()//createRecord
    inst._node = Full(node)
    inst.runSafe {
      inst.fields.foreach(_.resetDirty)
    }
    inst
  }

  /**
  * Populate the inst's fields with the values from a given node. Values are set
  * using setFromAny. Should be used to copy values from a different node.
  * Calling on self node will not cause changes
  */
  def setFieldsFromNode(inst: BaseRecord, node: Node): Unit = {
    inst.runSafe {
      inst.fields.foreach(field => {
        node(field.name) match {
          case Some(v) => field.setFromAny(v)
          case _ =>
        }
        field.resetDirty
      })
    }
  }
  
  
  private var linksList: List[LinkHolder[_]] = Nil
  private var linksMap: Map[String, LinkHolder[_]] = Map.empty
  
  def linkByName(linkName: String, inst: BaseRecord): Box[BaseLink[BaseRecord, _]] = {
    Box(linksMap.get(linkName).map(_.link(inst)))
  }
  
  def metaLinks() : List[BaseLink[BaseRecord, _]] = linksList.map(_.metaLink)

  def links(rec: BaseRecord) : List[BaseLink[BaseRecord, _]] = linksList.map(_.link(rec))

  case class LinkHolder[T <: Neo4jRecord[T]](name: String, method: Method, metaLink: BaseLink[BaseRecord, T]) {
    def link(inst: BaseRecord): BaseLink[BaseRecord, T] = method.invoke(inst).asInstanceOf[BaseLink[BaseRecord, T]]
  }
  
  def introspectLinks[T <: Neo4jRecord[T]](rec: BaseRecord, methods: Array[Method])(f: (Method, BaseLink[BaseRecord, T]) => Any): Unit = {
    linkLifecycleCallbacks = (for (v <- methods
                              if v.getName != "meta" && isLinkLifecycle(v)) yield (v.getName, v)).toList
    // find all the potential fields
    val potentialLinks = methods.toList.filter(isLink)

    // any fields with duplicate names get put into a List
    val map: Map[String, List[Method]] = potentialLinks.foldLeft[Map[String, List[Method]]](Map()){
      case (map, method) => val name = method.getName
      map + (name -> (method :: map.getOrElse(name, Nil)))
    }

    // sort each list based on having the most specific type and use that method
    val realMeth = map.values.map(_.sortWith {
      case (a, b) => !a.getReturnType.isAssignableFrom(b.getReturnType)
    }).map(_.head)

    for (v <- realMeth) {
      v.invoke(rec) match {
        case mf: BaseLink[BaseRecord, T] =>
          mf.setName_!(v.getName)
          f(v, mf)
        case _ =>
      }
    }

  }
  
  /**
   * Creates a new record
   */
  override def createRecord: BaseRecord = {
    val rec = super.createRecord
    rec runSafe {
      linksList.foreach(lh => lh.link(rec).setName_!(lh.name))
    }
    rec
  }
  
  private def isLink(m: Method) = {
    val ret = !m.isSynthetic && classOf[BaseLink[_, _]].isAssignableFrom(m.getReturnType)
    ret
  }

  this.runSafe {

    val methods = rootClass.getMethods
    
    def addLink[T <: Neo4jRecord[T]](v: Method, mf: BaseLink[BaseRecord, T]) = {
      val holder = LinkHolder(mf.name, v, mf)
      linksList ::= holder
      linksMap += (holder.name -> holder)
    }

    introspectLinks(this, methods) {addLink}
  }

  def includedLinks: List[String] = Nil
  
  def jvalue_excludedFields: List[String] = Nil

  override def asJValue(rec: BaseRecord): JObject = {
    JObject(super.asJValue(rec).children.map(_.asInstanceOf[JField]).filter(f => {
      f.name != "_id" && !jvalue_excludedFields.contains(f.name)
    }) :::
      includedLinks.map(linkByName(_, rec).map(l => JField(l.name, l.asJValue))).filter(!_.isEmpty).map(_.get))
  }
  
  def asJValue(rec: BaseRecord, locale: String): JObject = {
    JObject(super.asJValue(rec).children.map(_.asInstanceOf[JField]).filter(f => {
      f.name != "_id" && !jvalue_excludedFields.contains(f.name)
    }) :::
      includedLinks.map(linkByName(_, rec).map(l => JField(l.name, l.asJValueLocaled(locale)))).filter(!_.isEmpty).map(_.get))
  }

  def asJValueNoLinks(rec: BaseRecord): JObject =
    JObject(super.asJValue(rec).children.map(_.asInstanceOf[JField]).filter(f =>
      f.name != "_id" && !jvalue_excludedFields.contains(f.name)))

  override def setFieldsFromJValue(rec: BaseRecord, jvalue: JValue): Box[Unit] = {
    def fromJFields(jfields: List[JField]): Box[Unit] = {
      for {
        jfield <- jfields
      } {
        for (field <- rec.fieldByName(jfield.name))
          field.setFromJValue(jfield.value)
        for (link <- linkByName(jfield.name, rec))
          link.setFromJValue(jfield.value)
      }

      Full(())
    }

    jvalue match {
      case JObject(jfields) => fromJFields(jfields)
      case other => expectedA("JObject", other)
    }
  }
}