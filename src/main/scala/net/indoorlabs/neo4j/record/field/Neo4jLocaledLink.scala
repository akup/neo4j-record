package net.indoorlabs.neo4j
package record
package field


import org.neo4j.scala.Neo4jImplicits._
import java.util.Locale
import org.neo4j.graphdb._
import net.liftweb.json.JsonAST._
import net.liftweb.json.JsonDSL._
import net.liftweb.json.Printer
import net.liftweb.json.JsonAST

object lang_prop extends Enumeration {
  val av_locales = Locale.getAvailableLocales.toList.map(_.toString)
  type lang_prop = Value
  val lang_code = Value
}

class Neo4jLocaledLink[T <: Neo4jRecord[T], L <: Neo4jRecord[L]](from: T, r_t: RelationshipType, meta: Neo4jMetaRecord[L], postfix: String) extends Neo4jOne2ManyLink(from, r_t, meta) {
  private val _postfix = postfix
  protected val _r_t = r_t
  protected val _meta = meta
  protected val useAsId = lang_prop.lang_code.toString + _postfix
  
  override protected def single_params: Map[String, List[Any]] = Map((useAsId) -> lang_prop.av_locales)
  
  override def doDiff_? = true
  protected lazy val shouldReplace_? = false
  private def changeLinked(rec_node: L, param_map: Map[String, Any] = Map(), immediately: Boolean = false) = {
    val locale = param_map(useAsId).toString
    if (shouldReplace_?) getObjById(locale) match {
      case Some(rec) => this.deleteLinks(Map( (useAsId) -> locale), immediately)
      case _ =>
    }
    super.+= (rec_node, param_map, immediately)
  }
  
  def += (rec_node: L, locale: String) : Unit = changeLinked(rec_node,
      Map( (useAsId) -> locale) )
      
  def += (rec_node: L, locale: String, param_map: Map[String, Any]) : Unit = changeLinked(rec_node,
      Map( (useAsId) -> locale) ++ param_map )

  def += (rec_node: L, locale: String, param_map: Map[String, Any], immediately: Boolean) : Unit = changeLinked(rec_node,
      Map( (useAsId) -> locale) ++ param_map, immediately )
      
  def += (rec_node: L, locale: String, immediately: Boolean) : Unit = changeLinked(rec_node,
      Map( (useAsId) -> locale), immediately )

  def getByLocale(lang: String) = getObjById(lang)
  def getRelByLocale(lang: String) = getRelById(lang)
  
  override protected def getRelById(lang: String): Option[Relationship] = rels( Map( (useAsId) -> lang) ).headOption
  override protected def getObjById(lang: String) = get1( Map( (useAsId) -> lang) ).headOption
  
  def getLocales = get_wl().map(_._2).distinct
  def getAvailableLocales = lang_prop.av_locales
  
  override protected def metaFromRel(rel: Relationship): Option[L] = {
    rel(useAsId).map(lang => {
      meta.fromNode(rel.getOtherNode(owner.node.get))
    })
  }

  def get_wl(params: Map[String, Any] = Map()) = {
    get2(List(useAsId), params).map(e => e._1 -> e._2.find(
        _._1 == useAsId).map(_._2.toString)).filter(!_._2.isEmpty).map(x => x._1 -> x._2.get)
  }
  
  override def equals(other: Any): Boolean = other match {
    case that:Neo4jLocaledLink[T, L] if (that._r_t == r_t && that._meta == meta) => {
      val all_links = this.get_wl()
      val that_links = that.get_wl()
      val r = (all_links.size != that_links.size) || all_links.exists(v => {
        val th_links = that_links.filter(_._2 == v._2)

        that_links.isEmpty || {
          val this_end_node = v._1
          val that_end_node = th_links.head._1

          this_end_node != that_end_node
        }
      })
      !r
    }
    case _ => false
  }
  
  def equals_loc(other: Any, locale: String) = other match {
    case that:Neo4jLocaledLink[T, L] if (that._r_t == r_t && that._meta == meta) => {
      val this_link = this.get1(Map((useAsId) -> locale)).headOption
      val that_link = that.get1(Map((useAsId) -> locale)).headOption
      if (this_link.isEmpty && that_link.isEmpty) true
      else if (!this_link.isEmpty && !that_link.isEmpty) this_link.get == that_link.get
      else false
    }
    case c_ => false
  }
  
  override def asJValue: JObject = {
    get_wl().map(rel => {
      val lang = rel._2
      val str = rel._1
      JField(lang, str.asJValue)
    }).toList
  }
  
  override def asJValueLocaled(locale: String): JObject = {
    get_wl(Map((useAsId) -> locale)).headOption.map(rel => {
      val str = rel._1
      JField(locale, str.asJValue)
    }).toList
  }
  
  def asJArray = {
    JArray(asJValue.children.map(_.asInstanceOf[JField]).map(
          f => JObject(JField("loc", f.name) :: JField("name", f.value.asInstanceOf[JObject]\"str") :: Nil)))
  }
  
  override protected def internalDiff(that: Neo4jLink[T, L], filter: List[Any] = Nil/*, ignore: Boolean = false*/): JObject = that match {
    case that: Neo4jLocaledLink[T, L] if (that._r_t == r_t && that._meta == meta) => {
      var filt: ((String) => Boolean) = (x) => true
      if (!filter.isEmpty) {
        val str_filt = filter.map(_.toString)
        filt = ((x) => str_filt.contains(x))
      }
      
      val initial = asJValue
      val d = initial.diff(that.asJValue)
      val ch_lang_fields: List[JField] = d.changed.children.map(_.asInstanceOf[JField])
      //remember if string with specified locale have changed
      var loc_name_changed = false
      val changed = ch_lang_fields.map(f => JField(f.name, JField("update",
          JObject( f.value.children.map(_.asInstanceOf[JField]).map(val_f => {
            if (filt(f.name) && val_f.name == "str") loc_name_changed = true
            JField(val_f.name,
              JObject(JField("old", initial\f.name\val_f.name) :: JField("new", val_f.value) :: Nil)
            )
          }))
      ) :: Nil))
      val del_lang_fields: List[JField] = d.deleted.children.map(_.asInstanceOf[JField]).filter(f => filt(f.name) && loc_name_changed)
      val deleted = del_lang_fields.map(f => JField(f.name, JField("delete", f.value) :: Nil))
      val add_lang_fields: List[JField] = d.added.children.map(_.asInstanceOf[JField])
      val added = add_lang_fields.map(f => JField(f.name, JField("add", f.value) :: Nil))
      changed ::: deleted ::: added
    }
    case _ => throw new Exception("Can not diff with another typed object")
  }

  override def applyDiff(diff: JObject) = {
    val locale_fields = diff.children.filter(_.isInstanceOf[JField]).map(_.asInstanceOf[JField])
    
    locale_fields.foreach(loc_f => {
      val id = loc_f.name
      val act_field = loc_f.value.asInstanceOf[JObject].children(0).asInstanceOf[JField]
      act_field.name match {
        case "update" => getObjById(id) match {
          case Some(loc_string) => {
            loc_string.applyDiff(JObject(act_field :: Nil))
            //println("UPDATED: " + Printer.compact(JsonAST.render(JObject(act_field :: Nil))))
            //println("UPDATED: " + Printer.compact(JsonAST.render(loc_string.asJValue)))
          }
          case _ => throw new Exception("Locale link does not exist: "+loc_f.name)
        }
        case "delete" => getObjById(id) match {
          case Some(loc_string) => {
            if (!loc_string.delete_!) throw new Exception("Deletion error: "+loc_f.name)
          }
          case _ => throw new Exception("Locale link does not exist: "+loc_f.name)
        }
        case "add" => getObjById(id) match {
          case Some(_) => throw new Exception("Locale link already exists: "+loc_f.name)
          case _ => {
            val rec = meta.createRecord
            rec.setFieldsFromJValue(act_field.value)
            //println("ADDED: " + Printer.compact(JsonAST.render(rec.asJValue)))
            this.+=(rec, loc_f.name)
          }
        }
        case x => throw new Exception("Incorrect action field: "+x)
      }
    })
  }

  override def setFromJValue(jv: JValue): List[L] = {
    val fields = jv.asInstanceOf[JObject].children.map(_.asInstanceOf[JField])
    fields.map(f => {
      getObjById(f.name) match {
        case Some(loc_string) => {
          //val loc_string = LocString.fromNode(link.getEndNode(), () => LocString.createRecord(f.name))
          loc_string.setFieldsFromJValue(f.value)
          loc_string.save
          loc_string
        }
        case _ => {
          val loc_string = meta.createRecord//(f.name)
          loc_string.setFieldsFromJValue(f.value)
          loc_string.save
          this.+=(loc_string, f.name)
          loc_string
        }
      }
    })
  }
}