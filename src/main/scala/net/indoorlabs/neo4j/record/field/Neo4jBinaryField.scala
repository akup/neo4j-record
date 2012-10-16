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

import net.liftweb.mongodb._
import com.mongodb._
import gridfs._
import java.util.Date
import org.neo4j.scala.Neo4jImplicits._
import _root_.net.liftweb.common._
import _root_.net.liftweb.http.js._
import JsCmds._
import _root_.net.liftweb.json.JsonAST.JValue
import _root_.net.liftweb.util._
import Helpers._
import JE._
import net.liftweb.http.{FileParamHolder, LiftRules, S}
import org.bson.types.ObjectId
import org.neo4j.graphdb._
import net.liftweb.record.{OptionalTypedField, MandatoryTypedField, TypedField}
import xml.{NodeSeq, Text}
import com.sun.mail.iap.ByteArray
import java.io.{FileOutputStream, File, ByteArrayInputStream, InputStream}
import org.apache.commons.io.IOUtils
import java.io.FileInputStream
import net.liftweb.http.SHtml
import net.liftmodules.AjaxFileUpload._

//TODO: close stream on grabage collection
trait ByteArrayTypedField extends TypedField[Array[Byte]] {
  protected var _id: Box[ObjectId] = Empty
  protected var _bindata: Box[Array[Byte]] = Empty
  protected var _file_name: Box[String] = Empty
  protected var _file_length: Box[Long] = Empty
  
  private def elem(attrs: SHtml.ElemAttr*) =
      SHtml.fileUpload((fph: FileParamHolder) => this.setFromAny(fph), attrs: _*)
      
  private def elem_ajax(pl: Option[ProgressListener], options: Option[FileSHtml.UploadOptions], attrs: SHtml.ElemAttr*) =
      FileSHtml.ajaxFileUpload((fph: FileParamHolder) => {this.setFromAny(fph); Noop}, pl, options, attrs: _*)

  def toForm(ajax: Boolean, pl: Option[ProgressListener] = None, options: Option[FileSHtml.UploadOptions] = None): Box[NodeSeq] =
    uniqueFieldId match {
      case Full(id) => Full(if (!ajax) elem("id" -> id) else elem_ajax(pl, options, "id" -> id))
      case _ => Full(elem())
    }

  def id = _id
  //def stream = _stream.synchronized {_stream}
  def file_name = if (canRead_?) _id.synchronized {_file_name} else Failure("value obscured")
  def file_length = if (canRead_?) _id.synchronized {_file_length} else Failure("value obscured")
  def content_type = if (canRead_?) _file_name match {
      case Full(f_name) => {
        val _context = LiftRules.context
        if (_context != null)
          Full(LiftRules.context.mimeType(f_name) openOr "application/octet-stream")
        else
          Empty
      }
      case _ => Empty
  } else Failure("value obscured")

  override def setFromAny(in: Any): Box[Array[Byte]] = in match {
    case Pair(in, fname) if fname.isInstanceOf[String] => in match {
      case b_arr: Array[Byte] => {
        _id.synchronized {
          //val stream = new ByteArrayInputStream(b_arr)
          _file_name = Full(fname.toString)
          _file_length = Full(b_arr.length)
          _bindata = Full(b_arr)
          setBox(_bindata)
        }
      }
      case is: InputStream => {
        _id.synchronized {
          _file_name = Full(fname.toString)
          _bindata = Full(IOUtils.toByteArray(is))
          _file_length = _bindata.map(_.length)
          is.close
          setBox(_bindata)
        }
      }
      case in => genericSetFromAny(in)
    }
    case fph: FileParamHolder => {
      _id.synchronized {
        _file_name = Full(fph.fileName)
        _bindata = Full(IOUtils.toByteArray(fph.fileStream))
        _file_length = _bindata.map(_.length)
        setBox(_bindata)
      }
    }
    case obj_id: ObjectId => {
      MongoDB.use(DefaultMongoIdentifier) ( db => {
        _id.synchronized {
          val fs = new GridFS(db)
      
          fs.findOne(obj_id) match {
            case file:GridFSDBFile => _id.synchronized {
              val stream = Full(file.getInputStream)
              _file_name = Full(file.getFilename)
              _file_length = Full(file.getLength)
              _bindata = stream.map(IOUtils.toByteArray(_))
              setBox(_bindata)
            }
            case _ => Empty
          }
        }
      })
    }
    case in => genericSetFromAny(in)
  }

  def setFromString(s: String): Box[Array[Byte]] = s match {
    case "" if optional_? => {
      _id.synchronized {
        _file_name = Empty
        _bindata = Empty
        _file_length = Empty
      }
      setBox(Empty)
    }
    case _ => {
      _id.synchronized {
        _file_name = Empty
        _bindata = Full(s.getBytes("UTF-8"))
        _file_length = _bindata.map(_.length)
        setBox(_bindata)
      }
    }
  }

  //TODO: add to form with progress bar
  def toForm: Box[NodeSeq] = Empty

  implicit private def stream_to_array(is: InputStream): Array[Byte] =
      (Stream.continually(is.read).takeWhile(-1 !=).map(_.toByte).toArray)

  def asJs = valueBox.map(v => Str(hexEncode(v))) openOr JsNull

  def asJValue = asJString(v => base64Encode(v))
  def setFromJValue(jvalue: JValue) = setFromJString(jvalue)(s => tryo(new ByteArrayInputStream(base64Decode(s))))

  protected def getStreamBox(oid: ObjectId): Box[InputStream] = {
    _bindata match {
      case Full(bd) => Full(new ByteArrayInputStream(bd))
      case _ => MongoDB.use(DefaultMongoIdentifier) ( db => {
		    val fs = new GridFS(db)
		
		    fs.findOne(oid) match {
		      case file:GridFSDBFile => _id.synchronized {
		        val stream = Full(file.getInputStream)
		        _file_name = Full(file.getFilename)
		        _file_length = Full(file.getLength)
		        _id = Full(oid)
		        stream
		      }
		      case _ => Empty
		    }
		  })
    }
  }
  
  protected def checkExists(oid: ObjectId) = MongoDB.use(DefaultMongoIdentifier) ( db => {
    val fs = new GridFS(db)

    fs.findOne(oid) match {
      case file:GridFSDBFile => true
      case _ => false
    }
  })
  
  protected def getFileName(oid: ObjectId): Box[String] = {
    _file_name match {
      case Full(fn) => Full(fn)
      case _ => MongoDB.use(DefaultMongoIdentifier) ( db => {
        val fs = new GridFS(db)
    
        fs.findOne(oid) match {
          case file:GridFSDBFile => _id.synchronized {
            _file_length = Full(file.getLength)
            Full(file.getFilename)
          }
          case _ => Empty
        }
      })
    }
  }

  protected def save(node: Box[Node]): Unit = _id.synchronized {
    if (!dirty_?) return

    val new_file = MongoDB.use(DefaultMongoIdentifier) ( db => {
      val is = new ByteArrayInputStream(_bindata.get)
      val fs = new GridFS(db)

      val f = _file_name match {
        case Full(f_name) => fs.createFile(is, f_name)
        case _ => fs.createFile(is)
      }
      content_type.foreach(ct => f.setContentType(ct))
      f.save
      is.close
      f
    })
    _id = Full(new_file.getId.asInstanceOf[ObjectId])

    node.foreach( _(name) = _id.get.toByteArray())
  }
}

class Neo4jBinaryField[OwnerType <: Neo4jRecord[OwnerType]](rec: OwnerType)
    extends Neo4jField[Array[Byte], OwnerType] with MandatoryTypedField[Array[Byte]] with ByteArrayTypedField {

  def owner = rec

  def this(rec: OwnerType, value: Array[Byte]) = {
    this(rec)
    set(value)
  }

  def defaultValue = Array[Byte]()

  //override def validate: List[FieldError] = List()
  
  override def valueBox: Box[MyType] = synchronized {
    /*
    if (owner.node.isEmpty) _bindata//owner.save
    else getNodeValueBox
    */
    getNodeValueBox
  }
  
  /*
  def getStream() = synchronized {
    //if (owner.node.isEmpty) owner.save
    getInputStreamBox.get
  }
  */

  override protected def getNodeValueBox: Box[Array[Byte]] = getInputStreamBox.map(IOUtils.toByteArray(_))
  
  def getInputStreamBox: Box[InputStream] = owner.node match {
    case Full(n) => _id match {
      case Full(oid) => getStreamBox(oid)
      case _ => {
        for (oid_bytes <- n(name)) {
          return getStreamBox(new ObjectId(oid_bytes.asInstanceOf[Array[Byte]]))
        }
        Empty
      }
    }
    case _ => return getStreamBox(new ObjectId())
  }
  
  def checkFileExists: Boolean = owner.node match {
    case Full(n) => _id match {
      case Full(oid) => checkExists(oid)
      case _ => {
        for (oid_bytes <- n(name)) {
          return checkExists(new ObjectId(oid_bytes.asInstanceOf[Array[Byte]]))
        }
        false
      }
    }
    case _ => return false
  }
  
  def readFileName: Box[String] = owner.node match {
    case Full(n) => _id match {
      case Full(oid) => getFileName(oid)
      case _ => {
        for (oid_bytes <- n(name)) {
          return getFileName(new ObjectId(oid_bytes.asInstanceOf[Array[Byte]]))
        }
        Empty
      }
    }
    case _ => return getFileName(new ObjectId())
  }
  
  def getObjectId: Box[ObjectId] = owner.node match {
    case Full(n) => _id match {
      case Full(oid) => Full(oid)
      case _ => {
        for (oid_bytes <- n(name)) {
          return Full(new ObjectId(oid_bytes.asInstanceOf[Array[Byte]]))
        }
        Empty
      }
    }
    case _ => Empty
  }
  
  def copy_to_file(file: File): Boolean = _id.synchronized {
    getInputStreamBox match {
      case Full(in) => {
        val out = new FileOutputStream(file)
		    IOUtils.copy(in, out)
		    out.close
		    in.close
		    true
      }
      case _ => false
    }
  }
  
  def copy_to_folder(folder: File): Boolean = _id.synchronized {
    getInputStreamBox match {
      case Full(in) => {
        val file = new File(folder.getAbsolutePath + java.lang.System.getProperty("file.separator") + file_name.get)
        if (!file.exists) file.createNewFile
        val out = new FileOutputStream(file)
		    IOUtils.copy(in, out)
		    out.close
		    in.close
		    true
      }
      case _ => false
    }
  }

  override protected def propToBoxMyType(p: Any): Box[MyType] = Empty

  def save: Unit = save(owner.node)
}