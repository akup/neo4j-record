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

import org.neo4j.graphdb.Node

import net.liftweb.common.{Box, Empty, Failure, Full}
import java.math.MathContext
import scala.xml.Text

import net.liftweb.record._
import field._
import record.field._

import org.neo4j.scala.Neo4jImplicits._
import net.liftweb.util.{Helpers, FieldError}

object MyTestEnum extends Enumeration {
  val ONE = Value("ONE")
  val TWO = Value("TWO")
  val THREE = Value("THREE")
}

trait HarnessedLifecycleCallbacks extends LifecycleCallbacks {
  this: BaseField =>

  var beforeValidationHarness: () => Unit = () => ()
  override def beforeValidation = beforeValidationHarness()
  var afterValidationHarness: () => Unit = () => ()
  override def afterValidation = afterValidationHarness()

  var beforeSaveHarness: () => Unit = () => ()
  override def beforeSave = beforeSaveHarness()
  var beforeCreateHarness: () => Unit = () => ()
  override def beforeCreate = beforeCreateHarness()
  var beforeUpdateHarness: () => Unit = () => ()
  override def beforeUpdate = beforeUpdateHarness()

  var afterSaveHarness: () => Unit = () => ()
  override def afterSave = afterSaveHarness()
  var afterCreateHarness: () => Unit = () => ()
  override def afterCreate = afterCreateHarness()
  var afterUpdateHarness: () => Unit = () => ()
  override def afterUpdate = afterUpdateHarness()

  var beforeDeleteHarness: () => Unit = () => ()
  override def beforeDelete = beforeDeleteHarness()
  var afterDeleteHarness: () => Unit = () => ()
  override def afterDelete = afterDeleteHarness()
}

class FieldTypeTestRecord protected () extends Neo4jRecord[FieldTypeTestRecord] with Neo4jId[FieldTypeTestRecord] {
  def meta = FieldTypeTestRecord

  //object mandatoryBinaryField extends BinaryField(this)
  //object legacyOptionalBinaryField extends BinaryField(this) { override def optional_? = true }
  //object optionalBinaryField extends OptionalBinaryField(this)

  object mandatoryBooleanField extends Neo4jBooleanField(this)
  object legacyOptionalBooleanField extends Neo4jBooleanField(this) { override def optional_? = true }
  object optionalBooleanField extends Neo4jOptionalBooleanField(this)

  object mandatoryCountryField extends Neo4jCountryField(this)
  object legacyOptionalCountryField extends Neo4jCountryField(this) { override def optional_? = true }
  object optionalCountryField extends Neo4jOptionalCountryField(this)

  object mandatoryDateTimeField extends Neo4jDateTimeField(this)
  object legacyOptionalDateTimeField extends Neo4jDateTimeField(this) { override def optional_? = true }
  object optionalDateTimeField extends Neo4jOptionalDateTimeField(this)

  object mandatoryDateField extends Neo4jDateField(this)
  object legacyOptionalDateField extends Neo4jDateField(this) { override def optional_? = true }
  object optionalDateField extends Neo4jOptionalDateField(this)

  object mandatoryFloatField extends Neo4jFloatField(this)
  object legacyOptionalFloatField extends Neo4jFloatField(this) { override def optional_? = true }
  object optionalFloatField extends Neo4jOptionalFloatField(this)

  object mandatoryDoubleField extends Neo4jDoubleField(this)
  object legacyOptionalDoubleField extends Neo4jDoubleField(this) { override def optional_? = true }
  object optionalDoubleField extends Neo4jOptionalDoubleField(this)

  object mandatoryEmailField extends Neo4jEmailField(this, 100)
  object legacyOptionalEmailField extends Neo4jEmailField(this, 100) { override def optional_? = true }
  object optionalEmailField extends Neo4jOptionalEmailField(this, 100)

  object mandatoryEnumField extends Neo4jEnumField(this, MyTestEnum)
  object legacyOptionalEnumField extends Neo4jEnumField(this, MyTestEnum) { override def optional_? = true }
  object optionalEnumField extends Neo4jOptionalEnumField(this, MyTestEnum)

  object mandatoryEnumNameField extends Neo4jEnumNameField(this, MyTestEnum)
  object legacyOptionalEnumNameField extends Neo4jEnumNameField(this, MyTestEnum) { override def optional_? = true }
  object optionalEnumNameField extends Neo4jOptionalEnumNameField(this, MyTestEnum)

  object mandatoryIntField extends Neo4jIntField(this)
  object legacyOptionalIntField extends Neo4jIntField(this) { override def optional_? = true }
  object optionalIntField extends Neo4jOptionalIntField(this)

  object mandatoryLocaleField extends Neo4jLocaleField(this)
  object legacyOptionalLocaleField extends Neo4jLocaleField(this) { override def optional_? = true }
  object optionalLocaleField extends Neo4jOptionalLocaleField(this)

  object mandatoryLongField extends Neo4jLongField(this)
  object legacyOptionalLongField extends Neo4jLongField(this) { override def optional_? = true }
  object optionalLongField extends Neo4jOptionalLongField(this)

  // FIXME would be nice to have some of these PostalCode fields depend on an OptionalCountryField, but the type sig of
  // PostalCodeField does not yet allow it.
  object mandatoryPostalCodeField extends Neo4jPostalCodeField(this, mandatoryCountryField)
  object legacyOptionalPostalCodeField extends Neo4jPostalCodeField(this, mandatoryCountryField) { override def optional_? = true }
  object optionalPostalCodeField extends Neo4jOptionalPostalCodeField(this, mandatoryCountryField)

  object mandatoryStringField extends Neo4jStringField(this, 100)
  object legacyOptionalStringField extends Neo4jStringField(this, 100) { override def optional_? = true }
  object optionalStringField extends Neo4jOptionalStringField(this, 100)

  object mandatoryTextareaField extends Neo4jTextareaField(this, 100)
  object legacyOptionalTextareaField extends Neo4jTextareaField(this, 100) { override def optional_? = true }
  object optionalTextareaField extends Neo4jOptionalTextareaField(this, 100)

  object mandatoryTimeZoneField extends Neo4jTimeZoneField(this)
  object legacyOptionalTimeZoneField extends Neo4jTimeZoneField(this) { override def optional_? = true }
  object optionalTimeZoneField extends Neo4jOptionalTimeZoneField(this)

  override def equals(other: Any): Boolean = other match {
    case that:FieldTypeTestRecord =>
      //this.mandatoryBinaryField.value == that.mandatoryBinaryField.value &&
      this._id.value == that._id.value &&
      this.mandatoryBooleanField.value == that.mandatoryBooleanField.value &&
      this.mandatoryCountryField.value == that.mandatoryCountryField.value &&
      this.mandatoryDateTimeField.value == that.mandatoryDateTimeField.value &&
      this.mandatoryDateField.value == that.mandatoryDateField.value &&
      this.mandatoryFloatField.value == that.mandatoryFloatField.value &&
      this.mandatoryDoubleField.value == that.mandatoryDoubleField.value &&
      this.mandatoryEmailField.value == that.mandatoryEmailField.value &&
      this.mandatoryEnumField.value == that.mandatoryEnumField.value &&
      this.mandatoryEnumNameField.value == that.mandatoryEnumNameField.value &&
      this.mandatoryIntField.value == that.mandatoryIntField.value &&
      this.mandatoryLocaleField.value == that.mandatoryLocaleField.value &&
      this.mandatoryLongField.value == that.mandatoryLongField.value &&
      this.mandatoryPostalCodeField.value == that.mandatoryPostalCodeField.value &&
      this.mandatoryStringField.value == that.mandatoryStringField.value &&
      this.mandatoryTextareaField.value == that.mandatoryTextareaField.value &&
      this.mandatoryTimeZoneField.value == that.mandatoryTimeZoneField.value
    case _ => false
  }
}

object FieldTypeTestRecord extends FieldTypeTestRecord with Neo4jMetaRecord[FieldTypeTestRecord] {
  override def nodeGroupName = "TestsN4jFieldType"
}

class FieldTypeTestRecordInd1 protected () extends Neo4jRecord[FieldTypeTestRecordInd1] with Neo4jId[FieldTypeTestRecordInd1] {
  def meta = FieldTypeTestRecordInd1

  object mandatoryBooleanField extends Neo4jBooleanField(this)
  object legacyOptionalBooleanField extends Neo4jBooleanField(this) { override def optional_? = true }
  object optionalBooleanField extends Neo4jOptionalBooleanField(this)

  object mandatoryCountryField extends Neo4jCountryField(this)
  object legacyOptionalCountryField extends Neo4jCountryField(this) { override def optional_? = true }
  object optionalCountryField extends Neo4jOptionalCountryField(this)

  override def equals(other: Any): Boolean = other match {
    case that:FieldTypeTestRecordInd1 =>
      //this.mandatoryBinaryField.value == that.mandatoryBinaryField.value &&
      this._id.value == that._id.value &&
      this.mandatoryBooleanField.value == that.mandatoryBooleanField.value &&
      this.mandatoryCountryField.value == that.mandatoryCountryField.value
    case _ => false
  }
}

object FieldTypeTestRecordInd1 extends FieldTypeTestRecordInd1 with Neo4jMetaRecord[FieldTypeTestRecordInd1] {
  override def nodeGroupName = "TestsN4jFieldTypeInd1"

  override def dbIndNames = "_id" :: super.dbIndNames
}

class FieldTypeTestRecordInd2 protected () extends Neo4jRecord[FieldTypeTestRecordInd2] with Neo4jId[FieldTypeTestRecordInd2] {
  def meta = FieldTypeTestRecordInd2

  object mandatoryBooleanField extends Neo4jBooleanField(this)
  object legacyOptionalBooleanField extends Neo4jBooleanField(this) { override def optional_? = true }
  object optionalBooleanField extends Neo4jOptionalBooleanField(this)

  object mandatoryCountryField extends Neo4jCountryField(this)
  object legacyOptionalCountryField extends Neo4jCountryField(this) { override def optional_? = true }
  object optionalCountryField extends Neo4jOptionalCountryField(this)

  override def equals(other: Any): Boolean = other match {
    case that:FieldTypeTestRecordInd2 =>
      //this.mandatoryBinaryField.value == that.mandatoryBinaryField.value &&
      this._id.value == that._id.value &&
      this.mandatoryBooleanField.value == that.mandatoryBooleanField.value &&
      this.mandatoryCountryField.value == that.mandatoryCountryField.value
    case _ => false
  }
}

object FieldTypeTestRecordInd2 extends FieldTypeTestRecordInd2 with Neo4jMetaRecord[FieldTypeTestRecordInd2] {
  override def nodeGroupName = "TestsN4jFieldTypeInd2"

  override def dbIndNames = "_id" :: super.dbIndNames
}

class BinFieldTestRecord protected() extends Neo4jRecord[BinFieldTestRecord] with Neo4jIndId[BinFieldTestRecord] {
  def meta = BinFieldTestRecord

  object mandatoryBinField extends Neo4jBinaryField(this)
  object legacyOptionalBinField extends Neo4jBinaryField(this) { override def optional_? = true }
  //object optionalBinField extends Neo4jOptionalBinaryField(this)

  override def equals(other: Any): Boolean = other match {
    case that:BinFieldTestRecord =>
      this._id.value == that._id.value &&
      this.mandatoryBinField.id == that.mandatoryBinField.id
    case _ => false
  }
}

object BinFieldTestRecord extends BinFieldTestRecord with Neo4jMetaRecord[BinFieldTestRecord] {
  override def nodeGroupName = "TestsN4jBinField"
}

class LifecycleTestRecord private () extends Neo4jRecord[LifecycleTestRecord] {
  def meta = LifecycleTestRecord

  def foreachCallback(f: LifecycleCallbacks => Any): Unit =
    meta.foreachCallback(this, f)

  object stringFieldWithCallbacks extends Neo4jStringField(this, 100) with HarnessedLifecycleCallbacks
}

object LifecycleTestRecord extends LifecycleTestRecord with Neo4jMetaRecord[LifecycleTestRecord] {
  // without this, the Scala 2.7 compiler panics, so don't blame me if you remove it and it's confusing!
  override def foreachCallback(inst: LifecycleTestRecord, f: LifecycleCallbacks => Any) = super.foreachCallback(inst, f)

  override def nodeGroupName = "TestsLifecycleN4jFieldType"
}

class LinkTestRecord extends Neo4jRecord[LinkTestRecord] {
  def meta = LinkTestRecord

  object links extends Neo4jLink(this, "link1", FieldTypeTestRecordInd1)

  object links_guarded extends Neo4jLink(this, "link2", FieldTypeTestRecordInd1) {
    override protected def guard(from_node: Node, to_node: Node, params: Map[String, Any]) = {
	    var ret = false
	    for (v <- to_node("mandatoryBooleanField")) {
	      v match {
	        case b: Boolean => ret = b
	        case _ =>
	      }
	    }
	    ret
    }
    
    override protected def guard(to_rec: FieldTypeTestRecordInd1, params: Map[String, Any]) = {
      var ret = false
      to_rec.mandatoryBooleanField.get match {
        case b: Boolean => ret = b
        case _ =>
      }
      ret
    }
  }
}

object LinkTestRecord extends LinkTestRecord with Neo4jMetaRecord[LinkTestRecord] {
  override def nodeGroupName = "LinkTestN4j"
}

class LinkTestIndRecord extends Neo4jRecord[LinkTestIndRecord] {
  def meta = LinkTestIndRecord

  object links_ind extends Neo4jLink(this, "link1", FieldTypeTestRecordInd1) {
    lazy val rand = Helpers.nextFuncName
    override protected def index_name = "indexed_links_" + rand
      
    override lazy val ind_props = "country" :: Nil
  }
}

object LinkTestIndRecord extends LinkTestIndRecord with Neo4jMetaRecord[LinkTestIndRecord] {
  override def nodeGroupName = "LinkTestIndN4j"
}

class One2ManyLinkTestRecord extends Neo4jRecord[One2ManyLinkTestRecord] {
  def meta = One2ManyLinkTestRecord
    
  object links extends Neo4jOne2ManyLink(this, "link1", FieldTypeTestRecordInd1)
  
  object guarded_link extends Neo4jOne2ManyLink(this, "link2", FieldTypeTestRecordInd1) {
    override protected def single_params: Map[String, List[Any]] = Map("index" -> Nil)
  }
  
  object guarded_specified extends Neo4jOne2ManyLink(this, "link2", FieldTypeTestRecordInd1) {
    override protected def single_params: Map[String, List[Any]] = Map("index" -> ("index1" :: "index2" :: "index6" :: Nil))
  }
}

object One2ManyLinkTestRecord extends One2ManyLinkTestRecord with Neo4jMetaRecord[One2ManyLinkTestRecord] {
  override def nodeGroupName = "One2ManyLinkTestN4j"
}

class SingleLinkTestRecord extends Neo4jRecord[SingleLinkTestRecord] {
  def meta = SingleLinkTestRecord
    
  object links extends Neo4jSingleLink(this, "link1", FieldTypeTestRecordInd1)
}

object SingleLinkTestRecord extends SingleLinkTestRecord with Neo4jMetaRecord[SingleLinkTestRecord] {
  override def nodeGroupName = "SingleLinkTestN4j"
}

class SingleLinkGuardedTestRecord extends Neo4jRecord[SingleLinkGuardedTestRecord] {
  def meta = SingleLinkGuardedTestRecord
    
  object links extends Neo4jSingleLink(this, "link1", FieldTypeTestRecordInd1)((n , f_n) => {
        var ret = false
        for (v <- f_n("mandatoryBooleanField")) {
          v match {
            case b: Boolean => ret = b
            case _ =>
          }
        }
        ret
      },
      (r_n, l) => {
        var ret = false
        l._1.mandatoryBooleanField.get match {
          case b: Boolean => ret = b
          case _ =>
        }
        ret
      })
}

object SingleLinkGuardedTestRecord extends SingleLinkGuardedTestRecord with Neo4jMetaRecord[SingleLinkGuardedTestRecord] {
  override def nodeGroupName = "SingleLinkGuardedTestN4j"
}