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

import org.specs._
import org.specs.runner._
import org.specs.matcher.Matcher
import org.specs.runner.JUnit4
import net.liftweb.record.field._
import java.util.{GregorianCalendar, Date}
import org.neo4j.scala.Neo4jImplicits._
import field.Neo4jLink._
import org.neo4j.graphdb._
import net.liftweb.common.{Full, Empty}
import java.io.ByteArrayInputStream
import net.liftweb.http._
import net.liftweb.util.Helpers

class Neo4jRecordSpecTest extends JUnit4(Neo4jRecordSpec)

object Neo4jRecordSpec extends Specification("Neo4j Specification") with ScalaCheck with Neo4jTestKit {

  "Neo4jRecord field introspection" should {
    checkNeo4jIsRunning

    val rec = FieldTypeTestRecord.createRecord
    val allExpectedFieldNames: List[String] = "_id" :: (for {
      typeName <- "Boolean Country DateTime Date Float Double Email Enum EnumName Int Locale Long PostalCode String Textarea TimeZone".split(" ")
      flavor <- "mandatory legacyOptional optional".split(" ")
    } yield flavor + typeName + "Field").toList

    "introspect only the expected fields" in {
      rec.fields().map(_.name).sortWith(_ < _) must_== allExpectedFieldNames.sortWith(_ < _)
    }

    "correctly look up fields by name" in {
      val session = new LiftSession("", Helpers.nextFuncName, Empty)
      //init S for countries (S.?)
      S.initIfUninitted(session) {
        for (name <- allExpectedFieldNames) {
          rec.fieldByName(name) must verify(_.isDefined)
        }
      }
    }

    "not look up fields by bogus names" in {
      for (name <- allExpectedFieldNames) {
        rec.fieldByName("x" + name + "y") must not(verify(_.isDefined))
      }
    }
  }

  "Neo4jRecord lifecycle callbacks" should {
    checkNeo4jIsRunning

    def testOneHarness(scope: String, f: LifecycleTestRecord => HarnessedLifecycleCallbacks): Unit = {
      ("be called before validation when specified at " + scope) in {
        val rec = LifecycleTestRecord.createRecord
        var triggered = false
        f(rec).beforeValidationHarness = () => triggered = true
        rec.foreachCallback(_.beforeValidation)
        triggered must_== true
      }

      ("be called after validation when specified at " + scope) in {
        val rec = LifecycleTestRecord.createRecord
        var triggered = false
        f(rec).afterValidationHarness = () => triggered = true
        rec.foreachCallback(_.afterValidation)
        triggered must_== true
      }

      ("be called around validate when specified at " + scope) in {
        val rec = LifecycleTestRecord.createRecord
        var triggeredBefore = false
        var triggeredAfter = false
        f(rec).beforeValidationHarness = () => triggeredBefore = true
        f(rec).afterValidationHarness = () => triggeredAfter = true
        rec.validate must_== Nil
        triggeredBefore must_== true
        triggeredAfter must_== true
      }

      ("be called before save when specified at " + scope) in {
        val rec = LifecycleTestRecord.createRecord
        var triggered = false
        f(rec).beforeSaveHarness = () => triggered = true
        rec.foreachCallback(_.beforeSave)
        triggered must_== true
      }

      ("be called before create when specified at " + scope) in {
        val rec = LifecycleTestRecord.createRecord
        var triggered = false
        f(rec).beforeCreateHarness = () => triggered = true
        rec.foreachCallback(_.beforeCreate)
        triggered must_== true
      }

      ("be called before update when specified at " + scope) in {
        val rec = LifecycleTestRecord.createRecord
        var triggered = false
        f(rec).beforeUpdateHarness = () => triggered = true
        rec.foreachCallback(_.beforeUpdate)
        triggered must_== true
      }

      ("be called after save when specified at " + scope) in {
        val rec = LifecycleTestRecord.createRecord
        var triggered = false
        f(rec).afterSaveHarness = () => triggered = true
        rec.foreachCallback(_.afterSave)
        triggered must_== true
      }

      ("be called after create when specified at " + scope) in {
        val rec = LifecycleTestRecord.createRecord
        var triggered = false
        f(rec).afterCreateHarness = () => triggered = true
        rec.foreachCallback(_.afterCreate)
        triggered must_== true
      }

      ("be called after update when specified at " + scope) in {
        val rec = LifecycleTestRecord.createRecord
        var triggered = false
        f(rec).afterUpdateHarness = () => triggered = true
        rec.foreachCallback(_.afterUpdate)
        triggered must_== true
      }

      ("be called before delete when specified at " + scope) in {
        val rec = LifecycleTestRecord.createRecord
        var triggered = false
        f(rec).beforeDeleteHarness = () => triggered = true
        rec.foreachCallback(_.beforeDelete)
        triggered must_== true
      }

      ("be called after delete when specified at " + scope) in {
        val rec = LifecycleTestRecord.createRecord
        var triggered = false
        f(rec).afterDeleteHarness = () => triggered = true
        rec.foreachCallback(_.afterDelete)
        triggered must_== true
      }
    }

    testOneHarness("the field level", rec => rec.stringFieldWithCallbacks: HarnessedLifecycleCallbacks)
  }

  "Neo4jRecord" should {
    checkNeo4jIsRunning

    val fttr = FieldTypeTestRecord.createRecord
        .mandatoryBooleanField(false)
        .mandatoryCountryField(Countries.USA)
        .mandatoryDateTimeField(new GregorianCalendar)
        .mandatoryDateField(new Date)
        .mandatoryFloatField(3.14f)
        .mandatoryDoubleField(1999)
        .mandatoryEmailField("test@liftweb.net")
        .mandatoryEnumField(MyTestEnum.ONE)
        .mandatoryEnumNameField(MyTestEnum.TWO)
        .mandatoryIntField(99)
        .mandatoryLocaleField("en_US")
        .mandatoryLongField(100L)
        .mandatoryPostalCodeField("55401")
        .mandatoryStringField("string")
        .mandatoryTextareaField("string")
        .mandatoryTimeZoneField("America/Chicago")

    val fttrInd = FieldTypeTestRecordInd2.createRecord
        .mandatoryBooleanField(false)
        .mandatoryCountryField(Countries.USA)

    "save and retrieve 'standard' type fields" in {
      Neo4jDB.use(DefaultNeo4jIdentifier)( db =>
        execInNeo4j(d => {
          val session = new LiftSession("", Helpers.nextFuncName, Empty)
          //init S for countries (S.?)
          S.initIfUninitted(session) {
            fttr.save()
  
            val fttrFromDb = FieldTypeTestRecord.find(fttr.id)
            fttrFromDb must notBeEmpty
            fttrFromDb foreach { tr =>
              tr mustEqual fttr
            }
          }
        })(db)
      )
    }

    "save index and retrieve by index 'standart' type fields" in {
      Neo4jDB.use(DefaultNeo4jIdentifier)( db =>
        execInNeo4j(d => {
          val session = new LiftSession("", Helpers.nextFuncName, Empty)
          //init S for countries (S.?)
          S.initIfUninitted(session) {
            fttrInd.save()
  
            val fttrIndFromDb = FieldTypeTestRecordInd2.find(fttrInd.id)
            fttrIndFromDb must notBeEmpty
            fttrIndFromDb foreach { tr =>
              tr mustEqual fttrInd
            }
          }
        })(db)
      )
    }
  }
}