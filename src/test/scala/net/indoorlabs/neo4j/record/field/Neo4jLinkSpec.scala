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

import org.specs._
import org.specs.runner._
import org.specs.matcher.Matcher
import org.specs.runner.JUnit4
import org.neo4j.graphdb._
import net.liftweb.common.{Full, Empty}
import org.neo4j.scala.Neo4jImplicits._
import net.liftweb.record.field.Countries
import net.liftweb.http._
import net.liftweb.util.Helpers

class Neo4jLinkSpecTest extends JUnit4(Neo4jLinkSpec)

object Neo4jLinkSpec extends Specification("Neo4j Specification") with ScalaCheck with Neo4jTestKit {
  /*
  doBeforeSpec {
    val session = new LiftSession("", Helpers.nextFuncName, Empty)
    S.initIfUninitted(session)
  }
  */

  "Neo4jLink" should {
    checkNeo4jIsRunning

    val link_test = LinkTestRecord.createRecord
    
    val link_test_one2many = One2ManyLinkTestRecord.createRecord
    
    val link_test_single = SingleLinkTestRecord.createRecord
    val link_test_single_guarded = SingleLinkGuardedTestRecord.createRecord
    
    val link_ind_test = LinkTestIndRecord.createRecord

    val usa = FieldTypeTestRecordInd1.createRecord.
      mandatoryBooleanField(true).
      mandatoryCountryField(Countries.USA)

    val australia = FieldTypeTestRecordInd1.createRecord.
      mandatoryBooleanField(false).
      mandatoryCountryField(Countries.Australia)

    val canada = FieldTypeTestRecordInd1.createRecord.
      mandatoryBooleanField(false).
      mandatoryCountryField(Countries.Canada)

    val sweden = FieldTypeTestRecordInd1.createRecord.
      mandatoryBooleanField(true).
      mandatoryCountryField(Countries.Sweden)
      
    def makeReq = new Req(Req.NilPath, "", GetRequest, Empty, null,
                    System.nanoTime, System.nanoTime, false,
                    () => ParamCalcInfo(Nil, Map.empty, Nil, Empty), Map())

    "correctly set links unimmediately" in {
      Neo4jDB.use(DefaultNeo4jIdentifier)( db =>
        execInNeo4j(d => {
          link_test.links += (usa, Map("country" -> "usa", "index" -> "index1") )
          link_test.links += (australia, Map("country" -> "australia", "index" -> "index2"))
          link_test.links += (canada, Map("country" -> "canada", "index" -> "index3"))
          link_test.links += (sweden, Map("country" -> "sweden", "index" -> "index4"))

          link_test.links.rels.toList must beEmpty

          link_test.save()

          val ls1 = link_test.links.rels.toList
          val list1 = ls1.map(_("country") getOrElse "").toList
          val list2 = ls1.map(_("index") getOrElse "").toList
          list1 must_== List("usa", "australia", "canada", "sweden")
          list2 must_== List("index1", "index2", "index3", "index4")
        })(db)
      )
    }
    
    "correctly set links immediately" in {
      Neo4jDB.use(DefaultNeo4jIdentifier)( db =>
        execInNeo4j(d => {
          (link_test.links += (usa, Map("country" -> "usa", "index" -> "index1"), true )) must throwA(new Exception("No n4j nodes to link"))
          
          link_test.save()
          
          link_test.links += (usa, Map("country" -> "usa", "index" -> "index1"), true )
          link_test.links += (australia, Map("country" -> "australia", "index" -> "index2"), true)
          link_test.links += (canada, Map("country" -> "canada", "index" -> "index3"), true)
          link_test.links += (sweden, Map("country" -> "sweden", "index" -> "index4"), true)

          val ls = link_test.links.rels.toList

          val list1 = ls.map(_("country") getOrElse "").toList
          val list2 = ls.map(_("index") getOrElse "").toList
          list1 must_== List("usa", "australia", "canada", "sweden")
          list2 must_== List("index1", "index2", "index3", "index4")
        })(db)
      )
    }
    
    "correctly set guarded links unimmediately" in {
      Neo4jDB.use(DefaultNeo4jIdentifier)( db =>
        execInNeo4j(d => {
          link_test.links_guarded += (usa, Map("country" -> "usa", "index" -> "index1") )
          (link_test.links_guarded += (australia, Map("country" -> "australia", "index" -> "index2"))) must throwA(new GuardedException("guarded"))
          (link_test.links_guarded += (canada, Map("country" -> "canada", "index" -> "index3"))) must throwA(new GuardedException("guarded"))
          link_test.links_guarded += (sweden, Map("country" -> "sweden", "index" -> "index4"))

          link_test.links_guarded.rels.toList must beEmpty//) must throwA(new Exception("No n4j from node"))

          link_test.save
          
          val ls1 = link_test.links_guarded.rels.toList
          val list1 = ls1.map(_("country") getOrElse "").toList
          val list2 = ls1.map(_("index") getOrElse "").toList
          list1 must_== List("usa", "sweden")
          list2 must_== List("index1", "index4")
        })(db)
      )
    }

    "correctly set guarded links immediately" in {
      Neo4jDB.use(DefaultNeo4jIdentifier)( db =>
        execInNeo4j(d => {
          link_test.save()

          link_test.links_guarded += (usa, Map("country" -> "usa", "index" -> "index1"), true )
          (link_test.links_guarded += (australia, Map("country" -> "australia", "index" -> "index2"), true)) must throwA(new GuardedException("guarded"))
          (link_test.links_guarded += (canada, Map("country" -> "canada", "index" -> "index3"), true)) must throwA(new GuardedException("guarded"))
          link_test.links_guarded += (sweden, Map("country" -> "sweden", "index" -> "index4"), true)

          val ls = link_test.links_guarded.rels.toList

          val list1 = ls.map(_("country") getOrElse "").toList
          val list2 = ls.map(_("index") getOrElse "").toList
          list1 must_== List("usa", "sweden")
          list2 must_== List("index1", "index4")
        })(db)
      )
    }
    
    "correctly add One2ManyLink unimmediately" in {
      Neo4jDB.use(DefaultNeo4jIdentifier)( db =>
        execInNeo4j(d => {
          link_test_one2many.links += (usa, Map("country" -> "usa", "index" -> "index1") )
          link_test_one2many.links += (australia, Map("country" -> "australia", "index" -> "index2"))
          link_test_one2many.links += (canada, Map("country" -> "canada"))
          link_test_one2many.links += (sweden, Map("country" -> "sweden"))
          (link_test_one2many.links += (usa, Map("country" -> "usa", "index" -> "index5"))) must throwA(new GuardedException("guarded"))

          link_test_one2many.guarded_link += (usa, Map("country" -> "usa", "index" -> "index1") )
          (link_test_one2many.guarded_link += (australia, Map("country" -> "usa", "index" -> "index1") )) must throwA(new GuardedException("guarded"))
          link_test_one2many.guarded_link += (australia, Map("country" -> "usa", "index" -> "index2"))
          (link_test_one2many.guarded_link += (canada, Map("country" -> "canada"))) must throwA(new GuardedException("guarded"))
          (link_test_one2many.guarded_link += (sweden, Map("country" -> "sweden"))) must throwA(new GuardedException("guarded"))

          link_test_one2many.guarded_specified += (usa, Map("country" -> "usa", "index" -> "index1") )
          link_test_one2many.guarded_specified += (australia, Map("country" -> "australia", "index" -> "index2"))
          (link_test_one2many.guarded_specified += (canada, Map("country" -> "australia", "index" -> "index3"))) must throwA(new GuardedException("guarded"))
          link_test_one2many.guarded_specified += (sweden, Map("country" -> "australia", "index" -> "index6"))
        })(db)
      )
    }
    
    "correctly add One2ManyLink immediately" in {
      Neo4jDB.use(DefaultNeo4jIdentifier)( db =>
        execInNeo4j(d => {
          link_test_one2many.save
          
          link_test_one2many.links += (usa, Map("country" -> "usa", "index" -> "index1") )
          link_test_one2many.links += (australia, Map("country" -> "australia", "index" -> "index2"))
          link_test_one2many.links += (canada, Map("country" -> "canada"))
          link_test_one2many.links += (sweden, Map("country" -> "sweden"))
          (link_test_one2many.links += (usa, Map("country" -> "usa", "index" -> "index5"))) must throwA(new GuardedException("guarded"))

          link_test_one2many.guarded_link += (usa, Map("country" -> "usa", "index" -> "index1") )
          link_test_one2many.guarded_link += (australia, Map("country" -> "australia", "index" -> "index2"))
          (link_test_one2many.guarded_link += (canada, Map("country" -> "canada"))) must throwA(new GuardedException("guarded"))
          (link_test_one2many.guarded_link += (sweden, Map("country" -> "sweden"))) must throwA(new GuardedException("guarded"))

          link_test_one2many.guarded_specified += (usa, Map("country" -> "usa", "index" -> "index1") )
          link_test_one2many.guarded_specified += (australia, Map("country" -> "australia", "index" -> "index2"))
          (link_test_one2many.guarded_specified += (canada, Map("country" -> "australia", "index" -> "index3"))) must throwA(new GuardedException("guarded"))
          link_test_one2many.guarded_specified += (sweden, Map("country" -> "australia", "index" -> "index6"))
        })(db)
      )
    }

    "correctly delete unimmediatly case1" in {
      Neo4jDB.use(DefaultNeo4jIdentifier)( db =>
        execInNeo4j(d => {
          val session = new LiftSession("", Helpers.nextFuncName, Empty)
          //init S for countries (S.?)
          S.initIfUninitted(session) {
            link_test.links += (usa, Map("country" -> "usa", "index" -> "index1") )
            link_test.links += (australia, Map("country" -> "australia", "index" -> "index2"))
            link_test.links += (canada, Map("country" -> "canada", "index" -> "index3"))
            
            link_test.links.deleteLinks(Map("index" -> "index1"))
            link_test.links.deleteLinks(Map("index" -> "index3"))
            
            link_test.links.get1.map(_.mandatoryCountryField.get.toString).toList must_== List("Australia")
            link_test.links.get1(Map("country" -> "australia")).map(_.mandatoryCountryField.get.toString).toList must_== List("Australia")
            link_test.links.get1(Map("country" -> "sweden")).map(_.mandatoryCountryField.get.toString).toList must beEmpty
            
            link_test.links.get2(List("country", "index")).map(x => x._1.mandatoryCountryField.get.toString -> x._2).toList must_== List(("Australia", List("country" -> "australia", "index" -> "index2")))
            link_test.links.get2(List("country", "index"), Map("country" -> "sweden")).map(x => x._1.mandatoryCountryField.get.toString -> x._2).toList must beEmpty
            
            link_test.links.rels.toList must beEmpty
            
            link_test.save
            
            link_test.links.get2(List("country", "index")).map(x => x._1.mandatoryCountryField.get.toString -> x._2).toList must_== List(("Australia", List("country" -> "australia", "index" -> "index2")))
            link_test.links.get2(List("country", "index"), Map("country" -> "sweden")).map(x => x._1.mandatoryCountryField.get.toString -> x._2).toList must beEmpty
            
            link_test.links.get_cached_linked.map(_._1.mandatoryCountryField.get.toString).toList must beEmpty
            
            val ls = link_test.links.rels.toList
  
            val list1 = ls.map(_("country") getOrElse "").toList
            val list2 = ls.map(_("index") getOrElse "").toList
            list1 must_== List("australia")
            list2 must_== List("index2")
          }
        })(db)
      )
    }
    
    "correctly delete unimmediatly case2" in {
      Neo4jDB.use(DefaultNeo4jIdentifier)( db =>
        execInNeo4j(d => {
          val session = new LiftSession("", Helpers.nextFuncName, Empty)
          //init S for countries (S.?)
          S.initIfUninitted(session) {
            link_test.links += (usa, Map("country" -> "usa", "index" -> "index1") )
            link_test.links += (australia, Map("country" -> "australia", "index" -> "index2"))
            link_test.links += (canada, Map("country" -> "canada", "index" -> "index3"))
            
            link_test.links.deleteLinks(Map("index" -> "index1"))
            link_test.links.deleteLinks(Map("index" -> "index3"))
            
            link_test.links += (usa, Map("country" -> "usa", "index" -> "index1") )
            link_test.links += (canada, Map("country" -> "sweden", "index" -> "index4"))
            
            link_test.links.get1().map(
              _.mandatoryCountryField.get.toString
            ).toList must_== List("Australia", "United States", "Canada")
            link_test.links.get1(Map("country" -> "australia")).map(_.mandatoryCountryField.get.toString).toList must_== List("Australia")
            link_test.links.get1(Map("country" -> "sweden")).map(_.mandatoryCountryField.get.toString).toList must_== List("Canada")
            
            link_test.save
            
            val ls = link_test.links.rels.toList
  
            val list1 = ls.map(_("country") getOrElse "").toList
            val list2 = ls.map(_("index") getOrElse "").toList
            list1 must_== List("australia", "usa", "sweden")
            list2 must_== List("index2", "index1", "index4")
          }
        })(db)
      )
    }
    
    "correctly delete unimmediatly case3" in {
      Neo4jDB.use(DefaultNeo4jIdentifier)( db =>
        execInNeo4j(d => {
          val session = new LiftSession("", Helpers.nextFuncName, Empty)
          //init S for countries (S.?)
          S.initIfUninitted(session) {
            link_test.links += (usa, Map("country" -> "usa", "index" -> "index1") )
            link_test.links += (australia, Map("country" -> "australia", "index" -> "index2"))
            link_test.links += (canada, Map("country" -> "canada", "index" -> "index3"))
            
            link_test.links.deleteLinks(Map("index" -> "index1"))
            link_test.links.deleteLinks(Map("index" -> "index3"))
            
            link_test.links += (usa, Map("country" -> "usa", "index" -> "index1") )
            link_test.links += (canada, Map("country" -> "sweden", "index" -> "index4"))
            
            link_test.links.deleteLinks(Map("index" -> "index4"))
            
            link_test.links.get1().map(_.mandatoryCountryField.get.toString).toList must_== List("Australia", "United States")
            link_test.links.get1(Map("country" -> "australia")).map(_.mandatoryCountryField.get.toString).toList must_== List("Australia")
            link_test.links.get1(Map("country" -> "sweden")) must beEmpty
            
            link_test.save
            
            val ls = link_test.links.rels.toList
  
            val list1 = ls.map(_("country") getOrElse "").toList
            val list2 = ls.map(_("index") getOrElse "").toList
            list1 must_== List("australia", "usa")
            list2 must_== List("index2", "index1")
          }
        })(db)
      )
    }
    
    "correctly delete immediatly case1.1" in {
      Neo4jDB.use(DefaultNeo4jIdentifier)( db =>
        execInNeo4j(d => {
          val session = new LiftSession("", Helpers.nextFuncName, Empty)
          //init S for countries (S.?)
          S.initIfUninitted(session) {
            link_test.save
            
            link_test.links += (usa, Map("country" -> "usa", "index" -> "index1"), true )
            link_test.links += (australia, Map("country" -> "australia", "index" -> "index2"), true)
            link_test.links += (canada, Map("country" -> "canada", "index" -> "index3"), true)
            
            link_test.links.deleteLinks(Map("index" -> "index1"))
            link_test.links.deleteLinks(Map("index" -> "index3"))
            
            link_test.save
            
            val ls = link_test.links.rels.toList
  
            val list1 = ls.map(_("country") getOrElse "").toList
            val list2 = ls.map(_("index") getOrElse "").toList
            list1 must_== List("australia")
            list2 must_== List("index2")
          }
        })(db)
      )
    }
    
    "correctly delete immediatly case1.2" in {
      Neo4jDB.use(DefaultNeo4jIdentifier)( db =>
        execInNeo4j(d => {
          link_test.save
          
          link_test.links += (usa, Map("country" -> "usa", "index" -> "index1"), true )
          link_test.links += (australia, Map("country" -> "australia", "index" -> "index2"), true)
          link_test.links += (canada, Map("country" -> "canada", "index" -> "index3"), true)
          
          link_test.links.deleteLinks(Map("index" -> "index1"), true, true)
          link_test.links.deleteLinks(Map("index" -> "index3"), true, true)
          
          val ls = link_test.links.rels.toList

          val list1 = ls.map(_("country") getOrElse "").toList
          val list2 = ls.map(_("index") getOrElse "").toList
          list1 must_== List("australia")
          list2 must_== List("index2")
        })(db)
      )
    }
    
    "correctly delete immediatly case1.3" in {
      Neo4jDB.use(DefaultNeo4jIdentifier)( db =>
        execInNeo4j(d => {
          val session = new LiftSession("", Helpers.nextFuncName, Empty)
          //init S for countries (S.?)
          S.initIfUninitted(session) {
            link_test.save
            
            link_test.links += (usa, Map("country" -> "usa", "index" -> "index1"), true )
            link_test.links += (australia, Map("country" -> "australia", "index" -> "index2"), true)
            link_test.links += (canada, Map("country" -> "canada", "index" -> "index3"), true)
            
            link_test.links.deleteLinks(Map("index" -> "index1"))
            link_test.links.deleteLinks(Map("index" -> "index3"))
            
            link_test.links.get1().map(_.mandatoryCountryField.get.toString).toList must_== List("Australia")
            link_test.links.get1(Map("country" -> "australia")).map(_.mandatoryCountryField.get.toString).toList must_== List("Australia")
            link_test.links.get1(Map("country" -> "sweden")) must beEmpty
            
            val ls = link_test.links.rels.toList
  
            val list1 = ls.map(_("country") getOrElse "").toList
            val list2 = ls.map(_("index") getOrElse "").toList
            list1 must_== List("usa", "australia", "canada")
            list2 must_== List("index1", "index2", "index3")
          }
        })(db)
      )
    }
    
    "correctly delete immediatly case2.1" in {
      Neo4jDB.use(DefaultNeo4jIdentifier)( db =>
        execInNeo4j(d => {
          val session = new LiftSession("", Helpers.nextFuncName, Empty)
          //init S for countries (S.?)
          S.initIfUninitted(session) {
            link_test.save
            
            link_test.links += (usa, Map("country" -> "usa", "index" -> "index1"), true )
            link_test.links += (australia, Map("country" -> "australia", "index" -> "index2"), true)
            link_test.links += (canada, Map("country" -> "canada", "index" -> "index3"), true)
            
            link_test.links.deleteLinks(Map("index" -> "index1"))
            link_test.links.deleteLinks(Map("index" -> "index3"))
  
            link_test.links += (FieldTypeTestRecordInd1.createRecord.
                mandatoryBooleanField(true).
                mandatoryCountryField(Countries.USA),
              Map("country" -> "usa", "index" -> "index1"), true )
            link_test.links += (FieldTypeTestRecordInd1.createRecord.
                mandatoryBooleanField(false).
                mandatoryCountryField(Countries.Canada),
              Map("country" -> "sweden", "index" -> "index4"), true)
            
            link_test.save
            
            val ls = link_test.links.rels.toList
  
            val list1 = ls.map(_("country") getOrElse "").toList
            val list2 = ls.map(_("index") getOrElse "").toList
            list1 must_== List("australia", "usa", "sweden")
            list2 must_== List("index2", "index1", "index4")
          }
        })(db)
      )
    }
    
    "correctly delete immediatly case2.2" in {
      Neo4jDB.use(DefaultNeo4jIdentifier)( db =>
        execInNeo4j(d => {
          link_test.save
          
          link_test.links += (usa, Map("country" -> "usa", "index" -> "index1"), true )
          link_test.links += (australia, Map("country" -> "australia", "index" -> "index2"), true)
          link_test.links += (canada, Map("country" -> "canada", "index" -> "index3"), true)
          
          link_test.links.deleteLinks(Map("index" -> "index1"), true, true)
          link_test.links.deleteLinks(Map("index" -> "index3"), true, true)
          
          link_test.links += (FieldTypeTestRecordInd1.createRecord.
              mandatoryBooleanField(true).
              mandatoryCountryField(Countries.USA),
            Map("country" -> "usa", "index" -> "index1"), true )
          link_test.links += (FieldTypeTestRecordInd1.createRecord.
              mandatoryBooleanField(false).
              mandatoryCountryField(Countries.Canada),
            Map("country" -> "sweden", "index" -> "index4"), true)
          
          val ls = link_test.links.rels.toList

          val list1 = ls.map(_("country") getOrElse "").toList
          val list2 = ls.map(_("index") getOrElse "").toList
          list1 must_== List("australia", "usa", "sweden")
          list2 must_== List("index2", "index1", "index4")
        })(db)
      )
    }
    
    "correctly delete immediatly case2.3" in {
      Neo4jDB.use(DefaultNeo4jIdentifier)( db =>
        execInNeo4j(d => {
          link_test.save
          
          link_test.links += (usa, Map("country" -> "usa", "index" -> "index1"), true )
          link_test.links += (australia, Map("country" -> "australia", "index" -> "index2"), true)
          link_test.links += (canada, Map("country" -> "canada", "index" -> "index3"), true)
          
          link_test.links.deleteLinks(Map("index" -> "index1"), true, true)
          link_test.links.deleteLinks(Map("index" -> "index3"), true, true)
          
          link_test.links += (FieldTypeTestRecordInd1.createRecord.
              mandatoryBooleanField(true).
              mandatoryCountryField(Countries.USA),
            Map("country" -> "usa", "index" -> "index1") )
          link_test.links += (FieldTypeTestRecordInd1.createRecord.
              mandatoryBooleanField(false).
              mandatoryCountryField(Countries.Canada),
            Map("country" -> "sweden", "index" -> "index4"))
          
          link_test.save
          
          val ls = link_test.links.rels.toList

          val list1 = ls.map(_("country") getOrElse "").toList
          val list2 = ls.map(_("index") getOrElse "").toList
          list1 must_== List("australia", "usa", "sweden")
          list2 must_== List("index2", "index1", "index4")
        })(db)
      )
    }
    
    "correctly delete immediatly case2.4" in {
      Neo4jDB.use(DefaultNeo4jIdentifier)( db =>
        execInNeo4j(d => {
          link_test.save
          
          link_test.links += (usa, Map("country" -> "usa", "index" -> "index1"), true )
          link_test.links += (australia, Map("country" -> "australia", "index" -> "index2"), true)
          link_test.links += (canada, Map("country" -> "canada", "index" -> "index3"), true)
          
          link_test.links.deleteLinks(Map("index" -> "index1"))
          link_test.links.deleteLinks(Map("index" -> "index3"))
          
          link_test.links += (FieldTypeTestRecordInd1.createRecord.
              mandatoryBooleanField(true).
              mandatoryCountryField(Countries.USA),
            Map("country" -> "usa", "index" -> "index1") )
          link_test.links += (FieldTypeTestRecordInd1.createRecord.
              mandatoryBooleanField(true).
              mandatoryCountryField(Countries.Canada),
            Map("country" -> "sweden", "index" -> "index4"))
          
          link_test.save
          
          val ls = link_test.links.rels.toList

          val list1 = ls.map(_("country") getOrElse "").toList
          val list2 = ls.map(_("index") getOrElse "").toList
          list1 must_== List("australia", "usa", "sweden")
          list2 must_== List("index2", "index1", "index4")
        })(db)
      )
    }
    
    "correctly delete immediatly case2.5" in {
      Neo4jDB.use(DefaultNeo4jIdentifier)( db =>
        execInNeo4j(d => {
          link_test.save
          
          link_test.links += (usa, Map("country" -> "usa", "index" -> "index1"), true )
          link_test.links += (australia, Map("country" -> "australia", "index" -> "index2"), true)
          link_test.links += (canada, Map("country" -> "canada", "index" -> "index3"), true)
          
          link_test.links.deleteLinks(Map("index" -> "index1"), deleteNodes = false)
          link_test.links.deleteLinks(Map("index" -> "index3"), deleteNodes = false)
          
          link_test.links += (usa, Map("country" -> "usa", "index" -> "index1") )
          link_test.links += (canada, Map("country" -> "sweden", "index" -> "index4"))
          
          link_test.save
          
          val ls = link_test.links.rels.toList

          val list1 = ls.map(_("country") getOrElse "").toList
          val list2 = ls.map(_("index") getOrElse "").toList
          list1 must_== List("australia", "usa", "sweden")
          list2 must_== List("index2", "index1", "index4")
        })(db)
      )
    }
    
    "correctly delete immediatly case3.1" in {
      Neo4jDB.use(DefaultNeo4jIdentifier)( db =>
        execInNeo4j(d => {
          link_test.save
          
          link_test.links += (usa, Map("country" -> "usa", "index" -> "index1"), true )
          link_test.links += (australia, Map("country" -> "australia", "index" -> "index2"), true)
          link_test.links += (canada, Map("country" -> "canada", "index" -> "index3"), true)
          
          link_test.links.deleteLinks(Map("index" -> "index1"), deleteNodes = false)
          link_test.links.deleteLinks(Map("index" -> "index3"), deleteNodes = false)
          
          link_test.links += (usa, Map("country" -> "usa", "index" -> "index1") )
          link_test.links += (canada, Map("country" -> "sweden", "index" -> "index4"))
          
          link_test.links.deleteLinks(Map("index" -> "index4"), true, true)
          
          val ls = link_test.links.rels.toList

          val list1 = ls.map(_("country") getOrElse "").toList
          val list2 = ls.map(_("index") getOrElse "").toList
          list1 must_== List("australia", "usa")
          list2 must_== List("index2", "index1")
        })(db)
      )
    }
    
    "correctly delete immediatly case3.2" in {
      Neo4jDB.use(DefaultNeo4jIdentifier)( db =>
        execInNeo4j(d => {
          link_test.save
          
          link_test.links += (usa, Map("country" -> "usa", "index" -> "index1"))
          link_test.links += (australia, Map("country" -> "australia", "index" -> "index2"))
          link_test.links += (canada, Map("country" -> "canada", "index" -> "index3"))
          
          link_test.links.deleteLinks(Map("index" -> "index1"), deleteNodes = false)
          link_test.links.deleteLinks(Map("index" -> "index3"), deleteNodes = false)
          
          link_test.links += (usa, Map("country" -> "usa", "index" -> "index1") )
          link_test.links += (canada, Map("country" -> "sweden", "index" -> "index4"))
          
          link_test.links.deleteLinks(Map("index" -> "index4"), true, true)
          
          val ls = link_test.links.rels.toList

          val list1 = ls.map(_("country") getOrElse "").toList
          val list2 = ls.map(_("index") getOrElse "").toList
          list1 must_== List("australia", "usa")
          list2 must_== List("index2", "index1")
        })(db)
      )
    }
    
    "correctly add SingleLink unimmediately" in {
      Neo4jDB.use(DefaultNeo4jIdentifier)( db =>
        execInNeo4j(d => {
          val session = new LiftSession("", Helpers.nextFuncName, Empty)
          //init S for countries (S.?)
          S.initIfUninitted(session) {
            link_test_single.links --> (usa, Map("country" -> "usa", "index" -> "index1") )
            link_test_single.links --> (canada, Map("country" -> "canada", "index" -> "index2") )
            
            link_test_single.save
            
            link_test_single.links.get_saved must notBeEmpty
            link_test_single.links.get_saved.get must_== canada
          }
        })(db)
      )
    }

    "correctly add SingleLink immediately" in {
      Neo4jDB.use(DefaultNeo4jIdentifier)( db =>
        execInNeo4j(d => {
          val session = new LiftSession("", Helpers.nextFuncName, Empty)
          //init S for countries (S.?)
          S.initIfUninitted(session) {
            link_test_single.save
            
            link_test_single.links --> (usa, Map("country" -> "usa", "index" -> "index1"), true )
            (link_test_single.links --> (canada, Map("country" -> "usa", "index" -> "index1"), true )) must throwA(new Exception("Already has relationship"))
            
            link_test_single.links.get_saved must notBeEmpty
            link_test_single.links.get_saved.get must_== usa
          }
        })(db)
      )
    }
    
    "correctly delete SingleLinke unimmediately" in {
      Neo4jDB.use(DefaultNeo4jIdentifier)( db =>
        execInNeo4j(d => {
          link_test_single.links --> (usa, Map("country" -> "usa", "index" -> "index1") )
          link_test_single.links.deleteLink()
          
          link_test_single.save
          
          link_test_single.links.get_saved must beEmpty
        })(db)
      )
    }
    
    "correctly delete SingleLinke immediately" in {
      Neo4jDB.use(DefaultNeo4jIdentifier)( db =>
        execInNeo4j(d => {
          link_test_single.save
          
          link_test_single.links --> (usa, Map("country" -> "usa", "index" -> "index1") )
          link_test_single.links.deleteLink(immediately = true)
          
          link_test_single.links.get_saved must beEmpty
        })(db)
      )
    }
    
    "correctly guard SingleLink" in {
      Neo4jDB.use(DefaultNeo4jIdentifier)( db =>
        execInNeo4j(d => {
          val session = new LiftSession("", Helpers.nextFuncName, Empty)
          //init S for countries (S.?)
          S.initIfUninitted(session) {
            link_test_single_guarded.save
  
            link_test_single_guarded.links --> (usa, Map("country" -> "usa", "index" -> "index1") )
            (link_test_single_guarded.links --> (australia, Map("country" -> "australia", "index" -> "index2") )) must throwA(new GuardedException("guarded"))
            
            link_test_single_guarded.links.get_saved must beEmpty
            link_test_single_guarded.links.get1.get must_== usa
            link_test_single_guarded.links.get2(List("country", "index")).get must_== (usa, List("country" -> "usa", "index" -> "index1"))
            
            (link_test_single_guarded.links --> (canada, Map("country" -> "canada", "index" -> "index3"), true )) must throwA(new GuardedException("guarded"))
            link_test_single_guarded.links --> (sweden, Map("country" -> "sweden", "index" -> "index4"), true )
            
            link_test_single_guarded.links.get_saved.get must_== sweden
            link_test_single_guarded.links.get2(List("country", "index")).get must_== (sweden, List("country" -> "sweden", "index" -> "index4"))
          }
        })(db)
      )
    }
    
    "correctly get links by properties" in {
      Neo4jDB.use(DefaultNeo4jIdentifier)( db =>
        execInNeo4j(d => {
          link_test.save()

          link_test.links += (usa, Map("country" -> "usa", "index" -> "index1"), true )
          link_test.links += (australia, Map("country" -> "australia", "index" -> "index2"), true)
          link_test.links += (canada, Map("country" -> "canada", "index" -> "index3"), true)
          link_test.links += (sweden, Map("country" -> "sweden", "index" -> "index4"), true)
          link_test.links += (usa, Map("country" -> "usa", "index" -> "index5"), true)

          val ls1 = link_test.links.rels(Map("country" -> "usa"))
          val list1 = ls1.map(_("country") getOrElse "").toList
          val list2 = ls1.map(_("index") getOrElse "").toList
          
          list1 must_== List("usa", "usa")
          list2 must_== List("index1", "index5")
          
          link_test.links.rels(Map("country" -> "usa", "index" -> "index2")).toList.size mustBe 0
          link_test.links.rels(Map("country" -> "usa", "index" -> "index1")).toList.size mustBe 1
        })(db)
      )      
    }
    
    "correctly get indexed links by properties" in {
      Neo4jDB.use(DefaultNeo4jIdentifier)( db =>
        execInNeo4j(d => {
          link_ind_test.save()

          link_ind_test.links_ind += (usa, Map("country" -> "usa", "index" -> "index1"), true )
          link_ind_test.links_ind += (australia, Map("country" -> "australia", "index" -> "index2"), true)
          link_ind_test.links_ind += (canada, Map("country" -> "canada", "index" -> "index3"), true)
          link_ind_test.links_ind += (sweden, Map("country" -> "sweden", "index" -> "index4"), true)

          val ls1 = link_ind_test.links_ind.rels(Map("country" -> "usa"))
          val list = ls1.toList
          val list1 = list.map(_("country") getOrElse "")
          val list2 = list.map(_("index") getOrElse "")
          
          list1 must_== List("usa")
          list2 must_== List("index1")
          
          link_ind_test.links_ind.rels(Map("country" -> "usa", "index" -> "index2")).toList.size mustBe 0
          link_ind_test.links_ind.rels(Map("country" -> "usa", "index" -> "index1")).toList.size mustBe 1
        })(db)
      )
    }
  }
}