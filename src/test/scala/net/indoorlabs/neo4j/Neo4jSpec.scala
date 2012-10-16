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

import org.specs._
import org.specs.runner._

import org.neo4j.scala._

import org.neo4j.graphdb._
import org.neo4j.kernel.{EmbeddedGraphDatabase, AbstractGraphDatabase}
import scala.collection.JavaConversions._

class Neo4jSpecTest extends JUnit4(Neo4jSpec)

object Neo4jSpec extends Specification("Neo4j Specification") with Neo4jWrapper {

  case object TestNeo4jIdentifier extends Neo4jIdentifier {
    override val jndiName = "test_a"
  }

  /*
  Runtime.getRuntime.addShutdownHook(new Thread() {
    override def run() {
      Neo4jDB.close
    }
  })
  */

  doAfterSpec {
    Neo4jDB.close
  }

  def passDefinitionTests(id: Neo4jIdentifier, na: Neo4jAddress): Unit = {
    // define the db
    Neo4jDB.close
    Neo4jDB.defineDb(id, na)

    // make sure neo4j is running
    try {
      Neo4jDB.use(id)( db =>
        execInNeo4j(d => d.createNode match {
            case n: Node => Some(n)
            case _ => None
          })(db)
      ) must beSome
    }
    catch {
      case e: Exception => skip("Neo4j is not running")
    }

    // using an undefined identifier throws an exception
    Neo4jDB.use(DefaultNeo4jIdentifier) { db =>
      execInNeo4j(d => d.createNode match {
            case n: Node => Some(n)
            case _ => None
          })(db)
    } must throwA(new Exception("Neo4j not found: Neo4jIdentifier(test)"))

    // remove defined db
    Neo4jDB.close
  }

  "Neo4j" should {
    "Define DB with default location" in {
      passDefinitionTests(TestNeo4jIdentifier, Neo4jAddress(Neo4jHost()))
    }
    "Define DB with specified location" in {
      passDefinitionTests(TestNeo4jIdentifier, Neo4jAddress(Neo4jHost("tmp/test-gdb")))
    }
    "Define DB with specified location and options" in {
      passDefinitionTests(TestNeo4jIdentifier, Neo4jAddress(Neo4jHost("tmp/gdb", Map[String, String]())))
    }
    "Define DB with specified options" in {
      passDefinitionTests(TestNeo4jIdentifier, Neo4jAddress(Neo4jHost(Map[String, String]())))
    }
    "Define DB with directly specified options" in {
      passDefinitionTests(TestNeo4jIdentifier, Neo4jAddress(Neo4jHost(options=Map[String, String]())))
    }


    "Define DB with GraphDatabase instance" in {
      // define the db
      import org.neo4j.graphdb.factory.GraphDatabaseFactory
      Neo4jDB.close
      Neo4jDB.defineDb(TestNeo4jIdentifier, new GraphDatabaseFactory()
            .newEmbeddedDatabaseBuilder( "tmp/test-grafDB" )
            .setConfig( Map("allow_store_upgrade" -> "true") )
            .newGraphDatabase()/*new EmbeddedGraphDatabase("tmp/test-grafDB", Map("allow_store_upgrade" -> "true"))*/)

      // make sure neo4j is running
      try {
        Neo4jDB.use(TestNeo4jIdentifier)( db =>
          execInNeo4j(d => d.createNode match {
              case n: Node => Some(n)
              case _ => None
            })(db)
        ) must beSome
      }
      catch {
        case e: Exception => skip("Neo4j is not running")
      }

      // using an undefined identifier throws an exception
      Neo4jDB.use(DefaultNeo4jIdentifier) { db =>
        execInNeo4j(d => d.createNode match {
            case n: Node => Some(n)
            case _ => None
          })(db)
      } must throwA(new Exception("Neo4j not found: Neo4jIdentifier(test)"))
      // remove defined db
      Neo4jDB.close
    }
  }
}