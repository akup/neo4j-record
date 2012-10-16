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

import org.specs.Specification
import org.neo4j.scala.Neo4jImplicits._
import org.neo4j.graphdb.Node
import net.liftweb.mongodb._

trait Neo4jTestKit {
  this: Specification =>

  val defaultHost = Neo4jHost()

  // If you need more than one db, override this
  def dbs: List[(Neo4jIdentifier, Neo4jHost)] =
    List((DefaultNeo4jIdentifier, defaultHost))

  def dbName = "lift_record_"+this.getClass.getName
    .replace("$", "")
    .replace("net.nn.neo4j.record.", "")
    .replace(".", "_")
    .toLowerCase

  val mongoDefaultHost = MongoHost("127.0.0.1", 27017)

  // If you need more than one db, override this
  def mongo_dbs: List[(MongoIdentifier, MongoHost, String)] =
    List((DefaultMongoIdentifier, mongoDefaultHost, dbName))

  def isMongoRunning: Boolean =
    try {
      if (mongo_dbs.length < 1)
        false
      else {
        mongo_dbs foreach { dbtuple =>
          MongoDB.use(dbtuple._1) ( db => { db.getLastError } )
        }
        true
      }
    } catch {
      case _ => false
    }

  def checkMongoIsRunning =
    isMongoRunning must beEqualTo(true).orSkipExample

  def debug = false

  doBeforeSpec {
    // define the dbs
    dbs foreach { dbtuple =>
      Neo4jDB.defineDb(dbtuple._1, Neo4jAddress(dbtuple._2))
    }

    // define the dbs
    mongo_dbs foreach { dbtuple =>
      MongoDB.defineDb(dbtuple._1, MongoAddress(dbtuple._2, dbtuple._3))
    }
  }

  def isNeo4jRunning: Boolean =
    try {
      if (dbs.length < 1)
        false
      else {
        dbs foreach { dbtuple =>
          Neo4jDB.use(dbtuple._1)( db =>
            execInNeo4j(d => d.createNode match {
                case n: Node => Some(n)
                case _ => None
              })(db)
          ) must beSome
        }
        true
      }
    }
    catch {
      case e: Exception => skip("Neo4j is not running")
    }

  def checkNeo4jIsRunning =
    isNeo4jRunning must beEqualTo(true).orSkipExample

  doAfterSpec {
    Neo4jDB.close

    if (!debug && isMongoRunning) {
      // drop the databases
      mongo_dbs foreach { dbtuple =>
        MongoDB.use(dbtuple._1) { db => db.dropDatabase }
      }
    }

    // clear the mongo instances
    MongoDB.close
  }
}