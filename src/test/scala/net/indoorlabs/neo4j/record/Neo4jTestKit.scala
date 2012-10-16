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