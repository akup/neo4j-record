package net.indoorlabs.neo4j

import org.neo4j.kernel.EmbeddedGraphDatabase
import java.util.concurrent.ConcurrentHashMap
import org.neo4j.graphdb.GraphDatabaseService
import org.neo4j.graphdb.factory.GraphDatabaseFactory

/*
* A trait for identfying Neo4J instances
*/
trait Neo4jIdentifier {
  def jndiName: String
  override def toString() = "Neo4jIdentifier("+jndiName+")"
  override def hashCode() = jndiName.hashCode()
  override def equals(other: Any): Boolean = other match {
    case mi: Neo4jIdentifier => mi.jndiName == this.jndiName
    case _ => false
  }
}

/*
* The default Neo4JIdentifier
*/
case object DefaultNeo4jIdentifier extends Neo4jIdentifier {
  val jndiName = "test"
}

/*
* Wrapper for getting a reference to a db from the given Neo4J instance
*/
case class Neo4jAddress(host: Neo4jHostBase) {
  def db = host.neo4j
}

/*
* Wrapper for creating a GraphDB instance
*/
abstract class Neo4jHostBase {
  def neo4j: GraphDatabaseService
}

case class Neo4jHost(storeDir: String = "tmp/default-graphDb", options: Map[String, String] = Map[String, String]()) extends Neo4jHostBase {
  import scala.collection.JavaConversions._
  override lazy val neo4j = {
    val opts = options + ("allow_store_upgrade" -> "true")
    new GraphDatabaseFactory()
            .newEmbeddedDatabaseBuilder( storeDir )
            .setConfig( opts )
            .newGraphDatabase()
    //new EmbeddedGraphDatabase(storeDir, opts)
    
    //if (options.size > 0) new EmbeddedGraphDatabase(storeDir, options)
    //                        else new EmbeddedGraphDatabase(storeDir)
  }
}
object Neo4jHost {
  def apply(stDir: String): Neo4jHost =  Neo4jHost(storeDir=stDir)
  def apply(opt: Map[String, String]): Neo4jHost = Neo4jHost(options=opt)
}

/*
/*
* Wrapper for creating a Replica Pair
*/
case class MongoPair(left: ServerAddress, right: ServerAddress, options: MongoOptions = new MongoOptions) extends MongoHostBase {
  lazy val mongo = new Mongo(left, right, options)
}

/*
 * Wrapper for creating a Replica Set
 */
case class MongoSet(dbs: List[ServerAddress], options: MongoOptions = new MongoOptions) extends MongoHostBase {
  import scala.collection.JavaConversions._
  lazy val mongo = new Mongo(dbs, options)
}
*/

/*
* Main Neo4j object
*/
object Neo4jDB {

  /*
  * HashMap of Neo4jAddresses, keyed by Neo4jIdentifier
  */
  private val dbs = new ConcurrentHashMap[Neo4jIdentifier, Neo4jAddress]

  /*
  * Define a Neo4j
  */
  def defineDb(name: Neo4jIdentifier, address: Neo4jAddress) {
    dbs.put(name, address)
  }
  /*
   * Define a Mongo db using a standard Mongo instance.
   */
  def defineDb(name: Neo4jIdentifier, n4j: GraphDatabaseService) {
    dbs.put(name, Neo4jAddress(new Neo4jHostBase { override def neo4j = n4j }))
  }

  /*
  * Get a DB reference
  */
  def getDb(name: Neo4jIdentifier): Option[GraphDatabaseService] = dbs.get(name) match {
    case null => None
    case na: Neo4jAddress => Some(na.db)
  }

  def use[T](name: Neo4jIdentifier)(f: (GraphDatabaseService) => T): T = {

    val db = getDb(name) match {
      case Some(neo4j) => neo4j
      case _ => throw new Exception("Neo4j not found: "+name.toString)
    }

    f(db)
  }

  def use[T](f: (GraphDatabaseService) => T): T = {

    val db = getDb(DefaultNeo4jIdentifier) match {
      case Some(neo4j) => neo4j
      case _ => throw new Exception("Neo4j not found: "+DefaultNeo4jIdentifier.toString)
    }

    f(db)
  }

  //
  def close {
    import scala.collection.JavaConversions._
    dbs.foreach(_._2.db.shutdown())
    dbs.clear
  }
}