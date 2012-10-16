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

import org.neo4j.graphdb._
import index.Index
import org.neo4j.scala.Neo4jWrapper
import scala.collection.JavaConversions._

trait Neo4jMeta[BaseNode] extends Neo4jWrapper {

  // override this to specify a Neo4JIdentifier for this Node type
  def neo4jIdentifier: Neo4jIdentifier = DefaultNeo4jIdentifier

  /*
  * Delete exact node
  */
  def delete(node: Node, node_indecies: Seq[Index[Node]] = Nil) = {
    val rels = node.rels
    node_indecies.foreach(_.remove(node))
    node.delete
    rels
  }

  /*
  * Delete node with relations
   */
  def delete_!(node: Node, node_indecies: Seq[Index[Node]] = Nil, exclude_nodes: List[Node]) = {
    val ref_node = usedb(db => db.getReferenceNode)
    delete_rel(node).toList.distinct.filter(l_n => {
      //try { //neo4j bug
        !exclude_nodes.contains(l_n) &&
        l_n != ref_node &&
        !l_n.rels.iterator.hasNext
      //} catch {
      //  case _ => false
      //}
    }).foreach(delete(_, node_indecies))
    node_indecies.foreach(_.remove(node))
    node.delete
  }

  /*
  * Delete node relations
   */
  def delete_rel(node: Node) = {
    node.rels.map(r => {
      val n = r.getOtherNode(node)
      r.delete
      n
    })
  }

  def usedb[T](f: (GraphDatabaseService) => T): T = Neo4jDB.use(neo4jIdentifier)(f)


  /*
  /*
  * Ensure an index exists
  */
  def ensureIndex(keys: JObject) {
    MongoDB.useCollection(mongoIdentifier, collectionName) ( coll => {
      coll.ensureIndex(JObjectParser.parse(keys))
    })
  }

  /*
  * Ensure an index exists and make unique
  */
  def ensureIndex(keys: JObject, unique: Boolean) {
    val options = new BasicDBObject
    if (unique) options.put("unique", true)
    MongoDB.useCollection(mongoIdentifier, collectionName) ( coll => {
      coll.ensureIndex(JObjectParser.parse(keys), options)
    })
  }

  /*
  * Ensure an index exists with options
  */
  def ensureIndex(keys: JObject, opts: JObject) {
    MongoDB.useCollection(mongoIdentifier, collectionName) ( coll => {
      coll.ensureIndex(JObjectParser.parse(keys), JObjectParser.parse(opts))
    })
  }
  */
}

/*
* For passing in options to the find function
*/
abstract sealed class FindOption {
  def value: Int
}
case class Limit(value: Int) extends FindOption
case class Skip(value: Int) extends FindOption

/*
* For passing in options to the update function
*/
abstract sealed class UpdateOption
case object Upsert extends UpdateOption
case object Multi extends UpdateOption