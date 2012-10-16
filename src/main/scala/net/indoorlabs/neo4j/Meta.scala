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

import record.field.Neo4jBinaryField

private[neo4j] object Meta {
  /*
  * For converting scala objects into DBObject values
  */
  object Reflection {
    import java.lang.reflect._
    import java.util.{Calendar, Date, GregorianCalendar}

    /*
    * These don't require a conversion and can be put directly into a DBObject
    */
    val primitives = Set[Class[_]](classOf[String], classOf[Int], classOf[Long], classOf[Double],
                                  classOf[Float], classOf[Byte], classOf[Boolean],
                                  classOf[Short], classOf[java.lang.Integer], classOf[java.lang.Long],
                                  classOf[java.lang.Double], classOf[java.lang.Float],
                                  classOf[java.lang.Byte], classOf[java.lang.Boolean],
                                  classOf[java.lang.Short])

    def primitive_?(clazz: Class[_]) = primitives contains clazz

    def primitive2prop_value(a: Any) = a match {
      case x: java.lang.Integer => x.intValue
      case x: java.lang.Long => x.longValue
      case x: java.lang.Double => x.doubleValue
      case x: java.lang.Float => x.floatValue
      case x: java.lang.Byte => x.byteValue
      case x: java.lang.Boolean => x.booleanValue
      case x: java.lang.Short => x.shortValue
      case x => x
    }

    /*
    * Date types require formatting
    */
    val datetypes = Set[Class[_]](classOf[Calendar], classOf[Date], classOf[GregorianCalendar])

    def datetype_?(clazz: Class[_]) = datetypes contains clazz

    def datetype2prop_value(a: Any) = a match {
      case x: Calendar => x.getTime.getTime
      case x: Date => x.getTime
    }

    import org.neo4j.graphdb.Node
    import net.liftweb.record.Field
    import net.liftweb.record.field.{EnumNameTypedField, EnumTypedField}
    import org.neo4j.scala.Neo4jImplicits._
    import scala.Array
    def setNodeProperty(f: Field[_, _], node: Node) = f match {
      case field if (field.optional_? && field.valueBox.isEmpty) => // don't add to DBObject
      case field: Neo4jBinaryField[_] => field.save
      case field: EnumTypedField[Enumeration] =>
        field.asInstanceOf[EnumTypedField[Enumeration]].valueBox foreach {
          v => node(f.name) = v.id
        }
      case field: EnumNameTypedField[Enumeration] =>
        field.asInstanceOf[EnumNameTypedField[Enumeration]].valueBox foreach {
          v => node(f.name) = v.toString
        }
      case field => field.valueBox foreach (f_val => {
        val n4jtype = any2neo4j(f_val.asInstanceOf[AnyRef])
        if (n4jtype == null) node.removeProperty(f.name)
        else node(f.name) = n4jtype
      })
    }

    def any2neo4j(any: AnyRef) = {
      any match {
        case null => null

        case x if primitive_?(x.getClass) => primitive2prop_value(x)

        case x if datetype_?(x.getClass) => datetype2prop_value(x)

        case x : Array[String] => x
        case x : Array[Int] => x
        case x : Array[Long] => x
        case x : Array[Double] => x
        case x : Array[Float] => x
        case x : Array[Byte] => x
        case x : Array[Boolean] => x
        case x : Array[Short] => x

        case o => o.toString
      }
    }
  }
}