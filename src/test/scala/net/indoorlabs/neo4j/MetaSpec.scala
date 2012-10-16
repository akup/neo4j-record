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
import org.specs.matcher.Matcher

import org.specs.runner.JUnit4

import Meta.Reflection._

import org.neo4j.graphdb._
import org.neo4j.scala.Neo4jImplicits._

class MetaSpecTest extends JUnit4(MetaSpec)

object MetaSpec extends Specification("Meta Specification") with ScalaCheck {
  doAfterSpec {
    Neo4jDB.close
  }

  case class beEqualTyped[T, Q](a: Q, eq: Boolean) extends Matcher[T]() {
    def apply(v: => T) = ({
      v match {
        case test: T if ((a.asInstanceOf[AnyRef].getClass == test.asInstanceOf[AnyRef].getClass) && test == a) => eq
        case test: T if (a.asInstanceOf[AnyRef].getClass == test.asInstanceOf[AnyRef].getClass) => test match {
          case t: Array[_] => {
            val t_seq: Seq[_] = t
            val a_seq: Seq[_] = a.asInstanceOf[Array[_]]
            if (t_seq == a_seq) eq else !eq
          }
          case t if (t == a) => eq
          case _ => !eq
        }
        case _ => !eq
      }
      }, "okMessage", "koMessage")
  }

  "Meta" should {
    "convert string to string" in {
      val test = "456"
      val conv = any2neo4j(test.asInstanceOf[AnyRef])
      conv must beEqualTyped("456": String, true)
      conv must beEqualTyped("456_diff": String, false)
      conv must beEqualTyped(456: Int, false)
    }

    "convert int to int" in {
      val test: Int = 456
      val conv = any2neo4j(test.asInstanceOf[AnyRef])
      conv must beEqualTyped(456: Int, true)
      conv must beEqualTyped(457: Int, false)
      conv must beEqualTyped(456L: Long, false)
    }

    "convert long to long" in {
      val test: Long = 456L
      val conv = any2neo4j(test.asInstanceOf[AnyRef])
      conv must beEqualTyped(456L: Long, true)
      conv must beEqualTyped(457L: Long, false)
      conv must beEqualTyped(456: Int, false)
    }

    "convert double to double" in {
      val test: Double = 456.987
      val conv = any2neo4j(test.asInstanceOf[AnyRef])
      conv must beEqualTyped(456.987: Double, true)
      conv must beEqualTyped(456.986: Double, false)
      conv must beEqualTyped(456.987f: Float, false)
    }

    "convert float to float" in {
      val test: Float = 456.987f
      val conv = any2neo4j(test.asInstanceOf[AnyRef])
      conv must beEqualTyped(456.987f: Float, true)
      conv must beEqualTyped(456.986f: Float, false)
      conv must beEqualTyped(456.987: Double, false)
    }

    "convert byte to byte" in {
      val test: Byte = 34
      val conv = any2neo4j(test.asInstanceOf[AnyRef])
      conv must beEqualTyped(34: Byte, true)
      conv must beEqualTyped(35: Byte, false)
      conv must beEqualTyped(34: Int, false)
    }

    "convert boolean to boolean" in {
      val test: Boolean = true
      val conv = any2neo4j(test.asInstanceOf[AnyRef])
      conv must beEqualTyped(true: Boolean, true)
      conv must beEqualTyped(false: Boolean, false)
      conv must beEqualTyped(1: Int, false)
    }

    "convert short to short" in {
      val test: Short = 10
      val conv = any2neo4j(test.asInstanceOf[AnyRef])
      conv must beEqualTyped(10: Short, true)
      conv must beEqualTyped(11: Short, false)
      conv must beEqualTyped(10: Int, false)
    }

    "convert string array to string array" in {
      val test: Array[String] = Array[String]("AAA", "BBBB", "CCCCC")
      val conv = any2neo4j(test.asInstanceOf[AnyRef])
      conv must beEqualTyped(Array[String]("AAA", "BBBB", "CCCCC"), true)
      conv must beEqualTyped(Array[String]("AAA", "BBBB", "CCCCC", "DDDD"), false)
      conv must beEqualTyped(Array[String]("AAA", "BBBB"), false)
      conv must beEqualTyped(Array[Int](1, 2, 3), false)
    }

    "convert int array to int array" in {
      val test: Array[Int] = Array[Int](1, 2, 3)
      val conv = any2neo4j(test.asInstanceOf[AnyRef])
      conv must beEqualTyped(Array[Int](1, 2, 3), true)
      conv must beEqualTyped(Array[Int](1, 2, 3, 4), false)
      conv must beEqualTyped(Array[Int](1, 2), false)
      conv must beEqualTyped(Array[Long](1L, 2L, 3L), false)
    }

    "convert long array to long array" in {
      val test: Array[Long] = Array[Long](1L, 2L, 3L)
      val conv = any2neo4j(test.asInstanceOf[AnyRef])
      conv must beEqualTyped(Array[Long](1L, 2L, 3L), true)
      conv must beEqualTyped(Array[Long](1L, 2L, 3L, 4L), false)
      conv must beEqualTyped(Array[Long](1L, 2L), false)
      conv must beEqualTyped(Array[Int](1, 2, 3), false)
    }

    "convert double array to double array" in {
      val test: Array[Double] = Array[Double](1.0, 1.2, 3.45)
      val conv = any2neo4j(test.asInstanceOf[AnyRef])
      conv must beEqualTyped(Array[Double](1.0, 1.2, 3.45), true)
      conv must beEqualTyped(Array[Double](1.0, 1.2, 3.45, 2.111), false)
      conv must beEqualTyped(Array[Double](1.0, 1.2, 3.46), false)
      conv must beEqualTyped(Array[Float](1.0f, 1.2f, 3.45f), false)
    }

    "convert float array to float array" in {
      val test: Array[Float] = Array[Float](1.0f, 1.2f, 3.45f)
      val conv = any2neo4j(test.asInstanceOf[AnyRef])
      conv must beEqualTyped(Array[Float](1.0f, 1.2f, 3.45f), true)
      conv must beEqualTyped(Array[Float](1.0f, 1.2f, 3.45f, 2.111f), false)
      conv must beEqualTyped(Array[Float](1.0f, 1.2f, 3.46f), false)
      conv must beEqualTyped(Array[Double](1.0, 1.2, 3.45), false)
    }

    "convert byte array to byte array" in {
      val test: Array[Byte] = Array[Byte](1, 2, 3)
      val conv = any2neo4j(test.asInstanceOf[AnyRef])
      conv must beEqualTyped(Array[Byte](1, 2, 3), true)
      conv must beEqualTyped(Array[Byte](1, 2, 3, 2), false)
      conv must beEqualTyped(Array[Byte](1, 2, 4), false)
      conv must beEqualTyped(Array[Int](1, 2, 3), false)
    }

    "convert boolean array to boolean array" in {
      val test: Array[Boolean] = Array[Boolean](true, false, true)
      val conv = any2neo4j(test.asInstanceOf[AnyRef])
      conv must beEqualTyped(Array[Boolean](true, false, true), true)
      conv must beEqualTyped(Array[Boolean](true, false, true, true), false)
      conv must beEqualTyped(Array[Boolean](true, false, false), false)
      conv must beEqualTyped(Array[Int](1, 0, 1), false)
    }

    "convert short array to short array" in {
      val test: Array[Short] = Array[Short](1, 2, 3)
      val conv = any2neo4j(test.asInstanceOf[AnyRef])
      conv must beEqualTyped(Array[Short](1, 2, 3), true)
      conv must beEqualTyped(Array[Short](1, 2, 3, 4), false)
      conv must beEqualTyped(Array[Short](1, 2, 4), false)
      conv must beEqualTyped(Array[Int](1, 2, 3), false)
    }
  }
}