package net.indoorlabs.neo4j
package record
package field

import org.specs._
import org.specs.runner._
import org.specs.matcher.Matcher
import org.specs.runner.JUnit4
import org.neo4j.graphdb._
import net.liftweb.common.{Full, Empty}
import java.io.ByteArrayInputStream
import org.neo4j.scala.Neo4jImplicits._

class Neo4jBinarySpecTest extends JUnit4(Neo4jBinarySpec)

object Neo4jBinarySpec extends Specification("Neo4j Specification") with ScalaCheck with Neo4jTestKit {

  "Neo4jBinaryField" should {
    checkNeo4jIsRunning
    checkMongoIsRunning

    val bin_test = BinFieldTestRecord.createRecord

    "correctly set binary field from byte array" in {
      Neo4jDB.use(DefaultNeo4jIdentifier)( db =>
        execInNeo4j(d => {
		      val b_arr_with_name = Array(0, 1, 2, 3, 4, 5, 6, 7, 8, 9).map(_.toByte) -> "test array"
		      bin_test.mandatoryBinField.setFromAny(b_arr_with_name)
		
		      val stream = bin_test.mandatoryBinField.getInputStreamBox
		      val f_name = bin_test.mandatoryBinField.file_name
		
		      Stream.continually(stream.get.read).takeWhile(-1 !=).map(_.toByte).toList mustEqual b_arr_with_name._1.toList
		      f_name mustEqual Full(b_arr_with_name._2)
        })(db)
      )
    }

    "correctly set binary field from input stream" in {
      Neo4jDB.use(DefaultNeo4jIdentifier)( db =>
        execInNeo4j(d => {
		      val is_with_name = new ByteArrayInputStream(Array(0, 1, 2, 3, 4, 5, 6, 7, 8, 9).map(_.toByte)) -> "test stream"
		      bin_test.mandatoryBinField.setFromAny(is_with_name)
		
		      val stream = bin_test.mandatoryBinField.getInputStreamBox
		      val f_name = bin_test.mandatoryBinField.file_name
		
		      val str_list = Stream.continually(stream.get.read).takeWhile(-1 !=).map(_.toByte).toList
		      is_with_name._1.reset
		      val init_list = Stream.continually(is_with_name._1.read).takeWhile(-1 !=).map(_.toByte).toList
		      str_list mustEqual init_list
		      f_name mustEqual Full(is_with_name._2)
        })(db)
      )
    }

    "correctly save and get binary data" in {
      val is_with_name = new ByteArrayInputStream(Array(0, 1, 2, 3, 4, 5, 6, 7, 8, 9).map(_.toByte)) -> "test stream"
      bin_test.mandatoryBinField.setFromAny(is_with_name)

      Neo4jDB.use(DefaultNeo4jIdentifier)( db =>
        execInNeo4j(d => {
          bin_test.save()

          val binTestFromDb = BinFieldTestRecord.find(bin_test.id)
          binTestFromDb must notBeEmpty
          binTestFromDb foreach { bt =>
            bt mustEqual bin_test

            val stream1 = bt.mandatoryBinField.getInputStreamBox
            val stream2 = bin_test.mandatoryBinField.getInputStreamBox
            val list1 = Stream.continually(stream1.get.read).takeWhile(-1 !=).map(_.toByte).toList
            val list2 = Stream.continually(stream2.get.read).takeWhile(-1 !=).map(_.toByte).toList
            list1 mustEqual list2
          }
        })(db)
      )
    }
  }
}