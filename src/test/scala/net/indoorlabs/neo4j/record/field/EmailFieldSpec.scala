package net.indoorlabs.neo4j
package record
package field

import org.specs._
import org.specs.runner._
import org.specs.matcher.Matcher
import org.specs.runner.JUnit4

class EmailFieldSpecTest extends JUnit4(EmailFieldSpec)

object EmailFieldSpec extends Specification("EmailField Specification") with ScalaCheck {

  "EmailField" should {
    
    "correctly validate emails" in {
      EmailField.validEmailAddr_?("ak@neonavigation.com") mustBe true
      EmailField.validEmailAddr_?("AK@NEOnavigation.com") mustBe true
      EmailField.validEmailAddr_?("noreply@adm.neonavigation.com") mustBe true
      EmailField.validEmailAddr_?("kuprin.alexander@gmail.com") mustBe true
      EmailField.validEmailAddr_?("kuprin.alexander@gmail.gmail.com") mustBe true
      EmailField.validEmailAddr_?("kuprin.alexander@gma-il.gmail.com") mustBe true
      EmailField.validEmailAddr_?("ku-prin.alexander@gma-il.gmail.com") mustBe true
      EmailField.validEmailAddr_?("ku_prin.alexander@gma-il.gmail.com") mustBe true
      EmailField.validEmailAddr_?("ku_prin.alexander@gma_il.gmail.com") mustBe false
      EmailField.validEmailAddr_?("kuprin.alexander@gmail.gmail.com.") mustBe false
      EmailField.validEmailAddr_?("kuprin.alexander@.gmail.gmail.com") mustBe false
      EmailField.validEmailAddr_?("kuprin.alexander@gmail+gmail.com") mustBe false
      EmailField.validEmailAddr_?("kuprin+alexander@gmail.com") mustBe false
      EmailField.validEmailAddr_?(".kuprin.alexander@gmail.com") mustBe false
      EmailField.validEmailAddr_?("kuprin..alexander@gmail.com") mustBe false
      EmailField.validEmailAddr_?("kuprin.alexander@gmail..com") mustBe false
      EmailField.validEmailAddr_?("kuprin.alexander.@gmail.com") mustBe false
      EmailField.validEmailAddr_?("kuprin.alexandergmail.com") mustBe false
    }
  }
}