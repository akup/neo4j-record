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