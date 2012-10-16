package net.indoorlabs.neo4j
package record
package field

import net.liftweb.record.{OptionalTypedField, MandatoryTypedField}

import scala.xml._

import org.mindrot.jbcrypt.BCrypt

import net.liftweb._
import common._
import http.S
import http.js._
import json.JsonAST.{JNothing, JNull, JString, JValue}
import net.liftweb.record._
import util._
import Helpers._
import S._
import JE._

object PasswordField {
  @volatile var blankPw = "*******"
  @volatile var minPasswordLength = 6
  @volatile var logRounds = 10

  private[field] def hashpw(in: String): Box[String] =
    tryo(BCrypt.hashpw(in, BCrypt.gensalt(logRounds)))
}

trait PasswordTypedField extends TypedField[String] {
  /*
  * Call this after validation and before it is saved to the db to hash
  * the password. Eg. in the finish method of a screen.
  */
  def hashIt {
    valueBox foreach { v =>
      setBox(PasswordField.hashpw(v))
    }
  }
  
  private var invalidPw = false  
  private var invalidMsg = ""
  private var resetPw = false

  def setFromAny(in: Any): Box[String] = {
    in match {
      case (a: Array[String]) if (a.length == 2 && a(0)   == a(1)) => setBox(Full(a(0)))
      case (h1: String) :: (h2: String) :: Nil if h1 == h2 => setBox(Full(h1))
      case _ => {
        invalidMsg = S.??("passwords.do.not.match");
        val ret = genericSetFromAny(in);
        invalidPw = true;
        ret
      }
    }
  }
  
  def resetPassword() = {
    resetPw = true
    setBox(Full(""))
  }

  def setFromString(s: String): Box[String] = s match {
    case "" if optional_? => setBox(Empty)
    case _                => setBox(Full(s))
  }

  override def validate: List[FieldError] = runValidation(validatedValue)

  override def notOptionalErrorMessage = S.??("password.must.be.set")

  private var validatedValue: Box[String] = valueBox

  override def set_!(in: Box[String]): Box[String] = {
    validatedValue = in
    invalidPw = false
    if (!resetPw) {
      in.map(s => PasswordField.hashpw(s).get)//hash("{"+s+"} salt={"+salt_i.get+"}"))
    } else {
      resetPw = false
      in.map(s => "")
    }
  }
  /*
  * jBCrypt throws "String index out of range" exception
  * if password is an empty String
  */
  def match_?(toTest: String): Boolean = valueBox
    .filter(_.length > 0)
    .flatMap(p => tryo(BCrypt.checkpw(toTest, p)))
    .openOr(false)

  private def elem = S.fmapFunc(SFuncHolder(this.setFromAny(_))){
    funcName => <input type="password"
      name={funcName}
      value={valueBox openOr ""}
      tabindex={tabIndex toString}/>}

  override def toForm: Box[NodeSeq] =
    uniqueFieldId match {
      case Full(id) => Full(elem % ("id" -> (id + "_field")))
      case _ => Full(elem)
    }

  protected def validatePassword(pwdValue: ValueType): List[FieldError] =
    toBoxMyType(pwdValue) match {
      case Empty|Full(""|null) if !optional_? => Text(notOptionalErrorMessage)
      case Full(s) => {
        if (s == "*" || s == PasswordField.blankPw || s.length < PasswordField.minPasswordLength) {
          resetPw = true
          setBox(Full(""))
          Text(S.??("password.too.short"))
        }
        else if (invalidPw) {
          resetPw = true
          setBox(Full(""))
          Text(invalidMsg)
        }
        else Nil
      }
      case _ => Nil
    }

  override def validations = validatePassword _ :: Nil

  def defaultValue = ""

  def asJs = valueBox.map(Str) openOr JsNull

  def asJValue: JValue = valueBox.map(v => JString(v)) openOr (JNothing: JValue)
  def setFromJValue(jvalue: JValue): Box[MyType] = jvalue match {
    case JNothing|JNull if optional_? => setBox(Empty)
    case JString(s)                   => setFromString(s)
    case other                        => setBox(FieldHelpers.expectedA("JString", other))
  }

  //override def canRead_? = false
}

class Neo4jPasswordField[OwnerType <: Neo4jRecord[OwnerType]](rec: OwnerType)
  extends Neo4jField[String, OwnerType] with MandatoryTypedField[String] with PasswordTypedField {

  def this(rec: OwnerType, value: String) = {
    this(rec)
    set(value)
  }

  def owner = rec

  protected def propToBoxMyType(p: Any): Box[String] = p match {
    case s: String => Full(s)
    case _ => Empty
  }
}

class Neo4jOptionalPasswordField[OwnerType <: Neo4jRecord[OwnerType]](rec: OwnerType)
  extends Neo4jField[String, OwnerType] with OptionalTypedField[String] with PasswordTypedField {

  def this(rec: OwnerType, value: Box[String]) = {
    this(rec)
    setBox(value)
  }

  def owner = rec

  protected def propToBoxMyType(p: Any): Box[String] = p match {
    case s: String => Full(s)
    case _ => Empty
  }
}