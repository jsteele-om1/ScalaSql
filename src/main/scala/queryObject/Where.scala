package queryObject

import dbObject.Field

case class Where(condition: String) {
  override def toString: String = ""
  //  def empty = ""
}

trait Expression

trait ConditionOperator

object Expressions {

  def empty = ""

  def greaterThan(left: String, right: String) = s"$left > $right"
}

