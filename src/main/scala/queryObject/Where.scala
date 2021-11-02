package queryObject

import dbObject.Field

case class Where(conditions: Seq[Expression]) { // should probably just write it yourself
  private val conditionsString = conditions.mkString(("\nAND\n"))

  private val whereClause = conditions match {
    case Seq() => ""
    case _ => s"WHERE \n\t$conditionsString"
  }
  override def toString: String = whereClause
  //  def empty = ""
}

case class Expression(expression: String) {
  override def toString: String = expression
}

trait ConditionOperator

object Expressions {

  def empty: Expression = Expression("")

  def greaterThan(left: String, right: String): Expression = Expression(s"$left > $right")
  def lessThan(left: String, right: String): Expression = Expression(s"$left < $right")
  def equals(left: String, right: String): Expression = Expression(s"$left == $right")
  def isIn(column: Field, fieldType: String, inCollection: Seq[String]): Expression = {
    val expression = if (fieldType == "VARCHAR") s"(\n\t\t${inCollection.map(i => s"'$i'").mkString(",\n\t\t")}\n\t)"
            else s"(${inCollection.mkString(",\n")})"
    Expression(s"$column in $expression")
  }
}

