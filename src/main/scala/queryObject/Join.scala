package queryObject

import dbObject.{Column, Table}

trait Join extends QueryObject {
  def syntax: String
  def rightTable: Table
  def leftColumn: Column
  def rightColumn: Column

  override def toString: String = {
    s"$syntax JOIN $rightTable on $leftColumn = $rightColumn"
  }
}

case class InnerJoin(leftColumn: Column, rightTable: Table, rightColumn: Column) extends Join {
  override def syntax: String = "INNER"
}

// I'm totally doing this wrong... should I be using case classes here?