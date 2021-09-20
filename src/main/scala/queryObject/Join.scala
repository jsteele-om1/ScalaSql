package queryObject

import dbObject.{Column, DbTable}

trait Join extends QueryObject { // should this be a class or a trait?? or should inner outer etc... be functions??
  def syntax: String

  def rightTable: DbTable

  def leftColumn: Column

  def rightColumn: Column

  override def toString: String = {
    s"$syntax JOIN $rightTable on $leftColumn = $rightColumn"
  }
}

case class InnerJoin(leftColumn: Column, rightTable: DbTable, rightColumn: Column) extends Join {
  override def syntax: String = "INNER"
}

// I'm totally doing this wrong... should I be using case classes here?