package queryObject

import dbObject.{DbTable, Table}

case class From(maybeTable: Option[Table]) {
  val table = maybeTable match {
    case Some(table) => table
    case None => "" // ultimately would want this to be an empty table once I can implement that method
  }
  override def toString: String = s"FROM ${table.toString}"
  def empty = this.copy(maybeTable = None)
}

object From {
  implicit def something(companion: From.type): From = companion.apply // can I get this to work???
  def apply(table: DbTable): From = From(table)
  def apply: From = From(None)
//  def apply()
}
