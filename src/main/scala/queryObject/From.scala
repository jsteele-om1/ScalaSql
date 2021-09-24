package queryObject

import dbObject.{DbTable, EmptyTable, Table}
import query.Query

import scala.collection.View.Empty

case class From(table: Table) {

  override def toString: String = table match {
    case t: DbTable => s"FROM ${t.toString}"
    case t: Query => s"FROM (${t.toString})"
    case _ => throw new IllegalArgumentException(s"$_ cannot be passed into a from clause")
  }
  def empty: From = this.copy(table = EmptyTable) // is this bad?
}
